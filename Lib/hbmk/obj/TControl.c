/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "TControl.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( TCONTROL );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( HBOBJECT );
HB_FUNC_EXTERN( AADD );
HB_FUNC_STATIC( TCONTROL_ADDVARS );
HB_FUNC_STATIC( TCONTROL_INIT );
HB_FUNC_STATIC( TCONTROL_COLORS );
HB_FUNC_STATIC( TCONTROL_COORSUPDATE );
HB_FUNC_STATIC( TCONTROL_CREATE );
HB_FUNC_STATIC( TCONTROL_DEFAULT );
HB_FUNC_STATIC( TCONTROL_DELVARS );
HB_FUNC_STATIC( TCONTROL_END );
HB_FUNC_STATIC( TCONTROL_ERASEBKGND );
HB_FUNC_STATIC( TCONTROL_FORWHEN );
HB_FUNC_STATIC( TCONTROL_GETDLGCODE );
HB_FUNC_STATIC( TCONTROL_GETCLIRECT );
HB_FUNC_STATIC( TCONTROL_GETRECT );
HB_FUNC_STATIC( TCONTROL_GOTFOCUS );
HB_FUNC_STATIC( TCONTROL_GONEXTCTRL );
HB_FUNC_STATIC( TCONTROL_GOPREVCTRL );
HB_FUNC_STATIC( TCONTROL_LOSTFOCUS );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_STATIC( TCONTROL_HANDLEEVENT );
HB_FUNC_STATIC( TCONTROL_KEYCHAR );
HB_FUNC_STATIC( TCONTROL_KEYDOWN );
HB_FUNC_STATIC( TCONTROL_KILLFOCUS );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_STATIC( TCONTROL_LBUTTONDOWN );
HB_FUNC_STATIC( TCONTROL_LBUTTONUP );
HB_FUNC_STATIC( TCONTROL_MOUSEMOVE );
HB_FUNC_STATIC( TCONTROL_SUPERKEYDOWN );
HB_FUNC_STATIC( TCONTROL__BEGINPAINT );
HB_FUNC_EXTERN( ENDPAINT );
HB_FUNC_STATIC( TCONTROL_REGISTER );
HB_FUNC_STATIC( TCONTROL___SETFOCUS );
HB_FUNC_STATIC( TCONTROL_RBUTTONUP );
HB_FUNC_EXTERN( SETCAPTURE );
HB_FUNC_EXTERN( GETDC );
HB_FUNC_EXTERN( RELEASEDC );
HB_FUNC_EXTERN( POSTMESSAGE );
HB_FUNC_STATIC( TCONTROL_SETMSG );
HB_FUNC_STATIC( TCONTROL_SETCOLOR );
HB_FUNC_EXTERN( SHOWWINDOW );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_STATIC( TCONTROL_MOVE );
HB_FUNC_STATIC( TCONTROL_RESIZE );
HB_FUNC_STATIC( TCONTROL_COMMAND );
HB_FUNC_STATIC( TCONTROL_NOTIFY );
HB_FUNC_EXTERN( INVALIDATERECT );
HB_FUNC_EXTERN( _GETTEXTHEIGHT );
HB_FUNC_EXTERN( GETWINDOWTEXT );
HB_FUNC_STATIC( TCONTROL_VSCROLL );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( MSGINFO );
HB_FUNC_EXTERN( GETACTIVEMDIHANDLE );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( ADEL );
HB_FUNC_EXTERN( ASIZE );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( BEGINPAINT );
HB_FUNC_EXTERN( GETTEXTCOLOR );
HB_FUNC_EXTERN( GETBKCOLOR );
HB_FUNC_EXTERN( CREATESOLIDBRUSH );
HB_FUNC_EXTERN( GETRED );
HB_FUNC_EXTERN( GETGREEN );
HB_FUNC_EXTERN( GETBLUE );
HB_FUNC_EXTERN( SETTEXTCOLOR );
HB_FUNC_EXTERN( SETBKCOLOR );
HB_FUNC_EXTERN( NOR );
HB_FUNC_EXTERN( GETCLASSINFO );
HB_FUNC_EXTERN( GETINSTANCE );
HB_FUNC_EXTERN( REGISTER_CLASS );
HB_FUNC_EXTERN( _CREATEWINDOWEX );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC_EXTERN( DELETEOBJECT );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC_EXTERN( ISWINDOWHANDLE );
HB_FUNC_EXTERN( ISICONIC );
HB_FUNC_EXTERN( FILLRECT );
HB_FUNC_EXTERN( _GETKEYSTATE );
HB_FUNC_EXTERN( GETFOCUS );
HB_FUNC_EXTERN( SETFOCUS );
HB_FUNC_EXTERN( _GETCLIENTRECT );
HB_FUNC_EXTERN( GETNEXTDLGTABITEM );
HB_FUNC_EXTERN( GETACTIVEWINDOW );
HB_FUNC_EXTERN( SETKEY );
HB_FUNC_EXTERN( PROCNAME );
HB_FUNC_EXTERN( PROCLINE );
HB_FUNC_EXTERN( SETRESCURSOR );
HB_FUNC_EXTERN( LOADCURSOR );
HB_FUNC_EXTERN( MOVEWINDOW );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( _ISWINDOWACTIVE );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( GETITEMBAR );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( HIWORD );
HB_FUNC_EXTERN( LOWORD );
HB_FUNC_EXTERN( DOEVENTS );
HB_FUNC_EXTERN( GETNOTIFYCODE );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TCONTROL )
{ "TCONTROL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "HBOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBOBJECT )}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMULTICLSDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDINLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACONTROLS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ACONTROLS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "_LVALIDATING", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCONTROL_ADDVARS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_ADDVARS )}, NULL },
{ "ADDVIRTUAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AEVALWHEN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCONTROL_INIT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_INIT )}, NULL },
{ "TCONTROL_COLORS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_COLORS )}, NULL },
{ "TCONTROL_COORSUPDATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_COORSUPDATE )}, NULL },
{ "TCONTROL_CREATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_CREATE )}, NULL },
{ "TCONTROL_DEFAULT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_DEFAULT )}, NULL },
{ "TCONTROL_DELVARS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_DELVARS )}, NULL },
{ "TCONTROL_END", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_END )}, NULL },
{ "TCONTROL_ERASEBKGND", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_ERASEBKGND )}, NULL },
{ "TCONTROL_FORWHEN", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_FORWHEN )}, NULL },
{ "TCONTROL_GETDLGCODE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_GETDLGCODE )}, NULL },
{ "TCONTROL_GETCLIRECT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_GETCLIRECT )}, NULL },
{ "TCONTROL_GETRECT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_GETRECT )}, NULL },
{ "NID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCONTROL_GOTFOCUS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_GOTFOCUS )}, NULL },
{ "TCONTROL_GONEXTCTRL", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_GONEXTCTRL )}, NULL },
{ "TCONTROL_GOPREVCTRL", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_GOPREVCTRL )}, NULL },
{ "TCONTROL_LOSTFOCUS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_LOSTFOCUS )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCONTROL_HANDLEEVENT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_HANDLEEVENT )}, NULL },
{ "TCONTROL_KEYCHAR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_KEYCHAR )}, NULL },
{ "TCONTROL_KEYDOWN", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_KEYDOWN )}, NULL },
{ "TCONTROL_KILLFOCUS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_KILLFOCUS )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "BSETGET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCONTROL_LBUTTONDOWN", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_LBUTTONDOWN )}, NULL },
{ "TCONTROL_LBUTTONUP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_LBUTTONUP )}, NULL },
{ "TCONTROL_MOUSEMOVE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_MOUSEMOVE )}, NULL },
{ "TCONTROL_SUPERKEYDOWN", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_SUPERKEYDOWN )}, NULL },
{ "TCONTROL__BEGINPAINT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL__BEGINPAINT )}, NULL },
{ "_NPAINTCOUNT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ENDPAINT", {HB_FS_PUBLIC}, {HB_FUNCNAME( ENDPAINT )}, NULL },
{ "CPS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CPS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HDC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCONTROL_REGISTER", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_REGISTER )}, NULL },
{ "TCONTROL___SETFOCUS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL___SETFOCUS )}, NULL },
{ "TCONTROL_RBUTTONUP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_RBUTTONUP )}, NULL },
{ "SETCAPTURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCAPTURE )}, NULL },
{ "HDC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDC )}, NULL },
{ "NPAINTCOUNT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "RELEASEDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( RELEASEDC )}, NULL },
{ "POSTMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( POSTMESSAGE )}, NULL },
{ "BVALID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCONTROL_SETMSG", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_SETMSG )}, NULL },
{ "BWHEN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCONTROL_SETCOLOR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_SETCOLOR )}, NULL },
{ "LVALID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "POSTMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SHOWWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( SHOWWINDOW )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "TCONTROL_MOVE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_MOVE )}, NULL },
{ "TCONTROL_RESIZE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_RESIZE )}, NULL },
{ "TCONTROL_COMMAND", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_COMMAND )}, NULL },
{ "TCONTROL_NOTIFY", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_NOTIFY )}, NULL },
{ "INVALIDATERECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INVALIDATERECT )}, NULL },
{ "_NCHRHEIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_GETTEXTHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETTEXTHEIGHT )}, NULL },
{ "GETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWTEXT )}, NULL },
{ "TCONTROL_VSCROLL", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCONTROL_VSCROLL )}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LACTIVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LACTIVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LCAPTURED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LCAPTURED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "GETRECT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NTOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NTOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NLEFT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NLEFT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NBOTTOM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NBOTTOM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NRIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NRIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MOVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ENABLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DISABLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LINK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OFONT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETFONT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFONT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MSGINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGINFO )}, NULL },
{ "_TSB_ACONTROLHWND", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_TSB_ACONTROLOBJECTS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_TSB_ACLIENTMDIHWND", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "GETACTIVEMDIHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETACTIVEMDIHANDLE )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "ADEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADEL )}, NULL },
{ "ASIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASIZE )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "BEGINPAINT", {HB_FS_PUBLIC}, {HB_FUNCNAME( BEGINPAINT )}, NULL },
{ "_NCLRTEXT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NCLRTEXT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETTEXTCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEXTCOLOR )}, NULL },
{ "_NCLRPANE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NCLRPANE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETBKCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBKCOLOR )}, NULL },
{ "_HBRUSH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HBRUSH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CREATESOLIDBRUSH", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATESOLIDBRUSH )}, NULL },
{ "GETRED", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETRED )}, NULL },
{ "GETGREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETGREEN )}, NULL },
{ "GETBLUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBLUE )}, NULL },
{ "SETTEXTCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTEXTCOLOR )}, NULL },
{ "SETBKCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETBKCOLOR )}, NULL },
{ "CLASSNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CCAPTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CCAPTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NSTYLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NSTYLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( NOR )}, NULL },
{ "GETCLASSINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCLASSINFO )}, NULL },
{ "GETINSTANCE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETINSTANCE )}, NULL },
{ "_LREGISTERED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "REGISTER_CLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( REGISTER_CLASS )}, NULL },
{ "_CREATEWINDOWEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CREATEWINDOWEX )}, NULL },
{ "HWNDPARENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "ADDVARS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DELETEOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELETEOBJECT )}, NULL },
{ "DELVARS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "CCONTROLNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CPARENTWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ISWINDOWHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISWINDOWHANDLE )}, NULL },
{ "HWNDCHILD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ENDCTRL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ISICONIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISICONIC )}, NULL },
{ "GETCLIRECT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "FILLRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( FILLRECT )}, NULL },
{ "NLASTKEY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_GETKEYSTATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETKEYSTATE )}, NULL },
{ "GOPREVCTRL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GONEXTCTRL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFOCUS )}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETFOCUS )}, NULL },
{ "_NLASTKEY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_GETCLIENTRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCLIENTRECT )}, NULL },
{ "LVALIDATING", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LFOCUSED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGOTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETNEXTDLGTABITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETNEXTDLGTABITEM )}, NULL },
{ "GETACTIVEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETACTIVEWINDOW )}, NULL },
{ "_HCTLFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETKEY )}, NULL },
{ "PROCNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCNAME )}, NULL },
{ "PROCLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCLINE )}, NULL },
{ "BKEYCHAR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BKEYDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LMOUSEDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NLASTROW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NLASTCOL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BLCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BLBUTTONUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BLOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OCURSOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETRESCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETRESCURSOR )}, NULL },
{ "HCURSOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LOADCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOADCURSOR )}, NULL },
{ "LFOCUSED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LKEEPDEFAULTSTATUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BMMOVED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MOVEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MOVEWINDOW )}, NULL },
{ "COORSUPDATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BRBUTTONUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LREGISTERED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETSYSCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSCOLOR )}, NULL },
{ "BRESIZED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NSTATUSITEM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ISWINDOWACTIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWACTIVE )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "GETITEMBAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETITEMBAR )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "LWHEN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HIWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HIWORD )}, NULL },
{ "OVSCROLL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GOUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GODOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PAGEUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PAGEDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "THUMBPOS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LOWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOWORD )}, NULL },
{ "THUMBTRACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "COMMAND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NOTIFY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BEGINPAINT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PAINT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ENDPAINT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DOEVENTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOEVENTS )}, NULL },
{ "DESTROY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DRAWITEM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ERASEBKGND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HSCROLL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "KEYDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "KEYCHAR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETDLGCODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LBUTTONDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LBUTTONUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MOUSEMOVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "RBUTTONDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "RBUTTONUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GOTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VSCROLL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "RESIZE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TIMER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ASYNCSELECT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETNOTIFYCODE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETNOTIFYCODE )}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TCONTROL, "TControl.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TCONTROL
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TCONTROL )
   #include "hbiniseg.h"
#endif

HB_FUNC( TCONTROL )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 240 );
	hb_xvmSetLine( 26 );
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
	hb_xvmPushStringConst( "TControl", 8 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 0 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 28 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bSetGet", 7 );
	hb_xvmPushStringConst( "bChange", 7 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 29 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cCaption", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 30 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nLastRow", 8 );
	hb_xvmPushStringConst( "nLastCol", 8 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 31 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "NUMERIC", 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nAlign", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 32 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nStatusItem", 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 34 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bLClicked", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 35 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bLDblClick", 10 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 36 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bRClicked", 9 );
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
	hb_xvmPushStringConst( "bWhen", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 38 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cMsg", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 40 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bMoved", 6 );
	hb_xvmPushStringConst( "bLButtonUp", 10 );
	hb_xvmPushStringConst( "bKeyDown", 8 );
	hb_xvmPushStringConst( "bPainted", 8 );
	hb_xvmArrayGen( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 41 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bMButtonDown", 12 );
	hb_xvmPushStringConst( "bMButtonUp", 10 );
	hb_xvmPushStringConst( "bRButtonUp", 10 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 42 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bResized", 8 );
	hb_xvmPushStringConst( "bValid", 6 );
	hb_xvmPushStringConst( "bKeyChar", 8 );
	hb_xvmPushStringConst( "bMMoved", 7 );
	hb_xvmArrayGen( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 43 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bGotFocus", 9 );
	hb_xvmPushStringConst( "bLostFocus", 10 );
	hb_xvmPushStringConst( "bDropFiles", 10 );
	hb_xvmPushStringConst( "bDdeInit", 8 );
	hb_xvmPushStringConst( "bDdeExecute", 11 );
	hb_xvmArrayGen( 5 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 45 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lFocused", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 46 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lValidating", 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 47 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lCaptured", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 48 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lUpdate", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 49 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lDesign", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 50 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lVisible", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 51 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lMouseDown", 10 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 52 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lKeepDefaultStatus", 18 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 54 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nTop", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 55 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nLeft", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 56 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nBottom", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 57 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nRight", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 58 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nStyle", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 59 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nId", 3 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 60 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nClrText", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 61 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nClrPane", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 62 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nPaintCount", 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 63 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nLastKey", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 64 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nHelpId", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 65 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nChrHeight", 10 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 67 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "OBJECT", 6 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "oWnd", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 68 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "oCursor", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 69 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hCursor", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 70 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "oFont", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 71 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hFont", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 72 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hBrush", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 73 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hWnd", 4 );
	hb_xvmPushStringConst( "hCtlFocus", 9 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 74 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cControlName", 12 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 75 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cParentWnd", 10 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 76 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hDc", 3 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 77 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cPS", 3 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 78 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "oVScroll", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 79 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "oHScroll", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 81 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hWndParent", 10 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 82 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aControls", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 83 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "oWndlAppendMode", 15 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 85 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "oBrw", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 86 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "oCol", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 87 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nCol", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 91 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "cTitle", 6 );
	hb_xvmPushStringConst( "cVarName", 8 );
	hb_xvmPushStringConst( "nClrText", 8 );
	hb_xvmPushStringConst( "nClrPane", 8 );
	hb_xvmPushStringConst( "nAlign", 6 );
	hb_xvmPushStringConst( "nTop", 4 );
	hb_xvmPushStringConst( "nLeft", 5 );
	hb_xvmPushStringConst( "nWidth", 6 );
	hb_xvmPushStringConst( "nHeight", 7 );
	hb_xvmPushStringConst( "Cargo", 5 );
	hb_xvmArrayGen( 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 32L ) ) break;
	hb_xvmPushStringConst( "aProperties", 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 95 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "AddControl", 10 );
	{
		static const HB_BYTE codeblock[ 49 ] = {
			2, 0, 0, 0, 48, 8, 0, 95, 1, 112, 0, 100, 8, 28, 13, 48, 
			9, 0, 95, 1, 4, 0, 0, 112, 1, 73, 176, 10, 0, 48, 8, 0, 
			95, 1, 112, 0, 95, 2, 20, 2, 48, 11, 0, 95, 1, 9, 112, 1, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 97 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "AddVars", 7 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 99 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Change", 6 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 101 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Click", 5 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 48, 15, 0, 48, 16, 0, 95, 1, 112, 0, 112, 0, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 103 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Init", 4 );
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 105 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Colors", 6 );
	hb_xvmPushSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 107 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CoorsUpdate", 11 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 109 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Create", 6 );
	hb_xvmPushSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 111 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Default", 7 );
	hb_xvmPushSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 113 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "DelVars", 7 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 115 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Display", 7 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 117 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "DrawItem", 8 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 119 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Save", 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 121 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "End", 3 );
	hb_xvmPushSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 123 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "EraseBkGnd", 10 );
	hb_xvmPushSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 125 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "FillMeasure", 11 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 127 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ForWhen", 7 );
	hb_xvmPushSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 129 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetDlgCode", 10 );
	hb_xvmPushSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 131 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetCliRect", 10 );
	hb_xvmPushSymbol( symbols + 27 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 133 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetRect", 7 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 135 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetNewId", 8 );
	{
		static const HB_BYTE codeblock[ 33 ] = {
			1, 0, 0, 0, 48, 29, 0, 95, 1, 112, 0, 100, 8, 28, 12, 48, 
			30, 0, 95, 1, 92, 100, 112, 1, 73, 48, 30, 0, 95, 1, 147, 172, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 137 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GotFocus", 8 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 139 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GoNextCtrl", 10 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 141 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GoPrevCtrl", 10 );
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 143 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LostFocus", 9 );
	hb_xvmPushSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 145 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "nWidth", 6 );
	{
		static const HB_BYTE codeblock[ 19 ] = {
			1, 0, 0, 0, 176, 35, 0, 48, 36, 0, 95, 1, 112, 0, 92, 3, 
			12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 147 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "nHeight", 7 );
	{
		static const HB_BYTE codeblock[ 19 ] = {
			1, 0, 0, 0, 176, 35, 0, 48, 36, 0, 95, 1, 112, 0, 92, 4, 
			12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 149 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "HandleEvent", 11 );
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 151 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "KeyChar", 7 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 153 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "KeyDown", 7 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 155 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "KeyUp", 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 157 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "KillFocus", 9 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 160 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VarPut", 6 );
	{
		static const HB_BYTE codeblock[ 41 ] = {
			2, 0, 0, 0, 176, 41, 0, 48, 42, 0, 95, 1, 112, 0, 12, 1, 
			106, 2, 66, 0, 8, 28, 18, 48, 43, 0, 48, 42, 0, 95, 1, 112, 
			0, 95, 2, 112, 1, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 162 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VarGet", 6 );
	{
		static const HB_BYTE codeblock[ 39 ] = {
			1, 0, 0, 0, 176, 41, 0, 48, 42, 0, 95, 1, 112, 0, 12, 1, 
			106, 2, 66, 0, 8, 28, 16, 48, 43, 0, 48, 42, 0, 95, 1, 112, 
			0, 112, 0, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 164 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LButtonDown", 11 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 166 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LButtonUp", 9 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 168 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "MouseMove", 9 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 170 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Paint", 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 172 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SuperKeyDown", 12 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 174 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "BeginPaint", 10 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 177 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "EndPaint", 8 );
	{
		static const HB_BYTE codeblock[ 48 ] = {
			1, 0, 0, 0, 48, 49, 0, 95, 1, 147, 169, 176, 50, 0, 48, 36, 
			0, 95, 1, 112, 0, 48, 51, 0, 95, 1, 112, 0, 20, 2, 48, 52, 
			0, 95, 1, 100, 112, 1, 73, 48, 53, 0, 95, 1, 100, 112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 179 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Register", 8 );
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 181 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 183 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "RButtonUp", 9 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 185 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Capture", 7 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 176, 57, 0, 48, 36, 0, 95, 1, 112, 0, 12, 1, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 189 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetDC", 5 );
	{
		static const HB_BYTE codeblock[ 72 ] = {
			1, 0, 0, 0, 48, 58, 0, 95, 1, 112, 0, 100, 8, 28, 22, 48, 
			53, 0, 95, 1, 176, 59, 0, 48, 36, 0, 95, 1, 112, 0, 12, 1, 
			112, 1, 73, 48, 60, 0, 95, 1, 112, 0, 100, 8, 28, 13, 48, 49, 
			0, 95, 1, 122, 112, 1, 73, 25, 9, 48, 49, 0, 95, 1, 147, 170, 
			48, 58, 0, 95, 1, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 192 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ReleaseDC", 9 );
	{
		static const HB_BYTE codeblock[ 58 ] = {
			1, 0, 0, 0, 48, 49, 0, 95, 1, 147, 169, 48, 60, 0, 95, 1, 
			112, 0, 121, 8, 28, 36, 176, 61, 0, 48, 36, 0, 95, 1, 112, 0, 
			48, 58, 0, 95, 1, 112, 0, 12, 2, 28, 12, 48, 53, 0, 95, 1, 
			100, 112, 1, 25, 6, 100, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 195 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "PostMsg", 7 );
	{
		static const HB_BYTE codeblock[ 23 ] = {
			4, 0, 0, 0, 176, 62, 0, 48, 36, 0, 95, 1, 112, 0, 95, 2, 
			95, 3, 95, 4, 12, 4, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 197 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "lValid", 6 );
	{
		static const HB_BYTE codeblock[ 33 ] = {
			1, 0, 0, 0, 48, 63, 0, 95, 1, 112, 0, 100, 69, 28, 18, 48, 
			43, 0, 48, 63, 0, 95, 1, 112, 0, 95, 1, 112, 1, 25, 3, 120, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 199 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SetMsg", 6 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 201 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "lWhen", 5 );
	{
		static const HB_BYTE codeblock[ 33 ] = {
			1, 0, 0, 0, 48, 65, 0, 95, 1, 112, 0, 100, 69, 28, 18, 48, 
			43, 0, 48, 65, 0, 95, 1, 112, 0, 95, 1, 112, 1, 25, 3, 120, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 203 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SetColor", 8 );
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 206 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "EndCtrl", 7 );
	{
		static const HB_BYTE codeblock[ 29 ] = {
			2, 0, 0, 0, 48, 67, 0, 95, 1, 112, 0, 165, 80, 2, 28, 12, 
			48, 68, 0, 95, 1, 92, 16, 112, 1, 73, 95, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 208 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Hide", 4 );
	{
		static const HB_BYTE codeblock[ 18 ] = {
			1, 0, 0, 0, 176, 69, 0, 48, 36, 0, 95, 1, 112, 0, 121, 12, 
			2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 210 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Show", 4 );
	{
		static const HB_BYTE codeblock[ 19 ] = {
			1, 0, 0, 0, 176, 69, 0, 48, 36, 0, 95, 1, 112, 0, 92, 8, 
			12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 212 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SendMsg", 7 );
	{
		static const HB_BYTE codeblock[ 23 ] = {
			4, 0, 0, 0, 176, 70, 0, 48, 36, 0, 95, 1, 112, 0, 95, 2, 
			95, 3, 95, 4, 12, 4, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 214 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Move", 4 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 216 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ReSize", 6 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 218 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Command", 7 );
	hb_xvmPushSymbol( symbols + 73 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 220 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Notify", 6 );
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 223 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Refresh", 7 );
	{
		static const HB_BYTE codeblock[ 31 ] = {
			2, 0, 0, 0, 176, 75, 0, 48, 36, 0, 95, 1, 112, 0, 95, 2, 
			100, 8, 31, 6, 95, 2, 31, 5, 121, 25, 3, 122, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 226 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "nGetChrHeight", 13 );
	{
		static const HB_BYTE codeblock[ 51 ] = {
			1, 0, 0, 0, 48, 53, 0, 95, 1, 176, 59, 0, 48, 36, 0, 95, 
			1, 112, 0, 12, 1, 112, 1, 73, 48, 76, 0, 95, 1, 176, 77, 0, 
			48, 36, 0, 95, 1, 112, 0, 48, 58, 0, 95, 1, 112, 0, 12, 2, 
			112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 228 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetText", 7 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 176, 78, 0, 48, 36, 0, 95, 1, 112, 0, 12, 1, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 230 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VScroll", 7 );
	hb_xvmPushSymbol( symbols + 79 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 232 );
	hb_xvmPushSymbol( symbols + 80 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushStaticByRef( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 82 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 84 );
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
	hb_xvmPushSymbol( symbols + 82 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_INIT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 240 );
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00002;
lab00001: ;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00002: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00004;
lab00003: ;
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00004: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 242 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 243 );
	hb_xvmPushSymbol( symbols + 91 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 245 );
	hb_xvmPushSymbol( symbols + 92 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00006: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 246 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00008;
lab00007: ;
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00008: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 247 );
	hb_xvmPushSymbol( symbols + 96 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00010;
lab00009: ;
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00010: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 248 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00012;
lab00011: ;
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00012: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 250 );
	hb_xvmPushSymbol( symbols + 100 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmSend( 4 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 252 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00014;
lab00013: ;
	hb_xvmPushSymbol( symbols + 102 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00014: ;
	hb_xvmSetLine( 254 );
	hb_xvmPushSymbol( symbols + 103 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 256 );
	hb_xvmPushSymbol( symbols + 104 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 257 );
	hb_xvmPushSymbol( symbols + 105 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 104 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00017;
lab00015: ;
	hb_xvmSetLine( 259 );
	hb_xvmPushSymbol( symbols + 106 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00017;
lab00016: ;
	hb_xvmSetLine( 263 );
	hb_xvmPushFuncSymbol( symbols + 107 );
	hb_xvmPushStringConst( "No Valid Control ID", 19 );
	hb_xvmPushStringConst( "Error", 5 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00017: ;
	hb_xvmSetLine( 266 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_ADDVARS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 108 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 273 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 109 ) ) break;
	hb_xvmPushSelf();
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 274 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 110 ) ) break;
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 112 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 0 );
lab00002: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 276 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_DELVARS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 280 );
	hb_xvmPushSelf();
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 283 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	if( hb_xvmPushMemvar( symbols + 108 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 114 );
	if( hb_xvmPushMemvar( symbols + 108 ) ) break;
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 1, 0, 2, 0, 95, 1, 48, 36, 0, 95, 255, 112, 0, 8, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 0 );
lab00002: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 287 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 288 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	if( hb_xvmPushMemvar( symbols + 108 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	if( hb_xvmPushMemvar( symbols + 108 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 117 );
	if( hb_xvmPushMemvar( symbols + 108 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	if( hb_xvmPushMemvar( symbols + 109 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	if( hb_xvmPushMemvar( symbols + 109 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 117 );
	if( hb_xvmPushMemvar( symbols + 109 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 292 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	if( hb_xvmPushMemvar( symbols + 110 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 293 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	if( hb_xvmPushMemvar( symbols + 110 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 117 );
	if( hb_xvmPushMemvar( symbols + 110 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 296 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL__BEGINPAINT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 304 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 305 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 307 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	if( hb_xvmIncEqPop() ) break;
lab00002: ;
	hb_xvmSetLine( 310 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 311 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 313 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_COLORS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 321 );
	hb_xvmPushSymbol( symbols + 119 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 120 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 121 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushSymbol( symbols + 120 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00002: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 122 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00004: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 125 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 127 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 130 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00006: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 323 );
	hb_xvmPushFuncSymbol( symbols + 131 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 120 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 324 );
	hb_xvmPushFuncSymbol( symbols + 132 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 326 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_COORSUPDATE )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 332 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 4 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 334 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 342 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_CREATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 348 );
	hb_xvmLocalSetInt( 2, 0L );
	hb_xvmSetLine( 350 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 133 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 351 );
	hb_xvmPushSymbol( symbols + 134 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 135 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushStringConst( "", 0 );
	goto lab00004;
lab00003: ;
	hb_xvmPushSymbol( symbols + 135 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00004: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 352 );
	hb_xvmPushSymbol( symbols + 136 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 137 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 13565952 );
#else
	hb_xvmPushLong( 13565952L );
#endif
	goto lab00006;
lab00005: ;
	hb_xvmPushSymbol( symbols + 137 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00006: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 353 );
	hb_xvmPushSymbol( symbols + 92 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushInteger( 0 );
	goto lab00008;
lab00007: ;
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00008: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 354 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushInteger( 0 );
	goto lab00010;
lab00009: ;
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00010: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 355 );
	hb_xvmPushSymbol( symbols + 96 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushInteger( 10 );
	goto lab00012;
lab00011: ;
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00012: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 356 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushInteger( 10 );
	goto lab00014;
lab00013: ;
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00014: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 357 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushInteger( 0 );
	goto lab00016;
lab00015: ;
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00016: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 359 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 360 );
	hb_xvmPushSymbol( symbols + 136 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 138 );
	hb_xvmPushSymbol( symbols + 137 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1073741824 );
#else
	hb_xvmPushLong( 1073741824L );
#endif
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00017: ;
	hb_xvmSetLine( 363 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 364 );
	hb_xvmPushSymbol( symbols + 125 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 127 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 130 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00018: ;
	hb_xvmSetLine( 367 );
	hb_xvmPushFuncSymbol( symbols + 139 );
	hb_xvmPushFuncSymbol( symbols + 140 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 368 );
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00019;
	hb_xvmSetLine( 369 );
	hb_xvmPushSymbol( symbols + 141 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 142 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00021;
lab00019: ;
	hb_xvmSetLine( 371 );
	hb_xvmPushSymbol( symbols + 141 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 142 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00021;
lab00020: ;
	hb_xvmSetLine( 374 );
	hb_xvmPushSymbol( symbols + 141 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00021: ;
	hb_xvmSetLine( 377 );
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 32768L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 381 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 143 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 135 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 137 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPushSymbol( symbols + 144 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 140 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 12 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00023;
lab00022: ;
	hb_xvmSetLine( 387 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 143 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 135 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 137 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 144 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 140 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 12 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00023: ;
	hb_xvmSetLine( 390 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00024;
	hb_xvmSetLine( 391 );
	hb_xvmPushFuncSymbol( symbols + 145 );
	hb_xvmPushStringConst( "Window Create Error!", 20 );
	hb_xvmPushStringConst( "Alert", 5 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00025;
lab00024: ;
	hb_xvmSetLine( 393 );
	hb_xvmPushSymbol( symbols + 146 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00025: ;
	hb_xvmSetLine( 396 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_DEFAULT )
{
   do {
	hb_xvmSetLine( 402 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 404 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_END )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 0 );
	hb_xvmSetLine( 408 );
	hb_xvmPushSelf();
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 412 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 1, 0, 1, 0, 95, 1, 48, 36, 0, 95, 255, 112, 0, 8, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 0 );
lab00002: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 414 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 415 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 416 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 419 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 420 );
	hb_xvmPushFuncSymbol( symbols + 147 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 423 );
	hb_xvmPushSymbol( symbols + 148 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 425 );
	hb_xvmPushStringConst( "TGETBOX", 7 );
	hb_xvmPushFuncSymbol( symbols + 149 );
	hb_xvmPushSymbol( symbols + 133 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 426 );
	hb_xvmPushFuncSymbol( symbols + 150 );
	hb_xvmPushSymbol( symbols + 151 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 152 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 427 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 428 );
	hb_xvmPushFuncSymbol( symbols + 153 );
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 70 );
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 274 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 61536 );
#else
	hb_xvmPushLong( 61536L );
#endif
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00005: ;
	hb_xvmSetLine( 429 );
	hb_xvmPushFuncSymbol( symbols + 147 );
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 430 );
	hb_xvmPushFuncSymbol( symbols + 147 );
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 431 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
lab00006: ;
	hb_xvmSetLine( 434 );
	hb_xvmPushStringConst( "TBTNBOX", 7 );
	hb_xvmPushFuncSymbol( symbols + 149 );
	hb_xvmPushSymbol( symbols + 133 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 435 );
	hb_xvmPushSymbol( symbols + 154 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 436 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushSymbol( symbols + 154 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 16 );
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 438 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 16 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 439 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 442 );
	hb_xvmPushSymbol( symbols + 155 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_ERASEBKGND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 450 );
	hb_xvmPushFuncSymbol( symbols + 156 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 451 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 452 );
	hb_xvmPushSymbol( symbols + 157 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 453 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 454 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 456 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 459 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 460 );
	hb_xvmPushSymbol( symbols + 157 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 461 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 462 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 465 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_FORWHEN )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 471 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 473 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 477 );
	hb_xvmPushSymbol( symbols + 159 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 38L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 159 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 40L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 159 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 159 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
lab00001: ;
	hb_xvmSetLine( 478 );
	hb_xvmPushFuncSymbol( symbols + 160 );
	hb_xvmPushInteger( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 479 );
	hb_xvmPushSymbol( symbols + 161 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 481 );
	hb_xvmPushSymbol( symbols + 162 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 484 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushFuncSymbol( symbols + 163 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 485 );
	hb_xvmPushFuncSymbol( symbols + 164 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 489 );
	hb_xvmPushSymbol( symbols + 165 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 491 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_GETCLIRECT )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 497 );
	hb_xvmPushFuncSymbol( symbols + 166 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 499 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_GETDLGCODE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 505 );
	hb_xvmPushSymbol( symbols + 167 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 506 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 507 );
	hb_xvmPushSymbol( symbols + 165 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 515 );
	hb_xvmRetInt( 4L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_GETRECT )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 521 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 4 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 523 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 525 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_GOTFOCUS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 531 );
	hb_xvmPushSymbol( symbols + 168 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 532 );
	hb_xvmPushSymbol( symbols + 169 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 170 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 534 );
	hb_xvmPushSymbol( symbols + 171 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 535 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSymbol( symbols + 171 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 538 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_GONEXTCTRL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 546 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushFuncSymbol( symbols + 173 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 163 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 548 );
	hb_xvmPushSymbol( symbols + 174 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 550 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 551 );
	hb_xvmPushFuncSymbol( symbols + 164 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 554 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_GOPREVCTRL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 562 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushFuncSymbol( symbols + 173 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 163 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 564 );
	hb_xvmPushSymbol( symbols + 174 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 566 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 567 );
	hb_xvmPushFuncSymbol( symbols + 164 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 570 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_KEYCHAR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 576 );
	hb_xvmPushFuncSymbol( symbols + 175 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 579 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 160 );
	hb_xvmPushInteger( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 580 );
	hb_xvmPushSymbol( symbols + 161 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 581 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 583 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 584 );
	hb_xvmPushSymbol( symbols + 162 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 585 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 588 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 589 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 176 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 177 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 592 );
	hb_xvmPushSymbol( symbols + 178 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 593 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSymbol( symbols + 178 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 596 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_KEYDOWN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 602 );
	hb_xvmPushFuncSymbol( symbols + 175 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 604 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 605 );
	hb_xvmPushSymbol( symbols + 162 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 606 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 609 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 610 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 176 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 177 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 611 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 614 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 112L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 616 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 619 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 620 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 623 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_KILLFOCUS )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 631 );
	hb_xvmPushSymbol( symbols + 180 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_LBUTTONDOWN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 637 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 638 );
	hb_xvmPushSymbol( symbols + 182 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 639 );
	hb_xvmPushSymbol( symbols + 183 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 641 );
	hb_xvmPushSymbol( symbols + 184 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 642 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSymbol( symbols + 184 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 4 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 645 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_LBUTTONUP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 651 );
	hb_xvmPushSymbol( symbols + 185 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 652 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSymbol( symbols + 185 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 4 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 655 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_LOSTFOCUS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 661 );
	hb_xvmPushSymbol( symbols + 168 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 662 );
	hb_xvmPushSymbol( symbols + 169 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 663 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushSymbol( symbols + 186 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 664 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSymbol( symbols + 186 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 667 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_MOUSEMOVE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 673 );
	hb_xvmPushSymbol( symbols + 187 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 674 );
	hb_xvmPushFuncSymbol( symbols + 188 );
	hb_xvmPushSymbol( symbols + 189 );
	hb_xvmPushSymbol( symbols + 187 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 676 );
	hb_xvmPushFuncSymbol( symbols + 188 );
	hb_xvmPushFuncSymbol( symbols + 190 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 32512 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 679 );
	hb_xvmPushSymbol( symbols + 191 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 680 );
	hb_xvmPushSymbol( symbols + 169 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 170 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 192 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 683 );
	hb_xvmPushSymbol( symbols + 193 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 684 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSymbol( symbols + 193 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 4 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 687 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_MOVE )
{
   do {
	hb_xvmFrame( 0, 5 );
	hb_xvmSetLine( 693 );
	hb_xvmPushFuncSymbol( symbols + 194 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 695 );
	hb_xvmPushSymbol( symbols + 195 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 697 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_RBUTTONUP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 703 );
	hb_xvmPushSymbol( symbols + 196 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 704 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSymbol( symbols + 196 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 4 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 707 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_REGISTER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 715 );
	hb_xvmPushSymbol( symbols + 141 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 197 );
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
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00002: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 717 );
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 718 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 721 );
	hb_xvmPushFuncSymbol( symbols + 140 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 723 );
	hb_xvmPushSymbol( symbols + 151 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 725 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushFuncSymbol( symbols + 138 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmPushLocal( 1 );
lab00005: ;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 726 );
	hb_xvmPushSymbol( symbols + 122 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushFuncSymbol( symbols + 198 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00007;
lab00006: ;
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00007: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 727 );
	hb_xvmPushSymbol( symbols + 125 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushFuncSymbol( symbols + 127 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 130 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00009: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 729 );
	hb_xvmPushFuncSymbol( symbols + 138 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 16384 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 731 );
	hb_xvmPushFuncSymbol( symbols + 139 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 732 );
	hb_xvmPushSymbol( symbols + 141 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 142 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 7 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 734 );
	hb_xvmPushSymbol( symbols + 141 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00011: ;
	hb_xvmSetLine( 737 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_RESIZE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 743 );
	hb_xvmPushSymbol( symbols + 195 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 744 );
	hb_xvmPushSymbol( symbols + 199 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 745 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSymbol( symbols + 199 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 4 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 748 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_SETMSG )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 756 );
	hb_xvmPushSymbol( symbols + 200 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 757 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 760 );
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
	hb_xvmSetLine( 761 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushStringConst( "", 0 );
	goto lab00005;
lab00004: ;
	hb_xvmPushLocal( 1 );
lab00005: ;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 763 );
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmPushSymbol( symbols + 152 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	goto lab00007;
lab00006: ;
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 14L ) ) break;
lab00007: ;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 765 );
	hb_xvmPushFuncSymbol( symbols + 201 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 766 );
	hb_xvmPushFuncSymbol( symbols + 202 );
	hb_xvmPushStringConst( "StatusBar", 9 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 767 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 768 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	hb_xvmPushSymbol( symbols + 200 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 769 );
	hb_xvmPushFuncSymbol( symbols + 204 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 204 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 770 );
	hb_xvmPushFuncSymbol( symbols + 205 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "StatusBar", 9 );
	hb_xvmPushStringConst( "Item", 4 );
	hb_xvmPushSymbol( symbols + 200 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 5 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 772 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 273L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 773 );
	hb_xvmPushFuncSymbol( symbols + 205 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "StatusBar", 9 );
	hb_xvmPushStringConst( "Item", 4 );
	hb_xvmPushSymbol( symbols + 200 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 111 ) ) break;
	if( hb_xvmArrayItemPush( 273L ) ) break;
	if( hb_xvmDo( 5 ) ) break;
lab00009: ;
	hb_xvmSetLine( 778 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_SETCOLOR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 784 );
	hb_xvmPushSymbol( symbols + 119 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 785 );
	hb_xvmPushSymbol( symbols + 122 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 787 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 788 );
	hb_xvmPushFuncSymbol( symbols + 147 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 791 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 792 );
	hb_xvmPushSymbol( symbols + 125 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 794 );
	hb_xvmPushSymbol( symbols + 125 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 127 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 130 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 797 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_SUPERKEYDOWN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 803 );
	hb_xvmPushFuncSymbol( symbols + 175 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 805 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 806 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 176 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 177 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 807 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 810 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 112L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 812 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 815 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 816 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 819 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL___SETFOCUS )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 823 );
	hb_xvmPushSymbol( symbols + 206 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 824 );
	hb_xvmPushFuncSymbol( symbols + 164 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 825 );
	hb_xvmPushSymbol( symbols + 174 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 828 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_VSCROLL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 832 );
	hb_xvmPushFuncSymbol( symbols + 207 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 834 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 835 );
	hb_xvmPushSymbol( symbols + 208 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 837 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 209 );
	hb_xvmPushSymbol( symbols + 208 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00013;
lab00001: ;
	hb_xvmSetLine( 838 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushSymbol( symbols + 210 );
	hb_xvmPushSymbol( symbols + 208 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00013;
lab00002: ;
	hb_xvmSetLine( 839 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 211 );
	hb_xvmPushSymbol( symbols + 208 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00013;
lab00003: ;
	hb_xvmSetLine( 840 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushSymbol( symbols + 212 );
	hb_xvmPushSymbol( symbols + 208 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00013;
lab00004: ;
	hb_xvmSetLine( 841 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushSymbol( symbols + 213 );
	hb_xvmPushSymbol( symbols + 208 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00013;
lab00005: ;
	hb_xvmSetLine( 842 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmPushSymbol( symbols + 215 );
	hb_xvmPushSymbol( symbols + 208 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00013;
lab00006: ;
	hb_xvmSetLine( 843 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 8L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00007: ;
	hb_xvmSetLine( 848 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 2049 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00013;
lab00008: ;
	hb_xvmSetLine( 849 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 2050 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00013;
lab00009: ;
	hb_xvmSetLine( 850 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 2051 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00013;
lab00010: ;
	hb_xvmSetLine( 851 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 2052 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00013;
lab00011: ;
	hb_xvmSetLine( 852 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 2058 );
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	goto lab00013;
lab00012: ;
	hb_xvmSetLine( 853 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 2062 );
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00013: ;
	hb_xvmSetLine( 857 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_HANDLEEVENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 863 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 16L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 864 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 866 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 273L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 867 );
	hb_xvmPushSymbol( symbols + 216 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 869 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 78L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 870 );
	hb_xvmPushSymbol( symbols + 217 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 872 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 15L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 873 );
	hb_xvmPushSymbol( symbols + 218 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 874 );
	hb_xvmPushSymbol( symbols + 219 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 875 );
	hb_xvmPushSymbol( symbols + 220 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 876 );
	hb_xvmPushFuncSymbol( symbols + 221 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00022;
lab00004: ;
	hb_xvmSetLine( 878 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 879 );
	hb_xvmPushSymbol( symbols + 222 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 881 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 43L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 882 );
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 884 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 20L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 885 );
	hb_xvmPushSymbol( symbols + 224 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00007: ;
	hb_xvmSetLine( 887 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 276L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 888 );
	hb_xvmPushSymbol( symbols + 225 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 890 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 256L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 891 );
	hb_xvmPushSymbol( symbols + 226 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmSetLine( 893 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 258L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 894 );
	hb_xvmPushSymbol( symbols + 227 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00010: ;
	hb_xvmSetLine( 896 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 135L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 897 );
	hb_xvmPushSymbol( symbols + 228 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00011: ;
	hb_xvmSetLine( 899 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 8L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 900 );
	hb_xvmPushSymbol( symbols + 180 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00012: ;
	hb_xvmSetLine( 902 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 513L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 904 );
	hb_xvmPushSymbol( symbols + 229 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 207 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00013: ;
	hb_xvmSetLine( 905 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 514L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmSetLine( 907 );
	hb_xvmPushSymbol( symbols + 230 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 207 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00014: ;
	hb_xvmSetLine( 908 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 512L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
	hb_xvmSetLine( 910 );
	hb_xvmPushSymbol( symbols + 231 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 207 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00015: ;
	hb_xvmSetLine( 912 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 516L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 914 );
	hb_xvmPushSymbol( symbols + 232 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 207 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00016: ;
	hb_xvmSetLine( 915 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 517L, &fValue ) ) break;
	if( !fValue )
		goto lab00017;
	hb_xvmSetLine( 917 );
	hb_xvmPushSymbol( symbols + 233 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 207 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00017: ;
	hb_xvmSetLine( 918 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 7L, &fValue ) ) break;
	if( !fValue )
		goto lab00018;
	hb_xvmSetLine( 919 );
	hb_xvmPushSymbol( symbols + 234 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00018: ;
	hb_xvmSetLine( 921 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 277L, &fValue ) ) break;
	if( !fValue )
		goto lab00019;
	hb_xvmSetLine( 922 );
	hb_xvmPushSymbol( symbols + 235 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00019: ;
	hb_xvmSetLine( 924 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00020;
	hb_xvmSetLine( 925 );
	hb_xvmPushSymbol( symbols + 236 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 207 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00020: ;
	hb_xvmSetLine( 927 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 275L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 928 );
	hb_xvmPushSymbol( symbols + 237 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00021: ;
	hb_xvmSetLine( 930 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 2065L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 931 );
	hb_xvmPushSymbol( symbols + 238 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00022: ;
	hb_xvmSetLine( 935 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_COMMAND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 942 );
	hb_xvmPushFuncSymbol( symbols + 207 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 944 );
	hb_xvmCopyLocals( 2, 4 );
	hb_xvmSetLine( 947 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 950 );
	hb_xvmPushFuncSymbol( symbols + 207 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 951 );
	hb_xvmPushSymbol( symbols + 226 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 13 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 954 );
	hb_xvmPushFuncSymbol( symbols + 207 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 955 );
	hb_xvmPushSymbol( symbols + 226 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 27 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	goto lab00005;
lab00002: ;
	hb_xvmSetLine( 958 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 961 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 180 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00005;
lab00003: ;
	hb_xvmSetLine( 962 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( -8L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushSymbol( symbols + 180 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 963 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 512L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushSymbol( symbols + 180 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 969 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCONTROL_NOTIFY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 978 );
	hb_xvmPushFuncSymbol( symbols + 239 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( -8L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 979 );
	hb_xvmPushSymbol( symbols + 180 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 982 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 240, 1 );
	/* *** END PROC *** */
   } while( 0 );
}

