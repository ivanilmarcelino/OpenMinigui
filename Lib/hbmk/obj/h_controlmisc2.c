/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_controlmisc2.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( SBROWSE );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( ODLU4FONT );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_EXTERN( ALIAS );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( GETSYSTEMMETRICS );
HB_FUNC_EXTERN( OHMGDATA );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( __BREAKBLOCK );
HB_FUNC_EXTERN( GETACTIVEWINDOW );
HB_FUNC_EXTERN( _DEFINEMODALWINDOW );
HB_FUNC_EXTERN( _DEFINEWINDOW );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( _GETCLIENTRECT );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( _DEFINETBROWSE );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( _SETTHISFORMINFO );
HB_FUNC( SBROWSE_RECORD );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( GETTEXTWIDTH );
HB_FUNC_EXTERN( _ENDTBROWSE );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC_EXTERN( _DEFINEBUTTON );
HB_FUNC_EXTERN( DOMETHOD );
HB_FUNC_EXTERN( _DEFINEHOTKEY );
HB_FUNC_EXTERN( HB_ISOBJECT );
HB_FUNC_EXTERN( _ENDWINDOW );
HB_FUNC_EXTERN( _ACTIVATEWINDOW );
HB_FUNC_EXTERN( DBCLOSEAREA );
HB_FUNC_EXTERN( HB_OSNEWLINE );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( AADD );
HB_FUNC( _TBROWSE );
HB_FUNC_STATIC( _TBROWSE_CREATE );
HB_FUNC_EXTERN( HB_ISCHAR );
HB_FUNC_EXTERN( ASIZE );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( _DEFINEFONT );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( DELETED );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( HMG_RGB2N );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( MAX );
HB_FUNC_EXTERN( GETFONTHEIGHT );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( DOEVENTS );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_CONTROLMISC2 )
{ "SBROWSE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SBROWSE )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "ODLU4FONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU4FONT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "GAPSWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GAPSHEIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "ALIAS", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALIAS )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "GETSYSTEMMETRICS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSTEMMETRICS )}, NULL },
{ "OHMGDATA", {HB_FS_PUBLIC}, {HB_FUNCNAME( OHMGDATA )}, NULL },
{ "CLASSNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_USELECTOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "USELECTOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CBRW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CBRW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "SELECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SELECT )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "__BREAKBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __BREAKBLOCK )}, NULL },
{ "GETACTIVEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETACTIVEWINDOW )}, NULL },
{ "_DEFINEMODALWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEMODALWINDOW )}, NULL },
{ "_DEFINEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEWINDOW )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "_GETCLIENTRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCLIENTRECT )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "H1", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "BWINDOW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_DEFINETBROWSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINETBROWSE )}, NULL },
{ "_NCOLORDER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LNOGRAYBAR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LNOLITEBAR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LNORESETPOS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NSTATUSITEM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LNOKEYCHAR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NWHEELLINES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NCELLMARGINLR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NLINESTYLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NCLRLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETSYSCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSCOLOR )}, NULL },
{ "_LCHECKBOXALLRETURN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CARGO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LEDITABLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LCELLBRW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LUPDATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BRCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_SETTHISFORMINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETTHISFORMINFO )}, NULL },
{ "CPARENTWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SBROWSE_RECORD", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SBROWSE_RECORD )}, NULL },
{ "_LRECLOCKAREA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "ACOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LEDIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CFIELDTYP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NCLRHEADBACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETCOLUMN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LNOHSCROLL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LFOOTING", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LDRAWFOOTERS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NHEIGHTFOOT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NHEIGHTHEAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "INSCOLNUMBER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LISARR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CFOOTING", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "NLEN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LNOHILITE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NFREEZE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NCOLUMN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LLOCKFREEZE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NCELL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NFREEZE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HFONTHEAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HFONT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETTEXTWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEXTWIDTH )}, NULL },
{ "CHEADING", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ENDTBROWSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDTBROWSE )}, NULL },
{ "_OBRW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "_NCLRBACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NCLRHEADBACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NCLRFOCUBACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NCLRPANE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_DEFINEBUTTON", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEBUTTON )}, NULL },
{ "AMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "REPORT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GOTOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "W1", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EXCELOLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ISEDIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DOMETHOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOMETHOD )}, NULL },
{ "_DEFINEHOTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEHOTKEY )}, NULL },
{ "_CNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LPICKERMODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CPICTURE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NALIGN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__ENUMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISOBJECT )}, NULL },
{ "CARGO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LISDBF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "W", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETVALUE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NEDITWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BPREVEDIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NEDITMOVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NEDITMOVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "POSTEDIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BPOSTEDIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETNOHOLES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GORIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ENDWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDWINDOW )}, NULL },
{ "_ACTIVATEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ACTIVATEWINDOW )}, NULL },
{ "DBCLOSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEAREA )}, NULL },
{ "HB_OSNEWLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_OSNEWLINE )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "_TBROWSE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _TBROWSE )}, NULL },
{ "_TBROWSE_CREATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _TBROWSE_CREATE )}, NULL },
{ "_NBRW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CFORM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CFORM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CFORMNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LROWPOSATREC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LROWPOSATREC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LCLRSELECTORHDBACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LCLRSELECTORHDBACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "UALIAS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NROW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NCOL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NHEIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LSPECHD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LSPECHEADER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LDRAWSPECHD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LSUPERHD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LSUPERHEAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LZEBRA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LZEBRALINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LZEBRAROW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LCHESS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LCHESSLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LCHESSROW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BDBLCLICK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BDBLCLICK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BONDBLCLICK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BGOTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGOTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BONGOTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BLOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BLOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BONLOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BCHANGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BCHANGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BONCHANGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LNOPICTURE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LNOPICTURE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_OTODBC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_AHEAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "FIELDS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OTODBC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AHEAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "FIELDNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ARECORDSET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISCHAR )}, NULL },
{ "AFONT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ASIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASIZE )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "_DEFINEFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEFONT )}, NULL },
{ "_AFONT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "BINIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BEND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ABRUSH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACOLOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AHEADER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AFIELD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AFOOT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AFOOTER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NCELL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CALIAS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DELETED", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELETED )}, NULL },
{ "AZEBRA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AZEBRACOLOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "HMG_RGB2N", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_RGB2N )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "NAT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACHESS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACHESSCOLOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACOLORADD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CSPECHDCHAR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LVISIBLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CSPCHEADING", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BSPECHDENUM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BSPECHDENUM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MAX", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAX )}, NULL },
{ "ADJCOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BADJCOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BADJCOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LADJUST", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LNOHSCROLL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NROWCOUNT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "RESETVSCROLL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ASIZE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NVALUE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CTOOLTIP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AHEADCLICK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AALIGN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ABCOLOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AFCOLOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "APICT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LNOCHANGEORD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ANAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ANUMBER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AEDIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_OPARAM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LENUM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LDRAWSPECHD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NHEIGHTSPECHD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NHEIGHTSPECHD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFONTHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHEIGHT )}, NULL },
{ "_NHEIGHTHEAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NHEIGHTCELL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NHEIGHTCELL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NHEIGHTFOOT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETAPPENDMODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETDELETEMODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BBODY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LDRAWSUPERHD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CSUPERHD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CSUPERHD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NHEIGHTSUPER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NHEIGHTSUPER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NHEIGHTSUPERHD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDSUPERHEAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NCOLCOUNT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ASUPERHDCOLOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "USUPERHDBMP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BLDBLCLICK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BLDBLCLICK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "POSTMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BRCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BLCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BLCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BKEYDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BKEYDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NFIREKEY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NFIREKEY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AUSERKEYS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "USERKEYS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NROWPOS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETALLCOLSWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LMOREFIELDS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OHSCROLL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETRANGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "ASUPERHEAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BAFTER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AEVENTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EVENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DOEVENTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOEVENTS )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_CONTROLMISC2, "h_controlmisc2.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_CONTROLMISC2
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_CONTROLMISC2 )
   #include "hbiniseg.h"
#endif

HB_FUNC( SBROWSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 29, 10 );
	hb_xvmSetLine( 16 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 17 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 18 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 19 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 21 );
	hb_xvmPushStringConst( "M", 1 );
	hb_xvmPushStringConst( "C", 1 );
	hb_xvmPushStringConst( "S", 1 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 24 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 25 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 26 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 5 );
lab00001: ;
	hb_xvmSetLine( 29 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 30 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 31 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 6 );
lab00002: ;
	hb_xvmSetLine( 34 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 35 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 36 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 2 );
lab00003: ;
	hb_xvmSetLine( 39 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 40 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmPushNil();
lab00005: ;
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 41 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 42 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 3 );
lab00006: ;
	hb_xvmSetLine( 45 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 46 );
	hb_xvmCopyLocals( 3, 23 );
	hb_xvmSetLine( 47 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
lab00007: ;
	hb_xvmSetLine( 58 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmPushLocal( 1 );
lab00009: ;
	hb_xvmPopLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushLocal( 1 );
	goto lab00012;
lab00010: ;
	hb_xvmPushStringConst( "SBrowse", 7 );
	goto lab00012;
lab00011: ;
	hb_xvmPushLocal( 2 );
lab00012: ;
	hb_xvmPopLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	{
		static const HB_BYTE codeblock[ 2 ] = {
			9, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	goto lab00014;
lab00013: ;
	hb_xvmPushLocal( 3 );
lab00014: ;
	hb_xvmPopLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmArrayGen( 0 );
	goto lab00016;
lab00015: ;
	hb_xvmPushLocal( 4 );
lab00016: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00\xE8\?", 10, 2 );
	if( hb_xvmMult() ) break;
	goto lab00018;
lab00017: ;
	hb_xvmPushLocal( 5 );
lab00018: ;
	hb_xvmPopLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	goto lab00020;
lab00019: ;
	hb_xvmPushLocal( 6 );
lab00020: ;
	hb_xvmPopLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00022;
lab00021: ;
	hb_xvmPushLocal( 7 );
lab00022: ;
	hb_xvmPopLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00024;
lab00023: ;
	hb_xvmPushLocal( 8 );
lab00024: ;
	hb_xvmPopLocal( 8 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00026;
lab00025: ;
	hb_xvmPushLocal( 10 );
lab00026: ;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 60 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00028;
lab00027: ;
	hb_xvmPushLocal( 27 );
lab00028: ;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 62 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TSBROWSE", 8 );
	if( hb_xvmNotEqual() ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 36 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmSetLine( 63 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmPushInteger( 20 );
	goto lab00030;
lab00029: ;
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
lab00030: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmPushStringConst( "oBrw", 4 );
	goto lab00032;
lab00031: ;
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
lab00032: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00033: ;
	hb_xvmSetLine( 66 );
	goto lab00044;
lab00034: ;
	hb_xvmSetLine( 68 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmPushInteger( 1 );
	goto lab00036;
lab00035: ;
	hb_xvmPushInteger( 2 );
lab00036: ;
	hb_xvmPopLocal( 34 );
	goto lab00045;
lab00037: ;
	hb_xvmSetLine( 71 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00039;
lab00038: ;
	hb_xvmPushLocal( 8 );
lab00039: ;
	hb_xvmPopLocal( 34 );
	goto lab00045;
lab00040: ;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 76 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00041;
	hb_xvmPushInteger( 2 );
	goto lab00042;
lab00041: ;
	hb_xvmPushLocal( 34 );
lab00042: ;
	hb_xvmPopLocal( 34 );
	goto lab00045;
lab00043: ;
	hb_xvmSetLine( 79 );
	hb_xvmLocalSetInt( 34, 2L );
	goto lab00045;
lab00044: ;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "L", 1 ) )
		{
			hb_stackPop();
			goto lab00034;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00037;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "C", 1 ) )
		{
			hb_stackPop();
			goto lab00040;
		}
		{
			hb_stackPop();
			goto lab00043;
		}
	}
lab00045: ;
	hb_xvmSetLine( 81 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 35 );
	hb_xvmSetLine( 82 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushStringConst( "M", 1 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00048;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00048;
	hb_xvmSetLine( 85 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 86 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushStringConst( "SqlTable", 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "SELECT * FROM ", 14 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 17 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "UTF8", 4 );
	if( hb_xvmDo( 7 ) ) break;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 92 );
	hb_xvmCopyLocals( 17, 15 );
	hb_xvmSetLine( 93 );
	hb_xvmCopyLocals( 15, 1 );
	goto lab00051;
lab00046: ;
	hb_xvmSetLine( 96 );
	hb_xvmCopyLocals( 1, 14 );
	hb_xvmSetLine( 97 );
	hb_xvmCopyLocals( 1, 15 );
	hb_xvmSetLine( 98 );
	hb_xvmSeqBegin();
	for( ;; ) {
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSeqBlock() ) break;
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 100 );
	hb_xvmCopyLocals( 15, 1 );
	hb_stackPop();
	if( hb_xvmSeqEndTest() ) break;
	goto lab00047;
	}
	hb_xvmSetLine( 101 );
	if( hb_xvmSeqRecover() ) break;
	hb_stackPop();
	hb_xvmSetLine( 102 );
	hb_xvmPushLocal( 1 );
	hb_xvmArrayGen( 1 );
	hb_xvmArrayGen( 1 );
	hb_xvmPopLocal( 1 );
lab00047: ;
	goto lab00051;
lab00048: ;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00050;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00049;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00051;
lab00049: ;
	hb_xvmSetLine( 110 );
	hb_xvmPushLocal( 1 );
	hb_xvmArrayGen( 1 );
	hb_xvmArrayGen( 1 );
	hb_xvmPopLocal( 1 );
	goto lab00051;
lab00050: ;
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "BDLP", 4 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00051;
	hb_xvmSetLine( 114 );
	hb_xvmPushLocal( 1 );
	hb_xvmArrayGen( 1 );
	hb_xvmArrayGen( 1 );
	hb_xvmPopLocal( 1 );
lab00051: ;
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushStringConst( "SBrowse", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualInt( 2L ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualInt( 2L ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_stackPop();
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "Key", 3 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_stackPop();
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmExactlyEqual() ) break;
lab00052: ;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00053;
	hb_xvmPushLocal( 8 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00055;
lab00053: ;
	hb_xvmSetLine( 130 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 178L ) ) break;
	hb_xvmSetLine( 132 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00054;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushDouble( * ( double * ) "q=\x0A\xD7\xA3" "p\xE5\?", 10, 2 );
	if( hb_xvmMultEqPop() ) break;
lab00054: ;
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16767935 );
#else
	hb_xvmPushLong( 16767935L );
#endif
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
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 43 ) ) break;
	goto lab00057;
lab00055: ;
	hb_xvmSetLine( 139 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushStringConst( "S", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00056;
	hb_xvmSetLine( 143 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	{
		static const HB_BYTE codeblock[ 24 ] = {
			176, 29, 0, 98, 3, 0, 93, 254, 0, 1, 106, 8, 84, 111, 112, 109, 
			111, 115, 116, 0, 9, 12, 3, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16767935 );
#else
	hb_xvmPushLong( 16767935L );
#endif
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
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
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	goto lab00057;
lab00056: ;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	{
		static const HB_BYTE codeblock[ 24 ] = {
			176, 29, 0, 98, 3, 0, 93, 254, 0, 1, 106, 8, 84, 111, 112, 109, 
			111, 115, 116, 0, 9, 12, 3, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16767935 );
#else
	hb_xvmPushLong( 16767935L );
#endif
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
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
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
lab00057: ;
	hb_xvmSetLine( 153 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00060;
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	hb_xvmPushStringConst( "Cargo", 5 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00058;
	hb_xvmPushLocal( 27 );
	goto lab00059;
lab00058: ;
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmFunction( 0 ) ) break;
lab00059: ;
	if( hb_xvmDo( 4 ) ) break;
	goto lab00063;
lab00060: ;
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Cargo", 5 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00061;
	hb_xvmPushLocal( 27 );
	goto lab00062;
lab00061: ;
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmFunction( 0 ) ) break;
lab00062: ;
	if( hb_xvmDo( 3 ) ) break;
lab00063: ;
	hb_xvmSetLine( 155 );
	hb_xvmCopyLocals( 26, 20 );
	hb_xvmSetLine( 156 );
	hb_xvmCopyLocals( 25, 21 );
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00064;
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00065;
lab00064: ;
	hb_xvmPushFuncSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00065: ;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 21 );
	if( hb_xvmMultByInt( 2L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00066;
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00067;
lab00066: ;
	hb_xvmPushFuncSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00067: ;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmMultByInt( 2L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 161 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00068;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00068;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00068: ;
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushStringConst( "oBrw", 4 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 1 );
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
	hb_xvmPushLocal( 1 );
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
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 20 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 109 ] = {
			1, 0, 0, 0, 48, 38, 0, 95, 1, 121, 112, 1, 73, 48, 39, 0, 
			95, 1, 9, 112, 1, 73, 48, 40, 0, 95, 1, 9, 112, 1, 73, 48, 
			41, 0, 95, 1, 9, 112, 1, 73, 48, 42, 0, 95, 1, 121, 112, 1, 
			73, 48, 43, 0, 95, 1, 120, 112, 1, 73, 48, 44, 0, 95, 1, 122, 
			112, 1, 73, 48, 45, 0, 95, 1, 122, 112, 1, 73, 48, 46, 0, 95, 
			1, 122, 112, 1, 73, 48, 47, 0, 95, 1, 176, 48, 0, 92, 28, 12, 
			1, 112, 1, 73, 48, 49, 0, 95, 1, 120, 112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
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
	if( hb_xvmFunction( 67 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmPushLocal( 12 );
	hb_xvmWithObjectStart();
	hb_xvmSetLine( 179 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 181 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00069;
	hb_xvmPushLocal( 16 );
	goto lab00070;
lab00069: ;
	hb_xvmPushLogical( HB_FALSE );
lab00070: ;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 184 );
	hb_xvmPushLocal( 12 );
	hb_xvmWithObjectStart();
	hb_xvmSetLine( 185 );
	hb_xvmWithObjectMessage( symbols + 51 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 186 );
	hb_xvmWithObjectMessage( symbols + 52 );
	hb_xvmPushLocal( 16 );
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00071;
	hb_stackPop();
	hb_xvmPushLocal( 23 );
lab00071: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 187 );
	hb_xvmWithObjectMessage( symbols + 53 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 190 );
	hb_xvmWithObjectMessage( symbols + 54 );
	{
		static const HB_BYTE codeblock[ 45 ] = {
			0, 0, 4, 0, 12, 0, 28, 0, 31, 0, 32, 0, 176, 55, 0, 48, 
			56, 0, 95, 255, 112, 0, 20, 1, 176, 57, 0, 95, 255, 100, 95, 254, 
			100, 95, 253, 95, 252, 20, 6, 176, 55, 0, 12, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 191 );
	hb_xvmWithObjectMessage( symbols + 58 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 192 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00072;
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmWithObjectMessage( symbols + 60 );
	if( hb_xvmSend( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 27 ] = {
			1, 0, 0, 0, 48, 61, 0, 95, 1, 48, 62, 0, 95, 1, 112, 0, 
			106, 4, 43, 61, 94, 0, 24, 68, 112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00072: ;
	hb_xvmSetLine( 195 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmWithObjectMessage( symbols + 64 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 196 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00073;
	hb_xvmSetLine( 197 );
	hb_xvmWithObjectMessage( symbols + 65 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00076;
lab00073: ;
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00076;
	hb_xvmSetLine( 199 );
	hb_xvmWithObjectMessage( symbols + 66 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 200 );
	hb_xvmWithObjectMessage( symbols + 67 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 201 );
	hb_xvmWithObjectMessage( symbols + 68 );
	hb_xvmWithObjectMessage( symbols + 69 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 202 );
	hb_xvmWithObjectMessage( symbols + 70 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 203 );
	hb_xvmWithObjectMessage( symbols + 71 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00074;
	hb_xvmPushStringConst( "ARRAYNO", 7 );
	goto lab00075;
lab00074: ;
	hb_xvmPushStringConst( "ORDKEYNO", 8 );
lab00075: ;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 204 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmWithObjectMessage( symbols + 64 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmWithObjectMessage( symbols + 74 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 205 );
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmWithObjectMessage( symbols + 64 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 206 );
	hb_xvmWithObjectMessage( symbols + 76 );
	hb_xvmWithObjectMessage( symbols + 77 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 207 );
	hb_xvmWithObjectMessage( symbols + 78 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00076: ;
	hb_xvmSetLine( 209 );
	hb_xvmWithObjectMessage( symbols + 79 );
	hb_xvmWithObjectMessage( symbols + 80 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 210 );
	hb_xvmWithObjectMessage( symbols + 81 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00077;
	hb_xvmWithObjectMessage( symbols + 82 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00078;
lab00077: ;
	hb_xvmWithObjectMessage( symbols + 81 );
	if( hb_xvmSend( 0 ) ) break;
lab00078: ;
	hb_xvmPopLocal( 39 );
	hb_xvmSetLine( 211 );
	hb_xvmWithObjectMessage( symbols + 60 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 30 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00083;
lab00079: ;
	hb_xvmSetLine( 212 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00080;
	hb_xvmPushLocal( 39 );
	goto lab00081;
lab00080: ;
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmSend( 0 ) ) break;
lab00081: ;
	hb_xvmPopLocal( 39 );
	hb_xvmSetLine( 213 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 84 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 214 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00082;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmAddInt( 8L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00082: ;
	hb_xvmSetLine( 216 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00079;
lab00083: ;
	hb_xvmEnumEnd();
	hb_xvmWithObjectEnd();
	hb_xvmSetLine( 219 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmWithObjectEnd();
	hb_xvmSetLine( 221 );
	hb_xvmPushSymbol( symbols + 88 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00084;
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	hb_xvmPushStringConst( "Cargo", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00085;
lab00084: ;
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Cargo", 5 );
	if( hb_xvmFunction( 2 ) ) break;
lab00085: ;
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 223 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00086;
	hb_xvmPushStringConst( "ARRAYNO", 7 );
	goto lab00087;
lab00086: ;
	hb_xvmPushStringConst( "ORDKEYNO", 8 );
lab00087: ;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 224 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00088;
	hb_xvmSetLine( 225 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushLocal( 29 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 226 );
	hb_xvmPushSymbol( symbols + 91 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushLocal( 29 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 227 );
	hb_xvmPushSymbol( symbols + 92 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00088: ;
	hb_xvmSetLine( 230 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00089;
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00090;
lab00089: ;
	hb_xvmPushFuncSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00090: ;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 231 );
	hb_xvmCopyLocals( 25, 21 );
	hb_xvmSetLine( 234 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushStringConst( "Btn_1", 5 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 44L ) ) break;
	{
		static const HB_BYTE codeblock[ 30 ] = {
			0, 0, 2, 0, 12, 0, 2, 0, 48, 96, 0, 95, 255, 95, 254, 100, 
			100, 100, 120, 112, 5, 73, 48, 97, 0, 95, 255, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 25 ) ) break;
	hb_xvmSetLine( 236 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 239 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushStringConst( "Btn_2", 5 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushStringConst( "Excel", 5 );
	{
		static const HB_BYTE codeblock[ 14 ] = {
			0, 0, 1, 0, 12, 0, 48, 99, 0, 95, 255, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 25 ) ) break;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00091;
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00092;
lab00091: ;
	hb_xvmPushFuncSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00092: ;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 244 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushStringConst( "Btn_3", 5 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	{
		static const HB_BYTE codeblock[ 47 ] = {
			0, 0, 1, 0, 12, 0, 48, 100, 0, 95, 255, 112, 0, 28, 11, 48, 
			101, 0, 95, 255, 112, 0, 25, 24, 176, 102, 0, 98, 3, 0, 93, 254, 
			0, 1, 106, 8, 82, 69, 76, 69, 65, 83, 69, 0, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 25 ) ) break;
	hb_xvmSetLine( 246 );
	hb_xvmPushFuncSymbol( symbols + 103 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 27 );
	{
		static const HB_BYTE codeblock[ 47 ] = {
			0, 0, 1, 0, 12, 0, 48, 100, 0, 95, 255, 112, 0, 28, 11, 48, 
			101, 0, 95, 255, 112, 0, 25, 24, 176, 102, 0, 98, 3, 0, 93, 254, 
			0, 1, 106, 8, 82, 69, 76, 69, 65, 83, 69, 0, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 248 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00099;
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 250 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmDec() ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushInteger( 50 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 251 );
	hb_xvmPushSymbol( symbols + 104 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmDec() ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "KEY", 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 252 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmDec() ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 253 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 254 );
	hb_xvmPushSymbol( symbols + 104 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 255 );
	hb_xvmPushSymbol( symbols + 105 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 256 );
	hb_xvmLocalSetInt( 38, 16L );
	hb_xvmSetLine( 257 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 30 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00098;
lab00093: ;
	hb_xvmSetLine( 258 );
	hb_xvmPushSymbol( symbols + 106 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 259 );
	hb_xvmPushSymbol( symbols + 107 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "KEY", 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00094;
	hb_xvmPushInteger( 1 );
	goto lab00095;
lab00094: ;
	hb_xvmPushInteger( 0 );
lab00095: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 260 );
	hb_xvmPushLocalByRef( 38 );
	hb_xvmPushSymbol( symbols + 109 );
	hb_xvmPushLocalByRef( 30 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00096;
	hb_xvmPushInteger( 0 );
	goto lab00097;
lab00096: ;
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmSend( 0 ) ) break;
lab00097: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 261 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00093;
lab00098: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 262 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushSymbol( symbols + 110 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 263 );
	hb_xvmPushFuncSymbol( symbols + 111 );
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00099;
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TSBROWSE", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00099;
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00099;
	hb_xvmSetLine( 264 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 265 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 30 );
	hb_xvmSetLine( 266 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 292 );
	hb_xvmPushSymbol( symbols + 117 );
	hb_xvmPushLocal( 30 );
	{
		static const HB_BYTE codeblock[ 341 ] = {
			2, 0, 0, 0, 36, 13, 1, 120, 100, 100, 100, 100, 36, 14, 1, 176, 
			2, 0, 98, 3, 0, 93, 213, 0, 1, 12, 1, 36, 15, 1, 48, 114, 
			0, 95, 8, 101, 0, 0, 0, 0, 0, 0, 248, 63, 10, 1, 112, 1, 
			36, 16, 1, 48, 115, 0, 95, 2, 106, 4, 75, 69, 89, 0, 112, 1, 
			80, 4, 36, 17, 1, 48, 115, 0, 95, 2, 106, 6, 86, 65, 76, 85, 
			69, 0, 112, 1, 80, 7, 36, 18, 1, 48, 50, 0, 48, 64, 0, 95, 
			2, 106, 6, 86, 65, 76, 85, 69, 0, 112, 1, 100, 112, 1, 73, 36, 
			19, 1, 48, 112, 0, 95, 2, 112, 0, 80, 6, 36, 20, 1, 176, 111, 
			0, 95, 6, 12, 1, 28, 11, 48, 113, 0, 95, 6, 112, 0, 31, 7, 
			36, 21, 1, 9, 6, 36, 23, 1, 48, 64, 0, 95, 6, 95, 4, 112, 
			1, 80, 5, 36, 24, 1, 176, 1, 0, 48, 62, 0, 95, 5, 112, 0, 
			12, 1, 31, 44, 48, 108, 0, 95, 5, 112, 0, 106, 9, 83, 69, 76, 
			69, 67, 84, 79, 82, 0, 8, 31, 23, 48, 108, 0, 95, 5, 112, 0, 
			106, 9, 79, 82, 68, 75, 69, 89, 78, 79, 0, 8, 28, 10, 36, 25, 
			1, 9, 80, 3, 25, 29, 36, 26, 1, 48, 62, 0, 95, 5, 112, 0, 
			106, 6, 84, 61, 64, 43, 94, 0, 24, 28, 8, 36, 27, 1, 9, 80, 
			3, 36, 29, 1, 95, 3, 28, 73, 36, 30, 1, 176, 10, 0, 95, 7, 
			12, 1, 106, 4, 68, 78, 76, 0, 24, 28, 28, 36, 31, 1, 48, 116, 
			0, 48, 64, 0, 95, 2, 106, 6, 86, 65, 76, 85, 69, 0, 112, 1, 
			95, 9, 112, 1, 73, 36, 33, 1, 48, 50, 0, 48, 64, 0, 95, 2, 
			106, 6, 86, 65, 76, 85, 69, 0, 112, 1, 95, 1, 112, 1, 73, 36, 
			35, 1, 95, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 312 );
	hb_xvmPushSymbol( symbols + 121 );
	hb_xvmPushLocal( 30 );
	{
		static const HB_BYTE codeblock[ 229 ] = {
			2, 0, 0, 0, 36, 39, 1, 100, 100, 100, 100, 100, 36, 40, 1, 48, 
			115, 0, 95, 2, 48, 77, 0, 95, 2, 106, 4, 75, 69, 89, 0, 112, 
			1, 112, 1, 80, 3, 36, 41, 1, 48, 112, 0, 48, 64, 0, 95, 2, 
			106, 6, 86, 65, 76, 85, 69, 0, 112, 1, 112, 0, 80, 6, 36, 42, 
			1, 48, 116, 0, 48, 64, 0, 95, 2, 106, 6, 86, 65, 76, 85, 69, 
			0, 112, 1, 121, 112, 1, 73, 36, 43, 1, 95, 6, 100, 69, 28, 14, 
			95, 6, 95, 1, 8, 28, 7, 36, 44, 1, 100, 6, 36, 46, 1, 48, 
			112, 0, 95, 2, 112, 0, 80, 5, 36, 47, 1, 176, 111, 0, 95, 5, 
			12, 1, 28, 11, 48, 113, 0, 95, 5, 112, 0, 31, 7, 36, 48, 1, 
			100, 6, 36, 50, 1, 48, 64, 0, 95, 5, 95, 3, 112, 1, 80, 4, 
			36, 51, 1, 48, 118, 0, 95, 4, 112, 0, 80, 7, 36, 52, 1, 48, 
			119, 0, 95, 4, 121, 112, 1, 73, 36, 53, 1, 48, 120, 0, 95, 5, 
			95, 1, 48, 77, 0, 95, 5, 48, 108, 0, 95, 4, 112, 0, 112, 1, 
			112, 2, 73, 36, 54, 1, 48, 119, 0, 95, 4, 95, 7, 112, 1, 73, 
			36, 55, 1, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00099: ;
	hb_xvmSetLine( 316 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00100;
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00101;
lab00100: ;
	hb_xvmSetLine( 317 );
	hb_xvmPushSymbol( symbols + 122 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 318 );
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 319 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00101;
	hb_xvmSetLine( 320 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00101: ;
	hb_xvmSetLine( 324 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00102;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	goto lab00103;
lab00102: ;
	hb_xvmSetLine( 325 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00103: ;
	hb_xvmSetLine( 328 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00104;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00104;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00104: ;
	hb_xvmSetLine( 331 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 333 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00105;
	hb_xvmSetLine( 334 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "Center", 6 );
	if( hb_xvmDo( 2 ) ) break;
lab00105: ;
	hb_xvmSetLine( 336 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushLocal( 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 338 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 178L ) ) break;
	hb_xvmSetLine( 340 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00106;
	hb_xvmSetLine( 341 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmDo( 0 ) ) break;
	if( hb_xvmPopAlias() ) break;
lab00106: ;
	hb_xvmSetLine( 344 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00107;
	hb_xvmSetLine( 345 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 1 ) ) break;
lab00107: ;
	hb_xvmSetLine( 348 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SBROWSE_RECORD )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 354 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 355 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( "Record View", 11 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 2 );
lab00002: ;
	hb_xvmPopLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 3 );
lab00004: ;
	hb_xvmPopLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushStringConst( "Key", 3 );
	hb_xvmPushStringConst( "Value", 5 );
	hb_xvmArrayGen( 2 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 4 );
lab00006: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 7 );
lab00008: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 357 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
lab00009: ;
	hb_xvmSetLine( 358 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "SELECTOR", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 360 );
	hb_xvmPushSymbol( symbols + 84 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 361 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 362 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 11 );
lab00010: ;
	hb_xvmSetLine( 364 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushSymbol( symbols + 115 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 365 );
lab00011: ;
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
lab00012: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 367 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 369 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _TBROWSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 7 );
	hb_xvmSetLine( 379 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 380 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 381 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 382 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 11 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 383 );
	hb_xvmPushFuncSymbol( symbols + 131 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 384 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 109 );
	hb_xvmPushLocalByRef( 11 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 385 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 386 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00002: ;
	hb_xvmEnumEnd();
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 388 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 389 );
	hb_xvmCopyLocals( 1, 11 );
	hb_xvmSetLine( 390 );
	hb_xvmPushFuncSymbol( symbols + 131 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 8 );
lab00004: ;
	hb_xvmSetLine( 393 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushLocal( 9 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 8 );
lab00006: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _TBROWSE_CREATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 20, 7 );
	hb_xvmSetLine( 402 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 403 );
	hb_xvmPushSymbol( symbols + 133 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 134 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 135 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushSymbol( symbols + 134 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00004: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 404 );
	hb_xvmPushSymbol( symbols + 133 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 134 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmPushSymbol( symbols + 134 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00006: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 406 );
	hb_xvmPushSymbol( symbols + 134 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 407 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 409 );
	hb_xvmPushSymbol( symbols + 136 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 137 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00008;
lab00007: ;
	hb_xvmPushSymbol( symbols + 137 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00008: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 410 );
	hb_xvmPushSymbol( symbols + 138 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 139 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00010;
lab00009: ;
	hb_xvmPushSymbol( symbols + 139 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00010: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 411 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00012;
lab00011: ;
	hb_xvmPushLocal( 3 );
lab00012: ;
	hb_xvmPopLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushSymbol( symbols + 140 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00014;
lab00013: ;
	hb_xvmPushLocal( 2 );
lab00014: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 412 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushStringConst( "oBrw", 4 );
	goto lab00016;
lab00015: ;
	hb_xvmPushLocal( 3 );
lab00016: ;
	hb_xvmPopLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00018;
lab00017: ;
	hb_xvmPushLocal( 2 );
lab00018: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 413 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushSymbol( symbols + 141 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00020;
lab00019: ;
	hb_xvmPushLocal( 4 );
lab00020: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmPushSymbol( symbols + 142 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00022;
lab00021: ;
	hb_xvmPushLocal( 5 );
lab00022: ;
	hb_xvmPopLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00024;
lab00023: ;
	hb_xvmPushLocal( 6 );
lab00024: ;
	hb_xvmPopLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmPushSymbol( symbols + 143 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00026;
lab00025: ;
	hb_xvmPushLocal( 7 );
lab00026: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 414 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmPushSymbol( symbols + 144 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00028;
lab00027: ;
	hb_xvmPushLocal( 4 );
lab00028: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmPushSymbol( symbols + 145 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00030;
lab00029: ;
	hb_xvmPushLocal( 5 );
lab00030: ;
	hb_xvmPopLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmPushSymbol( symbols + 146 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00032;
lab00031: ;
	hb_xvmPushLocal( 6 );
lab00032: ;
	hb_xvmPopLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmPushSymbol( symbols + 147 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00034;
lab00033: ;
	hb_xvmPushLocal( 7 );
lab00034: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 415 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmPushSymbol( symbols + 148 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00036;
lab00035: ;
	hb_xvmPushLocal( 17 );
lab00036: ;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 416 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmPushSymbol( symbols + 149 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00038;
lab00037: ;
	hb_xvmPushLocal( 17 );
lab00038: ;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 417 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmPushSymbol( symbols + 150 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00040;
lab00039: ;
	hb_xvmPushLocal( 17 );
lab00040: ;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 418 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00041;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00042;
lab00041: ;
	hb_xvmPushLocal( 17 );
lab00042: ;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 420 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 151 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00043;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 152 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
lab00043: ;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 422 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 153 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00044;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 154 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00044;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 155 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
lab00044: ;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 424 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 156 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00045;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 157 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00045;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 158 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
lab00045: ;
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 426 );
	hb_xvmPushSymbol( symbols + 159 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 160 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
	hb_xvmPushSymbol( symbols + 161 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00047;
lab00046: ;
	hb_xvmPushSymbol( symbols + 160 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00047: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 427 );
	hb_xvmPushSymbol( symbols + 162 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 163 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00048;
	hb_xvmPushSymbol( symbols + 164 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00049;
lab00048: ;
	hb_xvmPushSymbol( symbols + 163 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00049: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 428 );
	hb_xvmPushSymbol( symbols + 165 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 166 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00050;
	hb_xvmPushSymbol( symbols + 167 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00051;
lab00050: ;
	hb_xvmPushSymbol( symbols + 166 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00051: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 429 );
	hb_xvmPushSymbol( symbols + 168 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 169 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_xvmPushSymbol( symbols + 170 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00053;
lab00052: ;
	hb_xvmPushSymbol( symbols + 169 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00053: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 430 );
	hb_xvmPushSymbol( symbols + 171 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 172 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00054;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00055;
lab00054: ;
	hb_xvmPushSymbol( symbols + 172 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00055: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 432 );
	hb_xvmPushFuncSymbol( symbols + 111 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00061;
	hb_xvmPushStringConst( "TODBC", 5 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00061;
	hb_xvmSetLine( 433 );
	hb_xvmPushSymbol( symbols + 173 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 434 );
	hb_xvmPushSymbol( symbols + 174 );
	hb_xvmPushLocal( 1 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 435 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 175 );
	hb_xvmPushSymbol( symbols + 176 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00058;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 175 );
	hb_xvmPushSymbol( symbols + 176 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00058;
	hb_xvmSetLine( 436 );
	hb_xvmPushSymbol( symbols + 175 );
	hb_xvmPushSymbol( symbols + 176 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 27 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00057;
lab00056: ;
	hb_xvmSetLine( 437 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 177 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 178 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 438 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00056;
lab00057: ;
	hb_xvmEnumEnd();
	goto lab00059;
lab00058: ;
	hb_xvmSetLine( 440 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 177 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Fields not found !", 18 );
	if( hb_xvmDo( 2 ) ) break;
lab00059: ;
	hb_xvmSetLine( 442 );
	hb_xvmPushSymbol( symbols + 176 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 443 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00060;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00060;
	hb_xvmSetLine( 444 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00065;
lab00060: ;
	hb_xvmSetLine( 446 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 177 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	hb_xvmArrayGen( 1 );
	hb_xvmPopLocal( 2 );
	goto lab00065;
lab00061: ;
	hb_xvmSetLine( 448 );
	hb_xvmPushFuncSymbol( symbols + 180 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00062;
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00062;
	hb_xvmSetLine( 449 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00065;
lab00062: ;
	hb_xvmSetLine( 450 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00065;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00065;
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00065;
	hb_xvmSetLine( 451 );
	hb_xvmCopyLocals( 2, 26 );
	hb_xvmSetLine( 452 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 453 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocalByRef( 25 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00064;
lab00063: ;
	hb_xvmSetLine( 454 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 25 );
	hb_xvmArrayGen( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 455 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00063;
lab00064: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 456 );
	hb_xvmPushSymbol( symbols + 171 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00065: ;
	hb_xvmSetLine( 459 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00070;
	hb_xvmSetLine( 460 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessThenIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00066;
	hb_xvmSetLine( 461 );
	hb_xvmPushFuncSymbol( symbols + 182 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00066: ;
	hb_xvmSetLine( 463 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 25 );
	goto lab00069;
lab00067: ;
	hb_xvmSetLine( 464 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00068;
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
lab00068: ;
	hb_xvmSetLine( 463 );
	if( hb_xvmLocalIncPush( 25 ) ) break;
lab00069: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00067;
	goto lab00074;
lab00070: ;
	hb_xvmSetLine( 468 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 183 );
	hb_xvmPushStringConst( "Normal", 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00071;
	hb_xvmSetLine( 469 );
	hb_xvmPushFuncSymbol( symbols + 184 );
	hb_xvmPushStringConst( "Normal", 6 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	if( hb_xvmDo( 10 ) ) break;
lab00071: ;
	hb_xvmSetLine( 471 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 183 );
	hb_xvmPushStringConst( "Bold", 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00072;
	hb_xvmSetLine( 472 );
	hb_xvmPushFuncSymbol( symbols + 184 );
	hb_xvmPushStringConst( "BOLD", 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	if( hb_xvmDo( 10 ) ) break;
lab00072: ;
	hb_xvmSetLine( 474 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 183 );
	hb_xvmPushStringConst( "Italic", 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00073;
	hb_xvmSetLine( 475 );
	hb_xvmPushFuncSymbol( symbols + 184 );
	hb_xvmPushStringConst( "Italic", 6 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	if( hb_xvmDo( 10 ) ) break;
lab00073: ;
	hb_xvmSetLine( 477 );
	hb_xvmPushSymbol( symbols + 185 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Normal", 6 );
	hb_xvmPushStringConst( "Bold", 4 );
	hb_xvmPushStringConst( "Bold", 4 );
	hb_xvmPushStringConst( "Italic", 6 );
	hb_xvmPushStringConst( "Bold", 4 );
	hb_xvmArrayGen( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00074: ;
	hb_xvmSetLine( 480 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00076;
	hb_xvmSetLine( 481 );
	hb_xvmLocalSetInt( 25, 0L );
lab00075: ;
	hb_xvmSetLine( 482 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 73 );
	if( hb_xvmLocalIncPush( 25 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 26 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00075;
	hb_xvmSetLine( 484 );
	hb_xvmCopyLocals( 26, 3 );
lab00076: ;
	hb_xvmSetLine( 487 );
	hb_xvmPushSymbol( symbols + 187 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 488 );
	hb_xvmPushSymbol( symbols + 188 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 489 );
	hb_xvmPushSymbol( symbols + 189 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 490 );
	hb_xvmPushSymbol( symbols + 190 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 491 );
	hb_xvmPushSymbol( symbols + 177 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00077;
	hb_xvmPushSymbol( symbols + 191 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00078;
lab00077: ;
	hb_xvmPushLocal( 11 );
lab00078: ;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 492 );
	hb_xvmPushSymbol( symbols + 192 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00079;
	hb_xvmPushSymbol( symbols + 193 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00080;
lab00079: ;
	hb_xvmPushLocal( 12 );
lab00080: ;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 493 );
	hb_xvmPushSymbol( symbols + 194 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00081;
	hb_xvmPushSymbol( symbols + 195 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00082;
lab00081: ;
	hb_xvmPushLocal( 13 );
lab00082: ;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 500 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00083;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	goto lab00084;
lab00083: ;
	hb_xvmPushLocal( 13 );
lab00084: ;
	hb_xvmPopLocal( 13 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00085;
	hb_xvmPushInteger( 0 );
	goto lab00086;
lab00085: ;
	hb_xvmPushLocal( 4 );
lab00086: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00087;
	hb_xvmPushInteger( 0 );
	goto lab00088;
lab00087: ;
	hb_xvmPushLocal( 5 );
lab00088: ;
	hb_xvmPopLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00089;
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmMultByInt( 2L ) ) break;
	if( hb_xvmMinus() ) break;
	goto lab00090;
lab00089: ;
	hb_xvmPushLocal( 6 );
lab00090: ;
	hb_xvmPopLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00093;
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDec() ) break;
	hb_xvmPushFuncSymbol( symbols + 186 );
	hb_xvmPushStringConst( "StatusBar", 9 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00091;
	hb_xvmPushFuncSymbol( symbols + 89 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushStringConst( "StatusBar", 9 );
	hb_xvmPushStringConst( "Height", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00092;
lab00091: ;
	hb_xvmPushInteger( 0 );
lab00092: ;
	if( hb_xvmMinus() ) break;
	goto lab00094;
lab00093: ;
	hb_xvmPushLocal( 7 );
lab00094: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 506 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00095;
	hb_xvmPushInteger( 5 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 2 );
	hb_xvmPushInteger( 6 );
	{
		static const HB_BYTE codeblock[ 64 ] = {
			3, 0, 0, 0, 95, 2, 80, 1, 48, 196, 0, 95, 3, 112, 0, 95, 
			2, 8, 28, 7, 93, 1, 255, 25, 40, 48, 113, 0, 95, 3, 112, 0, 
			28, 26, 85, 48, 197, 0, 95, 3, 112, 0, 74, 176, 198, 0, 12, 0, 
			119, 28, 9, 97, 64, 63, 63, 255, 25, 7, 97, 128, 30, 30, 255, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmArrayGen( 2 );
	hb_xvmPushInteger( 11 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 2 );
	hb_xvmPushInteger( 12 );
	{
		static const HB_BYTE codeblock[ 33 ] = {
			3, 0, 0, 0, 95, 2, 80, 1, 48, 196, 0, 95, 3, 112, 0, 95, 
			2, 8, 28, 9, 97, 0, 0, 128, 255, 25, 7, 97, 128, 30, 30, 255, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmArrayGen( 2 );
	hb_xvmArrayGen( 4 );
	goto lab00096;
lab00095: ;
	hb_xvmPushLocal( 14 );
lab00096: ;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 508 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00103;
	hb_xvmSetLine( 509 );
	hb_xvmPushSymbol( symbols + 199 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00097;
	hb_xvmPushSymbol( symbols + 200 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00098;
lab00097: ;
	hb_xvmPushLocal( 22 );
lab00098: ;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 510 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00099;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 2 );
	goto lab00100;
lab00099: ;
	hb_xvmPushLocal( 22 );
lab00100: ;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 511 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00110;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00110;
	hb_xvmSetLine( 512 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00101;
	hb_xvmPushFuncSymbol( symbols + 201 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00101;
	hb_xvmSetLine( 513 );
	hb_xvmPushFuncSymbol( symbols + 202 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
lab00101: ;
	hb_xvmSetLine( 515 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00102;
	hb_xvmPushFuncSymbol( symbols + 201 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00102;
	hb_xvmSetLine( 516 );
	hb_xvmPushFuncSymbol( symbols + 202 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00102: ;
	hb_xvmSetLine( 518 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00110;
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00110;
	hb_xvmSetLine( 520 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushInteger( 2 );
	{
		static const HB_BYTE codeblock[ 40 ] = {
			3, 0, 1, 0, 22, 0, 95, 255, 92, 2, 1, 80, 1, 95, 255, 122, 
			1, 80, 2, 48, 204, 0, 95, 3, 112, 0, 92, 2, 50, 121, 8, 28, 
			6, 95, 1, 25, 4, 95, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00110;
lab00103: ;
	hb_xvmSetLine( 523 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00110;
	hb_xvmSetLine( 524 );
	hb_xvmPushSymbol( symbols + 205 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 24 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00104;
	hb_xvmPushSymbol( symbols + 206 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00105;
lab00104: ;
	hb_xvmPushLocal( 24 );
lab00105: ;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 525 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00106;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 2 );
	goto lab00107;
lab00106: ;
	hb_xvmPushLocal( 24 );
lab00107: ;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 526 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00110;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00110;
	hb_xvmSetLine( 527 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00108;
	hb_xvmPushFuncSymbol( symbols + 201 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00108;
	hb_xvmSetLine( 528 );
	hb_xvmPushFuncSymbol( symbols + 202 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
lab00108: ;
	hb_xvmSetLine( 530 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00109;
	hb_xvmPushFuncSymbol( symbols + 201 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00109;
	hb_xvmSetLine( 531 );
	hb_xvmPushFuncSymbol( symbols + 202 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00109: ;
	hb_xvmSetLine( 538 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushInteger( 2 );
	{
		static const HB_BYTE codeblock[ 64 ] = {
			3, 0, 1, 0, 24, 0, 36, 22, 2, 95, 1, 92, 2, 50, 121, 8, 
			28, 20, 95, 2, 92, 2, 50, 121, 8, 28, 5, 122, 25, 4, 92, 2, 
			80, 3, 25, 21, 36, 23, 2, 95, 2, 92, 2, 50, 121, 8, 28, 6, 
			92, 2, 25, 3, 122, 80, 3, 36, 25, 2, 95, 255, 95, 3, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00110: ;
	hb_xvmSetLine( 542 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 207 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00115;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 207 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00115;
	hb_xvmSetLine( 543 );
	hb_xvmPushSymbol( symbols + 207 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 26 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00114;
lab00111: ;
	hb_xvmSetLine( 544 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00113;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00113;
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00113;
	hb_xvmSetLine( 545 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 546 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 547 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00112;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00112;
	hb_xvmSetLine( 548 );
	hb_xvmPushFuncSymbol( symbols + 202 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 26 );
lab00112: ;
	hb_xvmSetLine( 550 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00113: ;
	hb_xvmSetLine( 552 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00111;
lab00114: ;
	hb_xvmEnumEnd();
lab00115: ;
	hb_xvmSetLine( 573 );
	hb_xvmPushSymbol( symbols + 211 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 212 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00116;
	{
		static const HB_BYTE codeblock[ 210 ] = {
			3, 0, 0, 0, 36, 45, 2, 100, 100, 121, 36, 46, 2, 48, 113, 0, 
			95, 1, 112, 0, 28, 15, 106, 9, 79, 82, 68, 75, 69, 89, 78, 79, 
			0, 25, 12, 106, 8, 65, 82, 82, 65, 89, 78, 79, 0, 36, 47, 2, 
			48, 150, 0, 95, 1, 112, 0, 29, 150, 0, 36, 48, 2, 95, 3, 100, 
			8, 28, 11, 48, 208, 0, 95, 2, 112, 0, 25, 4, 95, 3, 80, 3, 
			36, 49, 2, 95, 3, 100, 8, 28, 8, 106, 2, 46, 0, 25, 4, 95, 
			3, 80, 3, 36, 50, 2, 48, 60, 0, 95, 1, 112, 0, 96, 4, 0, 
			129, 1, 1, 28, 89, 36, 51, 2, 48, 108, 0, 95, 4, 112, 0, 106, 
			9, 83, 69, 76, 69, 67, 84, 79, 82, 0, 8, 31, 62, 36, 53, 2, 
			95, 3, 80, 5, 36, 54, 2, 48, 108, 0, 95, 4, 112, 0, 95, 7, 
			69, 28, 24, 48, 209, 0, 95, 4, 112, 0, 28, 15, 36, 55, 2, 176, 
			73, 0, 175, 6, 0, 12, 1, 80, 5, 36, 57, 2, 48, 210, 0, 95, 
			4, 95, 5, 112, 1, 73, 36, 58, 2, 130, 31, 171, 132, 36, 60, 2, 
			100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	goto lab00117;
lab00116: ;
	hb_xvmPushSymbol( symbols + 212 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00117: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 590 );
	hb_xvmPushSymbol( symbols + 215 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 216 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00118;
	{
		static const HB_BYTE codeblock[ 182 ] = {
			1, 0, 0, 0, 36, 64, 2, 100, 100, 100, 36, 65, 2, 48, 113, 0, 
			95, 1, 112, 0, 28, 15, 106, 9, 79, 82, 68, 75, 69, 89, 78, 79, 
			0, 25, 12, 106, 8, 65, 82, 82, 65, 89, 78, 79, 0, 36, 67, 2, 
			176, 213, 0, 48, 77, 0, 95, 1, 106, 9, 83, 69, 76, 69, 67, 84, 
			79, 82, 0, 120, 112, 2, 48, 77, 0, 95, 1, 95, 5, 120, 112, 2, 
			12, 2, 80, 4, 36, 68, 2, 95, 4, 121, 15, 28, 73, 36, 69, 2, 
			4, 0, 0, 80, 2, 36, 70, 2, 95, 4, 165, 80, 3, 25, 40, 36, 
			71, 2, 48, 209, 0, 48, 60, 0, 95, 1, 112, 0, 95, 3, 1, 112, 
			0, 28, 14, 36, 72, 2, 176, 129, 0, 95, 2, 95, 3, 20, 2, 36, 
			70, 2, 175, 3, 0, 176, 7, 0, 48, 60, 0, 95, 1, 112, 0, 12, 
			1, 15, 28, 205, 36, 76, 2, 48, 214, 0, 95, 1, 95, 2, 112, 1, 
			73, 36, 77, 2, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	goto lab00119;
lab00118: ;
	hb_xvmPushSymbol( symbols + 216 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00119: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 605 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00120;
	{
		static const HB_BYTE codeblock[ 134 ] = {
			2, 0, 0, 0, 36, 82, 2, 48, 15, 0, 95, 2, 112, 0, 100, 69, 
			28, 59, 48, 217, 0, 95, 2, 112, 0, 100, 8, 28, 48, 48, 218, 0, 
			95, 1, 112, 0, 28, 39, 36, 83, 2, 176, 34, 0, 48, 216, 0, 95, 
			2, 112, 0, 12, 1, 28, 22, 36, 84, 2, 48, 36, 0, 48, 216, 0, 
			95, 2, 112, 0, 95, 1, 95, 2, 112, 2, 73, 36, 87, 2, 48, 74, 
			0, 95, 1, 112, 0, 48, 219, 0, 95, 1, 112, 0, 15, 28, 14, 36, 
			88, 2, 48, 220, 0, 95, 1, 120, 112, 1, 73, 36, 90, 2, 48, 122, 
			0, 95, 1, 112, 0, 73, 36, 91, 2, 48, 101, 0, 95, 1, 112, 0, 
			73, 36, 92, 2, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	goto lab00121;
lab00120: ;
	hb_xvmPushLocal( 19 );
lab00121: ;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 697 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 221 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmArrayGen( 1 );
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 222 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 224 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 225 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 226 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 227 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 14 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 228 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmArrayGen( 1 );
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 12 );
	hb_xvmArrayGen( 1 );
	{
		static const HB_BYTE codeblock[ 149 ] = {
			1, 0, 0, 0, 48, 50, 0, 95, 1, 176, 12, 0, 12, 0, 112, 1, 
			73, 48, 38, 0, 95, 1, 121, 112, 1, 73, 48, 65, 0, 95, 1, 120, 
			112, 1, 73, 48, 39, 0, 95, 1, 9, 112, 1, 73, 48, 40, 0, 95, 
			1, 9, 112, 1, 73, 48, 41, 0, 95, 1, 9, 112, 1, 73, 48, 105, 
			0, 95, 1, 9, 112, 1, 73, 48, 229, 0, 95, 1, 120, 112, 1, 73, 
			48, 42, 0, 95, 1, 121, 112, 1, 73, 48, 43, 0, 95, 1, 120, 112, 
			1, 73, 48, 44, 0, 95, 1, 122, 112, 1, 73, 48, 45, 0, 95, 1, 
			122, 112, 1, 73, 48, 46, 0, 95, 1, 122, 112, 1, 73, 48, 47, 0, 
			95, 1, 176, 48, 0, 92, 28, 12, 1, 112, 1, 73, 48, 49, 0, 95, 
			1, 120, 112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushSymbol( symbols + 230 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 13 );
	hb_xvmPushSymbol( symbols + 231 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 10 );
	hb_xvmPushSymbol( symbols + 232 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 217 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 67 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmPushLocal( 8 );
	hb_xvmWithObjectStart();
	hb_xvmSetLine( 699 );
	hb_xvmWithObjectMessage( symbols + 136 );
	hb_xvmPushSymbol( symbols + 137 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 700 );
	hb_xvmWithObjectMessage( symbols + 138 );
	hb_xvmPushSymbol( symbols + 139 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 702 );
	hb_xvmPushSymbol( symbols + 233 );
	hb_xvmWithObjectMessage( symbols + 112 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 703 );
	hb_xvmWithObjectMessage( symbols + 234 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 704 );
	hb_xvmWithObjectMessage( symbols + 235 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 706 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00125;
	hb_xvmSetLine( 707 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushSymbol( symbols + 236 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00122;
	hb_xvmPushSymbol( symbols + 236 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00122;
	hb_xvmSetLine( 708 );
	hb_xvmWithObjectMessage( symbols + 237 );
	hb_xvmPushSymbol( symbols + 236 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00125;
lab00122: ;
	hb_xvmSetLine( 710 );
	hb_xvmWithObjectMessage( symbols + 237 );
	hb_xvmPushFuncSymbol( symbols + 238 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00123;
	hb_xvmPushInteger( 4 );
	goto lab00124;
lab00123: ;
	hb_xvmPushInteger( 1 );
lab00124: ;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00125: ;
	hb_xvmSetLine( 714 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00126;
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00126;
	hb_xvmSetLine( 715 );
	hb_xvmWithObjectMessage( symbols + 239 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00126: ;
	hb_xvmSetLine( 718 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushSymbol( symbols + 240 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00127;
	hb_xvmPushSymbol( symbols + 240 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00127;
	hb_xvmSetLine( 719 );
	hb_xvmWithObjectMessage( symbols + 241 );
	hb_xvmPushSymbol( symbols + 240 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00127: ;
	hb_xvmSetLine( 722 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushSymbol( symbols + 242 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00128;
	hb_xvmPushSymbol( symbols + 242 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00128;
	hb_xvmSetLine( 723 );
	hb_xvmWithObjectMessage( symbols + 68 );
	hb_xvmPushSymbol( symbols + 242 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00128: ;
	hb_xvmSetLine( 726 );
	hb_xvmWithObjectMessage( symbols + 243 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 727 );
	hb_xvmWithObjectMessage( symbols + 244 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 729 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00129;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00129: ;
	hb_xvmSetLine( 732 );
	hb_xvmPushSymbol( symbols + 172 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00135;
	hb_xvmSetLine( 733 );
	hb_xvmWithObjectMessage( symbols + 113 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00130;
	hb_xvmPushStringConst( "ORDKEYNO", 8 );
	goto lab00131;
lab00130: ;
	hb_xvmPushStringConst( "ARRAYNO", 7 );
lab00131: ;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 734 );
	hb_xvmWithObjectMessage( symbols + 60 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 25 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00134;
lab00132: ;
	hb_xvmSetLine( 735 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00133;
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00133;
	hb_xvmSetLine( 736 );
	hb_xvmPushSymbol( symbols + 106 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00133: ;
	hb_xvmSetLine( 738 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00132;
lab00134: ;
	hb_xvmEnumEnd();
lab00135: ;
	hb_xvmSetLine( 741 );
	hb_xvmWithObjectMessage( symbols + 150 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00136;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 231 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00136;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 212 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00136;
	hb_xvmSetLine( 742 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 212 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00136: ;
	hb_xvmSetLine( 745 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 245 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00137;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 245 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00137: ;
	hb_xvmSetLine( 748 );
	hb_xvmWithObjectMessage( symbols + 246 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00144;
	hb_xvmPushLocal( 20 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00144;
	hb_xvmSetLine( 749 );
	hb_xvmPushSymbol( symbols + 247 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 248 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00138;
	hb_xvmPushStringConst( " ", 1 );
	goto lab00139;
lab00138: ;
	hb_xvmPushSymbol( symbols + 248 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00139: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 750 );
	hb_xvmPushSymbol( symbols + 249 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 250 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00140;
	hb_xvmPushSymbol( symbols + 251 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00141;
lab00140: ;
	hb_xvmPushSymbol( symbols + 250 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00141: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 751 );
	hb_xvmPushSymbol( symbols + 249 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 250 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00142;
	hb_xvmWithObjectMessage( symbols + 69 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00143;
lab00142: ;
	hb_xvmPushSymbol( symbols + 250 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00143: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 753 );
	hb_xvmWithObjectMessage( symbols + 252 );
	hb_xvmPushInteger( 1 );
	hb_xvmWithObjectMessage( symbols + 253 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 248 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 250 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 254 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 255 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmSend( 13 ) ) break;
	hb_stackPop();
lab00144: ;
	hb_xvmSetLine( 756 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 256 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00145;
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 160 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00146;
lab00145: ;
	hb_xvmSetLine( 757 );
	hb_xvmWithObjectMessage( symbols + 257 );
	{
		static const HB_BYTE codeblock[ 25 ] = {
			4, 0, 0, 0, 95, 3, 165, 80, 2, 80, 1, 48, 2, 1, 95, 4, 
			93, 0, 1, 92, 13, 121, 112, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00148;
lab00146: ;
	hb_xvmSetLine( 758 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 256 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00147;
	hb_xvmSetLine( 759 );
	hb_xvmWithObjectMessage( symbols + 257 );
	hb_xvmPushSymbol( symbols + 256 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00148;
lab00147: ;
	hb_xvmSetLine( 760 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 160 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00148;
	hb_xvmSetLine( 761 );
	hb_xvmWithObjectMessage( symbols + 257 );
	hb_xvmPushSymbol( symbols + 160 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00148: ;
	hb_xvmSetLine( 764 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 259 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00149;
	hb_xvmSetLine( 765 );
	hb_xvmWithObjectMessage( symbols + 54 );
	hb_xvmPushSymbol( symbols + 259 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00149: ;
	hb_xvmSetLine( 768 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 260 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00150;
	hb_xvmSetLine( 769 );
	hb_xvmWithObjectMessage( symbols + 261 );
	hb_xvmPushSymbol( symbols + 260 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00150: ;
	hb_xvmSetLine( 772 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 262 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00151;
	hb_xvmSetLine( 773 );
	hb_xvmWithObjectMessage( symbols + 263 );
	hb_xvmPushSymbol( symbols + 262 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00151: ;
	hb_xvmSetLine( 776 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushSymbol( symbols + 264 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00152;
	hb_xvmSetLine( 777 );
	hb_xvmWithObjectMessage( symbols + 265 );
	hb_xvmPushSymbol( symbols + 264 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00152: ;
	hb_xvmSetLine( 780 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 266 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00159;
	hb_xvmSetLine( 781 );
	hb_xvmPushSymbol( symbols + 266 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00158;
lab00153: ;
	hb_xvmSetLine( 782 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00154;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	goto lab00155;
lab00154: ;
	hb_xvmPushLogical( HB_FALSE );
lab00155: ;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 783 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00156;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	goto lab00157;
lab00156: ;
	hb_xvmPushLogical( HB_FALSE );
lab00157: ;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 784 );
	hb_xvmWithObjectMessage( symbols + 267 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmSend( 4 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 785 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00153;
lab00158: ;
	hb_xvmEnumEnd();
lab00159: ;
	hb_xvmSetLine( 788 );
	hb_xvmWithObjectMessage( symbols + 74 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00160;
	hb_xvmSetLine( 789 );
	hb_xvmWithObjectMessage( symbols + 268 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 790 );
	hb_xvmWithObjectMessage( symbols + 79 );
	hb_xvmWithObjectMessage( symbols + 80 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00160: ;
	hb_xvmSetLine( 793 );
	hb_xvmWithObjectMessage( symbols + 269 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDec() ) break;
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmWithObjectMessage( symbols + 110 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00161;
	hb_xvmSetLine( 794 );
	hb_xvmWithObjectMessage( symbols + 65 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 795 );
	hb_xvmWithObjectMessage( symbols + 270 );
	hb_xvmWithObjectMessage( symbols + 253 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenInt( 30L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00162;
lab00161: ;
	hb_xvmSetLine( 796 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00162;
	hb_xvmPushSymbol( symbols + 217 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00162;
	hb_xvmSetLine( 797 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 216 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00162;
	hb_xvmSetLine( 798 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 216 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00162: ;
	hb_xvmSetLine( 802 );
	hb_xvmWithObjectMessage( symbols + 220 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 803 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmWithObjectMessage( symbols + 271 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00163;
	hb_xvmSetLine( 804 );
	hb_xvmPushSymbol( symbols + 272 );
	hb_xvmWithObjectMessage( symbols + 271 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00163: ;
	hb_xvmSetLine( 807 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmWithObjectEnd();
	hb_xvmSetLine( 809 );
	hb_xvmPushSymbol( symbols + 246 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00164;
	hb_xvmSetLine( 810 );
	hb_xvmPushSymbol( symbols + 253 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 273 );
	hb_xvmPushSymbol( symbols + 274 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00164: ;
	hb_xvmSetLine( 813 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 163 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00165;
	hb_xvmSetLine( 814 );
	hb_xvmPushSymbol( symbols + 162 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushSymbol( symbols + 163 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00165: ;
	hb_xvmSetLine( 817 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 166 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00166;
	hb_xvmSetLine( 818 );
	hb_xvmPushSymbol( symbols + 165 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushSymbol( symbols + 166 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00166: ;
	hb_xvmSetLine( 821 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 169 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00167;
	hb_xvmSetLine( 822 );
	hb_xvmPushSymbol( symbols + 168 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushSymbol( symbols + 169 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00167: ;
	hb_xvmSetLine( 825 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 275 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00168;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 275 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00168: ;
	hb_xvmSetLine( 828 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00169;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00169: ;
	hb_xvmSetLine( 831 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 276 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00174;
	hb_xvmSetLine( 832 );
	hb_xvmPushSymbol( symbols + 276 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00173;
lab00170: ;
	hb_xvmSetLine( 833 );
	hb_xvmPushSymbol( symbols + 277 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00171;
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	hb_xvmPushStringConst( "Object", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00172;
lab00171: ;
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Object", 6 );
	if( hb_xvmFunction( 2 ) ) break;
lab00172: ;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 834 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00170;
lab00173: ;
	hb_xvmEnumEnd();
lab00174: ;
	hb_xvmSetLine( 837 );
	hb_xvmPushFuncSymbol( symbols + 278 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 839 );
	hb_xvmPushLocal( 8 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

