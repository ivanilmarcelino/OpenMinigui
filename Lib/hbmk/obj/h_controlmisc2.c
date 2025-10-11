/*
 * Harbour 3.2.0dev (r2510040809)
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
HB_FUNC_EXTERN( AFILL );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( GETFONTWIDTH );
HB_FUNC_EXTERN( DELETED );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( HMG_RGB2N );
HB_FUNC_EXTERN( PROCNAME );
HB_FUNC_STATIC( _TBROWSE_BSPECHDENUM );
HB_FUNC_STATIC( _TBROWSE_BADJCOLUMNS );
HB_FUNC_STATIC( _TBROWSE_BINIT );
HB_FUNC_STATIC( _TBROWSE_BBODY );
HB_FUNC_STATIC( _TBROWSE_BAFTER );
HB_FUNC_STATIC( _TBROWSE_BEND );
HB_FUNC_EXTERN( GETFONTHEIGHT );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( DOEVENTS );
HB_FUNC_EXTERN( MAX );
HB_FUNC_EXTERN( ACLONE );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( PCOUNT );


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
{ "LZEBRAROW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LZEBRALINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
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
{ "_ASIZELEN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ASIZELEN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ASIZECHAR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ASIZECHARS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ASIZE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AFILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFILL )}, NULL },
{ "ASIZE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "GETFONTWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTWIDTH )}, NULL },
{ "NCELL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CALIAS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DELETED", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELETED )}, NULL },
{ "AZEBRA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AZEBRACOLOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "HMG_RGB2N", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_RGB2N )}, NULL },
{ "NAT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LZEBRALINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AZEBRAGROUPCOLOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_AZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACHESS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACHESSCOLOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACOLORADD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "L_LOG_OUT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "B_LOG_OUT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PROCNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCNAME )}, NULL },
{ "_BSPECHDENUM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BSPECHDENUM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_TBROWSE_BSPECHDENUM", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _TBROWSE_BSPECHDENUM )}, NULL },
{ "_BADJCOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BADJCOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_TBROWSE_BADJCOLUMNS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _TBROWSE_BADJCOLUMNS )}, NULL },
{ "_TBROWSE_BINIT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _TBROWSE_BINIT )}, NULL },
{ "_BBODY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BBODY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_TBROWSE_BBODY", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _TBROWSE_BBODY )}, NULL },
{ "_BAFTER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BAFTER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_TBROWSE_BAFTER", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _TBROWSE_BAFTER )}, NULL },
{ "_TBROWSE_BEND", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _TBROWSE_BEND )}, NULL },
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
{ "LADJUST", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
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
{ "RESETVSCROLL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OHSCROLL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETRANGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "ASUPERHEAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AEVENTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EVENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DOEVENTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOEVENTS )}, NULL },
{ "CSPECHDCHAR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LVISIBLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CSPCHEADING", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "B_ADJCOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MAX", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAX )}, NULL },
{ "ADJCOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LHIDE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HIDE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ARELATION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACLONE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ACLONE )}, NULL },
{ "LOADFIELDS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DELCOLUMN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "B_ANUMBER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "B_INIT_DEF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "B_BODY_DEF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADELCOL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADELCOLUMN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADELCOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AMOVECOL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AMOVECOLUMN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AMOVECOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "MOVECOLUMN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AHIDECOL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AHIDECOLUMN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AHIDECOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HIDECOLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "B_SUPER_HD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "B_SUPER_HEAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_UZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_OZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "UZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NZEBRAGROUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "_BZEBRAGROUP_NCLRBACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BZEBRAGROUP_NCLRBACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BONDRAWLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BONDRAWLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BHLCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BHLCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BHRCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BHRCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BSLCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BSLCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BSRCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BSRCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BFLCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BFLCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BFRCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BFRCLICKED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "B_AFTER_DEF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "B_END_DEF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LNOHSCROLL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NROWCOUNT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL }
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
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushStringConst( "SBrowse", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 126 );
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
	hb_xvmSetLine( 128 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00053;
	hb_xvmPushLocal( 8 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00055;
lab00053: ;
	hb_xvmSetLine( 129 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 178L ) ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00054;
	hb_xvmSetLine( 132 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushDouble( * ( double * ) "q=\x0A\xD7\xA3" "p\xE5\?", 10, 2 );
	if( hb_xvmMultEqPop() ) break;
lab00054: ;
	hb_xvmSetLine( 136 );
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
	hb_xvmSetLine( 138 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushStringConst( "S", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00056;
	hb_xvmSetLine( 142 );
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
	hb_xvmSetLine( 148 );
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
	hb_xvmSetLine( 152 );
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
	hb_xvmSetLine( 154 );
	hb_xvmCopyLocals( 26, 20 );
	hb_xvmSetLine( 155 );
	hb_xvmCopyLocals( 25, 21 );
	hb_xvmSetLine( 157 );
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
	hb_xvmSetLine( 158 );
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
	hb_xvmSetLine( 160 );
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
	hb_xvmSetLine( 176 );
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
	hb_xvmSetLine( 178 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 180 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 181 );
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
	hb_xvmSetLine( 183 );
	hb_xvmPushLocal( 12 );
	hb_xvmWithObjectStart();
	hb_xvmSetLine( 184 );
	hb_xvmWithObjectMessage( symbols + 51 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 185 );
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
	hb_xvmSetLine( 186 );
	hb_xvmWithObjectMessage( symbols + 53 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 189 );
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
	hb_xvmSetLine( 190 );
	hb_xvmWithObjectMessage( symbols + 58 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 191 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00072;
	hb_xvmSetLine( 192 );
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
	hb_xvmSetLine( 194 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmWithObjectMessage( symbols + 64 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 195 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00073;
	hb_xvmSetLine( 196 );
	hb_xvmWithObjectMessage( symbols + 65 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00076;
lab00073: ;
	hb_xvmSetLine( 197 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00076;
	hb_xvmSetLine( 198 );
	hb_xvmWithObjectMessage( symbols + 66 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 199 );
	hb_xvmWithObjectMessage( symbols + 67 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 200 );
	hb_xvmWithObjectMessage( symbols + 68 );
	hb_xvmWithObjectMessage( symbols + 69 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 201 );
	hb_xvmWithObjectMessage( symbols + 70 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 202 );
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
	hb_xvmSetLine( 203 );
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
	hb_xvmSetLine( 204 );
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmWithObjectMessage( symbols + 64 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 205 );
	hb_xvmWithObjectMessage( symbols + 76 );
	hb_xvmWithObjectMessage( symbols + 77 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 206 );
	hb_xvmWithObjectMessage( symbols + 78 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00076: ;
	hb_xvmSetLine( 208 );
	hb_xvmWithObjectMessage( symbols + 79 );
	hb_xvmWithObjectMessage( symbols + 80 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 209 );
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
	hb_xvmSetLine( 210 );
	hb_xvmWithObjectMessage( symbols + 60 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 30 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00083;
lab00079: ;
	hb_xvmSetLine( 211 );
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
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 84 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 213 );
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
	hb_xvmSetLine( 215 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00079;
lab00083: ;
	hb_xvmEnumEnd();
	hb_xvmWithObjectEnd();
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmWithObjectEnd();
	hb_xvmSetLine( 220 );
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
	hb_xvmSetLine( 222 );
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
	hb_xvmSetLine( 223 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00088;
	hb_xvmSetLine( 224 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushLocal( 29 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 225 );
	hb_xvmPushSymbol( symbols + 91 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushLocal( 29 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 226 );
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
	hb_xvmSetLine( 229 );
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
	hb_xvmSetLine( 230 );
	hb_xvmCopyLocals( 25, 21 );
	hb_xvmSetLine( 233 );
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
	hb_xvmSetLine( 235 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 238 );
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
	hb_xvmSetLine( 240 );
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
	hb_xvmSetLine( 243 );
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
	hb_xvmSetLine( 245 );
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
	hb_xvmSetLine( 247 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00099;
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 249 );
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
	hb_xvmSetLine( 250 );
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
	hb_xvmSetLine( 251 );
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
	hb_xvmSetLine( 252 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 253 );
	hb_xvmPushSymbol( symbols + 104 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 254 );
	hb_xvmPushSymbol( symbols + 105 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 255 );
	hb_xvmLocalSetInt( 38, 16L );
	hb_xvmSetLine( 256 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 30 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00098;
lab00093: ;
	hb_xvmSetLine( 257 );
	hb_xvmPushSymbol( symbols + 106 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 258 );
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
	hb_xvmSetLine( 259 );
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
	hb_xvmSetLine( 260 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00093;
lab00098: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 261 );
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
	hb_xvmSetLine( 262 );
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
	hb_xvmSetLine( 263 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 264 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 30 );
	hb_xvmSetLine( 265 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 291 );
	hb_xvmPushSymbol( symbols + 117 );
	hb_xvmPushLocal( 30 );
	{
		static const HB_BYTE codeblock[ 341 ] = {
			2, 0, 0, 0, 36, 12, 1, 120, 100, 100, 100, 100, 36, 13, 1, 176, 
			2, 0, 98, 3, 0, 93, 213, 0, 1, 12, 1, 36, 14, 1, 48, 114, 
			0, 95, 8, 101, 0, 0, 0, 0, 0, 0, 248, 63, 10, 1, 112, 1, 
			36, 15, 1, 48, 115, 0, 95, 2, 106, 4, 75, 69, 89, 0, 112, 1, 
			80, 4, 36, 16, 1, 48, 115, 0, 95, 2, 106, 6, 86, 65, 76, 85, 
			69, 0, 112, 1, 80, 7, 36, 17, 1, 48, 50, 0, 48, 64, 0, 95, 
			2, 106, 6, 86, 65, 76, 85, 69, 0, 112, 1, 100, 112, 1, 73, 36, 
			18, 1, 48, 112, 0, 95, 2, 112, 0, 80, 6, 36, 19, 1, 176, 111, 
			0, 95, 6, 12, 1, 28, 11, 48, 113, 0, 95, 6, 112, 0, 31, 7, 
			36, 20, 1, 9, 6, 36, 22, 1, 48, 64, 0, 95, 6, 95, 4, 112, 
			1, 80, 5, 36, 23, 1, 176, 1, 0, 48, 62, 0, 95, 5, 112, 0, 
			12, 1, 31, 44, 48, 108, 0, 95, 5, 112, 0, 106, 9, 83, 69, 76, 
			69, 67, 84, 79, 82, 0, 8, 31, 23, 48, 108, 0, 95, 5, 112, 0, 
			106, 9, 79, 82, 68, 75, 69, 89, 78, 79, 0, 8, 28, 10, 36, 24, 
			1, 9, 80, 3, 25, 29, 36, 25, 1, 48, 62, 0, 95, 5, 112, 0, 
			106, 6, 84, 61, 64, 43, 94, 0, 24, 28, 8, 36, 26, 1, 9, 80, 
			3, 36, 28, 1, 95, 3, 28, 73, 36, 29, 1, 176, 10, 0, 95, 7, 
			12, 1, 106, 4, 68, 78, 76, 0, 24, 28, 28, 36, 30, 1, 48, 116, 
			0, 48, 64, 0, 95, 2, 106, 6, 86, 65, 76, 85, 69, 0, 112, 1, 
			95, 9, 112, 1, 73, 36, 32, 1, 48, 50, 0, 48, 64, 0, 95, 2, 
			106, 6, 86, 65, 76, 85, 69, 0, 112, 1, 95, 1, 112, 1, 73, 36, 
			34, 1, 95, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 311 );
	hb_xvmPushSymbol( symbols + 121 );
	hb_xvmPushLocal( 30 );
	{
		static const HB_BYTE codeblock[ 229 ] = {
			2, 0, 0, 0, 36, 38, 1, 100, 100, 100, 100, 100, 36, 39, 1, 48, 
			115, 0, 95, 2, 48, 77, 0, 95, 2, 106, 4, 75, 69, 89, 0, 112, 
			1, 112, 1, 80, 3, 36, 40, 1, 48, 112, 0, 48, 64, 0, 95, 2, 
			106, 6, 86, 65, 76, 85, 69, 0, 112, 1, 112, 0, 80, 6, 36, 41, 
			1, 48, 116, 0, 48, 64, 0, 95, 2, 106, 6, 86, 65, 76, 85, 69, 
			0, 112, 1, 121, 112, 1, 73, 36, 42, 1, 95, 6, 100, 69, 28, 14, 
			95, 6, 95, 1, 8, 28, 7, 36, 43, 1, 100, 6, 36, 45, 1, 48, 
			112, 0, 95, 2, 112, 0, 80, 5, 36, 46, 1, 176, 111, 0, 95, 5, 
			12, 1, 28, 11, 48, 113, 0, 95, 5, 112, 0, 31, 7, 36, 47, 1, 
			100, 6, 36, 49, 1, 48, 64, 0, 95, 5, 95, 3, 112, 1, 80, 4, 
			36, 50, 1, 48, 118, 0, 95, 4, 112, 0, 80, 7, 36, 51, 1, 48, 
			119, 0, 95, 4, 121, 112, 1, 73, 36, 52, 1, 48, 120, 0, 95, 5, 
			95, 1, 48, 77, 0, 95, 5, 48, 108, 0, 95, 4, 112, 0, 112, 1, 
			112, 2, 73, 36, 53, 1, 48, 119, 0, 95, 4, 95, 7, 112, 1, 73, 
			36, 54, 1, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00099: ;
	hb_xvmSetLine( 315 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00100;
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00101;
lab00100: ;
	hb_xvmSetLine( 316 );
	hb_xvmPushSymbol( symbols + 122 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 317 );
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 318 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00101;
	hb_xvmSetLine( 319 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00101: ;
	hb_xvmSetLine( 323 );
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
	hb_xvmSetLine( 324 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00103: ;
	hb_xvmSetLine( 327 );
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
	hb_xvmSetLine( 330 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 332 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00105;
	hb_xvmSetLine( 333 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "Center", 6 );
	if( hb_xvmDo( 2 ) ) break;
lab00105: ;
	hb_xvmSetLine( 335 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushLocal( 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 337 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 178L ) ) break;
	hb_xvmSetLine( 339 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00106;
	hb_xvmSetLine( 340 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmDo( 0 ) ) break;
	if( hb_xvmPopAlias() ) break;
lab00106: ;
	hb_xvmSetLine( 343 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00107;
	hb_xvmSetLine( 344 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 1 ) ) break;
lab00107: ;
	hb_xvmSetLine( 347 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SBROWSE_RECORD )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 353 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 354 );
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
	hb_xvmSetLine( 356 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
lab00009: ;
	hb_xvmSetLine( 357 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "SELECTOR", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 359 );
	hb_xvmPushSymbol( symbols + 84 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 360 );
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
	hb_xvmSetLine( 361 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 11 );
lab00010: ;
	hb_xvmSetLine( 363 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushSymbol( symbols + 115 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 364 );
lab00011: ;
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
lab00012: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 366 );
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
	hb_xvmSetLine( 368 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _TBROWSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 7 );
	hb_xvmSetLine( 378 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 379 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 380 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 381 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 11 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 382 );
	hb_xvmPushFuncSymbol( symbols + 131 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 383 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 109 );
	hb_xvmPushLocalByRef( 11 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 384 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 385 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00002: ;
	hb_xvmEnumEnd();
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 387 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 388 );
	hb_xvmCopyLocals( 1, 11 );
	hb_xvmSetLine( 389 );
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
	hb_xvmSetLine( 392 );
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
	hb_xvmFrame( 21, 7 );
	hb_xvmSetLine( 401 );
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
	hb_xvmSetLine( 402 );
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
	hb_xvmSetLine( 405 );
	hb_xvmPushSymbol( symbols + 134 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 406 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 408 );
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
	hb_xvmSetLine( 409 );
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
	hb_xvmSetLine( 410 );
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
	hb_xvmSetLine( 411 );
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
	hb_xvmSetLine( 412 );
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
	hb_xvmSetLine( 413 );
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
	hb_xvmSetLine( 414 );
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
	hb_xvmSetLine( 415 );
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
	hb_xvmSetLine( 416 );
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
	hb_xvmSetLine( 417 );
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
	hb_xvmSetLine( 419 );
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
	hb_xvmSetLine( 421 );
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
	hb_xvmSetLine( 423 );
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
	hb_xvmSetLine( 425 );
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
	hb_xvmSetLine( 426 );
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
	hb_xvmSetLine( 427 );
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
	hb_xvmSetLine( 428 );
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
	hb_xvmSetLine( 429 );
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
	hb_xvmSetLine( 431 );
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
	hb_xvmSetLine( 432 );
	hb_xvmPushSymbol( symbols + 173 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 433 );
	hb_xvmPushSymbol( symbols + 174 );
	hb_xvmPushLocal( 1 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 434 );
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
	hb_xvmSetLine( 435 );
	hb_xvmPushSymbol( symbols + 175 );
	hb_xvmPushSymbol( symbols + 176 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 28 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00057;
lab00056: ;
	hb_xvmSetLine( 436 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 177 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 178 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 437 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00056;
lab00057: ;
	hb_xvmEnumEnd();
	goto lab00059;
lab00058: ;
	hb_xvmSetLine( 439 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 177 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Fields not found !", 18 );
	if( hb_xvmDo( 2 ) ) break;
lab00059: ;
	hb_xvmSetLine( 441 );
	hb_xvmPushSymbol( symbols + 176 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 442 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00060;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00060;
	hb_xvmSetLine( 443 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00065;
lab00060: ;
	hb_xvmSetLine( 445 );
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
	hb_xvmSetLine( 447 );
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
	hb_xvmSetLine( 448 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00065;
lab00062: ;
	hb_xvmSetLine( 449 );
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
	hb_xvmSetLine( 450 );
	hb_xvmCopyLocals( 2, 27 );
	hb_xvmSetLine( 451 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 452 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocalByRef( 26 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00064;
lab00063: ;
	hb_xvmSetLine( 453 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 26 );
	hb_xvmArrayGen( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 454 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00063;
lab00064: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 455 );
	hb_xvmPushSymbol( symbols + 171 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00065: ;
	hb_xvmSetLine( 458 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00070;
	hb_xvmSetLine( 459 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessThenIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00066;
	hb_xvmSetLine( 460 );
	hb_xvmPushFuncSymbol( symbols + 182 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00066: ;
	hb_xvmSetLine( 462 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 26 );
	goto lab00069;
lab00067: ;
	hb_xvmSetLine( 463 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
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
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPop() ) break;
lab00068: ;
	hb_xvmSetLine( 462 );
	if( hb_xvmLocalIncPush( 26 ) ) break;
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
	hb_xvmSetLine( 467 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 183 );
	hb_xvmPushStringConst( "Normal", 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00071;
	hb_xvmSetLine( 468 );
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
	hb_xvmSetLine( 470 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 183 );
	hb_xvmPushStringConst( "Bold", 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00072;
	hb_xvmSetLine( 471 );
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
	hb_xvmSetLine( 473 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 183 );
	hb_xvmPushStringConst( "Italic", 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00073;
	hb_xvmSetLine( 474 );
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
	hb_xvmSetLine( 476 );
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
	hb_xvmSetLine( 479 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00076;
	hb_xvmSetLine( 480 );
	hb_xvmLocalSetInt( 26, 0L );
lab00075: ;
	hb_xvmSetLine( 481 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 73 );
	if( hb_xvmLocalIncPush( 26 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 27 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00075;
	hb_xvmSetLine( 483 );
	hb_xvmCopyLocals( 27, 3 );
lab00076: ;
	hb_xvmSetLine( 486 );
	hb_xvmPushSymbol( symbols + 187 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 487 );
	hb_xvmPushSymbol( symbols + 188 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 488 );
	hb_xvmPushSymbol( symbols + 189 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 489 );
	hb_xvmPushSymbol( symbols + 190 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 490 );
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
	hb_xvmSetLine( 491 );
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
	hb_xvmSetLine( 492 );
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
	hb_xvmSetLine( 494 );
	hb_xvmPushSymbol( symbols + 196 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00083;
	hb_xvmPushSymbol( symbols + 198 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00084;
lab00083: ;
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00084: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 495 );
	hb_xvmPushSymbol( symbols + 196 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00085;
	hb_xvmPushSymbol( symbols + 199 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00086;
lab00085: ;
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00086: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 496 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00091;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00091;
	hb_xvmSetLine( 497 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 498 );
	hb_xvmPushSymbol( symbols + 200 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 27 );
	hb_xvmArrayDim( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 201 );
	hb_xvmPushSymbol( symbols + 202 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 10 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 499 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00087;
	hb_xvmPushFuncSymbol( symbols + 182 );
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmDo( 2 ) ) break;
lab00087: ;
	hb_xvmSetLine( 501 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 502 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 26 );
	goto lab00090;
lab00088: ;
	hb_xvmSetLine( 503 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00089;
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00089;
	hb_xvmSetLine( 504 );
	hb_xvmPushFuncSymbol( symbols + 204 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushSymbol( symbols + 202 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPop() ) break;
lab00089: ;
	hb_xvmSetLine( 502 );
	if( hb_xvmLocalIncPush( 26 ) ) break;
lab00090: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 202 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00088;
lab00091: ;
	hb_xvmSetLine( 514 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00092;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	goto lab00093;
lab00092: ;
	hb_xvmPushLocal( 13 );
lab00093: ;
	hb_xvmPopLocal( 13 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00094;
	hb_xvmPushInteger( 0 );
	goto lab00095;
lab00094: ;
	hb_xvmPushLocal( 4 );
lab00095: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00096;
	hb_xvmPushInteger( 0 );
	goto lab00097;
lab00096: ;
	hb_xvmPushLocal( 5 );
lab00097: ;
	hb_xvmPopLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00098;
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmMultByInt( 2L ) ) break;
	if( hb_xvmMinus() ) break;
	goto lab00099;
lab00098: ;
	hb_xvmPushLocal( 6 );
lab00099: ;
	hb_xvmPopLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00102;
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
		goto lab00100;
	hb_xvmPushFuncSymbol( symbols + 89 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushStringConst( "StatusBar", 9 );
	hb_xvmPushStringConst( "Height", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00101;
lab00100: ;
	hb_xvmPushInteger( 0 );
lab00101: ;
	if( hb_xvmMinus() ) break;
	goto lab00103;
lab00102: ;
	hb_xvmPushLocal( 7 );
lab00103: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 520 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00104;
	hb_xvmPushInteger( 5 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 2 );
	hb_xvmPushInteger( 6 );
	{
		static const HB_BYTE codeblock[ 64 ] = {
			3, 0, 0, 0, 95, 2, 80, 1, 48, 205, 0, 95, 3, 112, 0, 95, 
			2, 8, 28, 7, 93, 1, 255, 25, 40, 48, 113, 0, 95, 3, 112, 0, 
			28, 26, 85, 48, 206, 0, 95, 3, 112, 0, 74, 176, 207, 0, 12, 0, 
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
			3, 0, 0, 0, 95, 2, 80, 1, 48, 205, 0, 95, 3, 112, 0, 95, 
			2, 8, 28, 9, 97, 0, 0, 128, 255, 25, 7, 97, 128, 30, 30, 255, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmArrayGen( 2 );
	hb_xvmArrayGen( 4 );
	goto lab00105;
lab00104: ;
	hb_xvmPushLocal( 14 );
lab00105: ;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 522 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00125;
	hb_xvmSetLine( 523 );
	hb_xvmPushSymbol( symbols + 208 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00106;
	hb_xvmPushSymbol( symbols + 209 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00107;
lab00106: ;
	hb_xvmPushLocal( 22 );
lab00107: ;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 524 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00108;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 2 );
	goto lab00109;
lab00108: ;
	hb_xvmPushLocal( 22 );
lab00109: ;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 525 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00112;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00112;
	hb_xvmSetLine( 526 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00110;
	hb_xvmPushFuncSymbol( symbols + 210 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00110;
	hb_xvmSetLine( 527 );
	hb_xvmPushFuncSymbol( symbols + 211 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
lab00110: ;
	hb_xvmSetLine( 529 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00111;
	hb_xvmPushFuncSymbol( symbols + 210 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00111;
	hb_xvmSetLine( 530 );
	hb_xvmPushFuncSymbol( symbols + 211 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00111: ;
	hb_xvmSetLine( 532 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00112;
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00112;
	hb_xvmSetLine( 534 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushInteger( 2 );
	{
		static const HB_BYTE codeblock[ 40 ] = {
			3, 0, 1, 0, 22, 0, 95, 255, 92, 2, 1, 80, 1, 95, 255, 122, 
			1, 80, 2, 48, 212, 0, 95, 3, 112, 0, 92, 2, 50, 121, 8, 28, 
			6, 95, 1, 25, 4, 95, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00112: ;
	hb_xvmSetLine( 537 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 213 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00132;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 214 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00132;
	hb_xvmSetLine( 538 );
	hb_xvmPushSymbol( symbols + 215 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 155 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00113;
	hb_xvmPushSymbol( symbols + 154 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00114;
lab00113: ;
	hb_xvmPushSymbol( symbols + 155 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00114: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 539 );
	hb_xvmPushSymbol( symbols + 215 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 155 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00115;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00116;
lab00115: ;
	hb_xvmPushSymbol( symbols + 155 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00116: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 540 );
	hb_xvmPushSymbol( symbols + 216 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 25 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00117;
	hb_xvmPushSymbol( symbols + 217 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00118;
lab00117: ;
	hb_xvmPushLocal( 25 );
lab00118: ;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 541 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00122;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00122;
	hb_xvmSetLine( 542 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 26 );
	goto lab00121;
lab00119: ;
	hb_xvmSetLine( 543 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00120;
	hb_xvmPushFuncSymbol( symbols + 210 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00120;
	hb_xvmSetLine( 544 );
	hb_xvmPushFuncSymbol( symbols + 211 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPop() ) break;
lab00120: ;
	hb_xvmSetLine( 542 );
	if( hb_xvmLocalIncPush( 26 ) ) break;
lab00121: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00119;
	hb_xvmSetLine( 547 );
	hb_xvmPushSymbol( symbols + 218 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00122: ;
	hb_xvmSetLine( 549 );
	hb_xvmPushSymbol( symbols + 218 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 216 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00123;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 12632256 );
#else
	hb_xvmPushLong( 12632256L );
#endif
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmArrayGen( 2 );
	goto lab00124;
lab00123: ;
	hb_xvmPushSymbol( symbols + 216 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00124: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00132;
lab00125: ;
	hb_xvmSetLine( 552 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00132;
	hb_xvmSetLine( 553 );
	hb_xvmPushSymbol( symbols + 219 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 24 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00126;
	hb_xvmPushSymbol( symbols + 220 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00127;
lab00126: ;
	hb_xvmPushLocal( 24 );
lab00127: ;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 554 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00128;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 2 );
	goto lab00129;
lab00128: ;
	hb_xvmPushLocal( 24 );
lab00129: ;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 555 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00132;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00132;
	hb_xvmSetLine( 556 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00130;
	hb_xvmPushFuncSymbol( symbols + 210 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00130;
	hb_xvmSetLine( 557 );
	hb_xvmPushFuncSymbol( symbols + 211 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
lab00130: ;
	hb_xvmSetLine( 559 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00131;
	hb_xvmPushFuncSymbol( symbols + 210 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00131;
	hb_xvmSetLine( 560 );
	hb_xvmPushFuncSymbol( symbols + 211 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00131: ;
	hb_xvmSetLine( 567 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushInteger( 2 );
	{
		static const HB_BYTE codeblock[ 64 ] = {
			3, 0, 1, 0, 24, 0, 36, 51, 2, 95, 1, 92, 2, 50, 121, 8, 
			28, 20, 95, 2, 92, 2, 50, 121, 8, 28, 5, 122, 25, 4, 92, 2, 
			80, 3, 25, 21, 36, 52, 2, 95, 2, 92, 2, 50, 121, 8, 28, 6, 
			92, 2, 25, 3, 122, 80, 3, 36, 54, 2, 95, 255, 95, 3, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00132: ;
	hb_xvmSetLine( 571 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 221 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00137;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 221 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00137;
	hb_xvmSetLine( 572 );
	hb_xvmPushSymbol( symbols + 221 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 27 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00136;
lab00133: ;
	hb_xvmSetLine( 573 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00135;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00135;
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00135;
	hb_xvmSetLine( 574 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 575 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 576 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00134;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00134;
	hb_xvmSetLine( 577 );
	hb_xvmPushFuncSymbol( symbols + 211 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 27 );
lab00134: ;
	hb_xvmSetLine( 579 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00135: ;
	hb_xvmSetLine( 581 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00133;
lab00136: ;
	hb_xvmEnumEnd();
lab00137: ;
	hb_xvmSetLine( 584 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 222 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00138;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00138;
	hb_xvmSetLine( 585 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 224 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00138: ;
	hb_xvmSetLine( 588 );
	hb_xvmPushSymbol( symbols + 225 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 226 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00139;
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 227, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	goto lab00140;
lab00139: ;
	hb_xvmPushSymbol( symbols + 226 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00140: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 590 );
	hb_xvmPushSymbol( symbols + 228 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 229 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00141;
	{
		static const HB_BYTE codeblock[ 14 ] = {
			2, 0, 0, 0, 176, 230, 0, 95, 1, 95, 2, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	goto lab00142;
lab00141: ;
	hb_xvmPushSymbol( symbols + 229 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00142: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 592 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00143;
	{
		static const HB_BYTE codeblock[ 14 ] = {
			2, 0, 0, 0, 176, 231, 0, 95, 1, 95, 2, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	goto lab00144;
lab00143: ;
	hb_xvmPushLocal( 18 );
lab00144: ;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 594 );
	hb_xvmPushSymbol( symbols + 232 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 233 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00145;
	{
		static const HB_BYTE codeblock[ 14 ] = {
			2, 0, 0, 0, 176, 234, 0, 95, 1, 95, 2, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	goto lab00146;
lab00145: ;
	hb_xvmPushSymbol( symbols + 233 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00146: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 596 );
	hb_xvmPushSymbol( symbols + 235 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 236 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00147;
	{
		static const HB_BYTE codeblock[ 14 ] = {
			2, 0, 0, 0, 176, 237, 0, 95, 1, 95, 2, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	goto lab00148;
lab00147: ;
	hb_xvmPushSymbol( symbols + 236 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00148: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 598 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00149;
	{
		static const HB_BYTE codeblock[ 14 ] = {
			2, 0, 0, 0, 176, 238, 0, 95, 1, 95, 2, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	goto lab00150;
lab00149: ;
	hb_xvmPushLocal( 19 );
lab00150: ;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 638 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 202 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmArrayGen( 1 );
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 239 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 240 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 241 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 242 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 243 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 244 );
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
	hb_xvmPushSymbol( symbols + 245 );
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
			0, 95, 1, 9, 112, 1, 73, 48, 246, 0, 95, 1, 120, 112, 1, 73, 
			48, 42, 0, 95, 1, 121, 112, 1, 73, 48, 43, 0, 95, 1, 120, 112, 
			1, 73, 48, 44, 0, 95, 1, 122, 112, 1, 73, 48, 45, 0, 95, 1, 
			122, 112, 1, 73, 48, 46, 0, 95, 1, 122, 112, 1, 73, 48, 47, 0, 
			95, 1, 176, 48, 0, 92, 28, 12, 1, 112, 1, 73, 48, 49, 0, 95, 
			1, 120, 112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushSymbol( symbols + 247 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 13 );
	hb_xvmPushSymbol( symbols + 248 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 10 );
	hb_xvmPushSymbol( symbols + 249 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 250 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 67 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmPushLocal( 8 );
	hb_xvmWithObjectStart();
	hb_xvmSetLine( 640 );
	hb_xvmWithObjectMessage( symbols + 136 );
	hb_xvmPushSymbol( symbols + 137 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 641 );
	hb_xvmWithObjectMessage( symbols + 138 );
	hb_xvmPushSymbol( symbols + 139 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 643 );
	hb_xvmPushSymbol( symbols + 251 );
	hb_xvmWithObjectMessage( symbols + 112 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 644 );
	hb_xvmWithObjectMessage( symbols + 252 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 645 );
	hb_xvmWithObjectMessage( symbols + 253 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 647 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00154;
	hb_xvmSetLine( 648 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushSymbol( symbols + 254 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00151;
	hb_xvmPushSymbol( symbols + 254 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00151;
	hb_xvmSetLine( 649 );
	hb_xvmWithObjectMessage( symbols + 255 );
	hb_xvmPushSymbol( symbols + 254 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00154;
lab00151: ;
	hb_xvmSetLine( 651 );
	hb_xvmWithObjectMessage( symbols + 255 );
	hb_xvmPushFuncSymbol( symbols + 256 );
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
		goto lab00152;
	hb_xvmPushInteger( 4 );
	goto lab00153;
lab00152: ;
	hb_xvmPushInteger( 1 );
lab00153: ;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00154: ;
	hb_xvmSetLine( 655 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00155;
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00155;
	hb_xvmSetLine( 656 );
	hb_xvmWithObjectMessage( symbols + 257 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00155: ;
	hb_xvmSetLine( 659 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushSymbol( symbols + 258 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00156;
	hb_xvmPushSymbol( symbols + 258 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00156;
	hb_xvmSetLine( 660 );
	hb_xvmWithObjectMessage( symbols + 259 );
	hb_xvmPushSymbol( symbols + 258 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00156: ;
	hb_xvmSetLine( 663 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushSymbol( symbols + 260 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00157;
	hb_xvmPushSymbol( symbols + 260 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00157;
	hb_xvmSetLine( 664 );
	hb_xvmWithObjectMessage( symbols + 68 );
	hb_xvmPushSymbol( symbols + 260 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00157: ;
	hb_xvmSetLine( 667 );
	hb_xvmWithObjectMessage( symbols + 261 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 668 );
	hb_xvmWithObjectMessage( symbols + 262 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 670 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00158;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00158: ;
	hb_xvmSetLine( 673 );
	hb_xvmPushSymbol( symbols + 172 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00164;
	hb_xvmSetLine( 674 );
	hb_xvmWithObjectMessage( symbols + 113 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00159;
	hb_xvmPushStringConst( "ORDKEYNO", 8 );
	goto lab00160;
lab00159: ;
	hb_xvmPushStringConst( "ARRAYNO", 7 );
lab00160: ;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 675 );
	hb_xvmWithObjectMessage( symbols + 60 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 26 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00163;
lab00161: ;
	hb_xvmSetLine( 676 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00162;
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00162;
	hb_xvmSetLine( 677 );
	hb_xvmPushSymbol( symbols + 106 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00162: ;
	hb_xvmSetLine( 679 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00161;
lab00163: ;
	hb_xvmEnumEnd();
lab00164: ;
	hb_xvmSetLine( 682 );
	hb_xvmWithObjectMessage( symbols + 150 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00165;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 248 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00165;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 226 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00165;
	hb_xvmSetLine( 683 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 226 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00165: ;
	hb_xvmSetLine( 686 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 233 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00166;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 233 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00166: ;
	hb_xvmSetLine( 689 );
	hb_xvmWithObjectMessage( symbols + 263 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00173;
	hb_xvmPushLocal( 20 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00173;
	hb_xvmSetLine( 690 );
	hb_xvmPushSymbol( symbols + 264 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 265 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00167;
	hb_xvmPushStringConst( " ", 1 );
	goto lab00168;
lab00167: ;
	hb_xvmPushSymbol( symbols + 265 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00168: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 691 );
	hb_xvmPushSymbol( symbols + 266 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 267 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00169;
	hb_xvmPushSymbol( symbols + 268 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00170;
lab00169: ;
	hb_xvmPushSymbol( symbols + 267 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00170: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 692 );
	hb_xvmPushSymbol( symbols + 266 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 267 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00171;
	hb_xvmWithObjectMessage( symbols + 69 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00172;
lab00171: ;
	hb_xvmPushSymbol( symbols + 267 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
lab00172: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 694 );
	hb_xvmWithObjectMessage( symbols + 269 );
	hb_xvmPushInteger( 1 );
	hb_xvmWithObjectMessage( symbols + 270 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 265 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 267 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 271 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 272 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmSend( 13 ) ) break;
	hb_stackPop();
lab00173: ;
	hb_xvmSetLine( 697 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 273 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00174;
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 160 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00175;
lab00174: ;
	hb_xvmSetLine( 698 );
	hb_xvmWithObjectMessage( symbols + 274 );
	{
		static const HB_BYTE codeblock[ 25 ] = {
			4, 0, 0, 0, 95, 3, 165, 80, 2, 80, 1, 48, 19, 1, 95, 4, 
			93, 0, 1, 92, 13, 121, 112, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00177;
lab00175: ;
	hb_xvmSetLine( 699 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 273 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00176;
	hb_xvmSetLine( 700 );
	hb_xvmWithObjectMessage( symbols + 274 );
	hb_xvmPushSymbol( symbols + 273 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00177;
lab00176: ;
	hb_xvmSetLine( 701 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 160 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00177;
	hb_xvmSetLine( 702 );
	hb_xvmWithObjectMessage( symbols + 274 );
	hb_xvmPushSymbol( symbols + 160 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00177: ;
	hb_xvmSetLine( 705 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 276 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00178;
	hb_xvmSetLine( 706 );
	hb_xvmWithObjectMessage( symbols + 54 );
	hb_xvmPushSymbol( symbols + 276 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00178: ;
	hb_xvmSetLine( 709 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 277 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00179;
	hb_xvmSetLine( 710 );
	hb_xvmWithObjectMessage( symbols + 278 );
	hb_xvmPushSymbol( symbols + 277 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00179: ;
	hb_xvmSetLine( 713 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 279 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00180;
	hb_xvmSetLine( 714 );
	hb_xvmWithObjectMessage( symbols + 280 );
	hb_xvmPushSymbol( symbols + 279 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00180: ;
	hb_xvmSetLine( 717 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushSymbol( symbols + 281 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00181;
	hb_xvmSetLine( 718 );
	hb_xvmWithObjectMessage( symbols + 282 );
	hb_xvmPushSymbol( symbols + 281 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00181: ;
	hb_xvmSetLine( 721 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 283 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00188;
	hb_xvmSetLine( 722 );
	hb_xvmPushSymbol( symbols + 283 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00187;
lab00182: ;
	hb_xvmSetLine( 723 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00183;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	goto lab00184;
lab00183: ;
	hb_xvmPushLogical( HB_FALSE );
lab00184: ;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 724 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00185;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	goto lab00186;
lab00185: ;
	hb_xvmPushLogical( HB_FALSE );
lab00186: ;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 725 );
	hb_xvmWithObjectMessage( symbols + 284 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmSend( 4 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 726 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00182;
lab00187: ;
	hb_xvmEnumEnd();
lab00188: ;
	hb_xvmSetLine( 729 );
	hb_xvmWithObjectMessage( symbols + 74 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00189;
	hb_xvmSetLine( 730 );
	hb_xvmWithObjectMessage( symbols + 285 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 731 );
	hb_xvmWithObjectMessage( symbols + 79 );
	hb_xvmWithObjectMessage( symbols + 80 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00189: ;
	hb_xvmSetLine( 734 );
	hb_xvmWithObjectMessage( symbols + 286 );
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
		goto lab00190;
	hb_xvmSetLine( 735 );
	hb_xvmWithObjectMessage( symbols + 65 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 736 );
	hb_xvmWithObjectMessage( symbols + 287 );
	hb_xvmWithObjectMessage( symbols + 270 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenInt( 30L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00191;
lab00190: ;
	hb_xvmSetLine( 737 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00191;
	hb_xvmPushSymbol( symbols + 250 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00191;
	hb_xvmSetLine( 738 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 229 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00191;
	hb_xvmSetLine( 739 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 229 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00191: ;
	hb_xvmSetLine( 743 );
	hb_xvmWithObjectMessage( symbols + 288 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 744 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmWithObjectMessage( symbols + 289 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00192;
	hb_xvmSetLine( 745 );
	hb_xvmPushSymbol( symbols + 290 );
	hb_xvmWithObjectMessage( symbols + 289 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00192: ;
	hb_xvmSetLine( 748 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmWithObjectEnd();
	hb_xvmSetLine( 750 );
	hb_xvmPushSymbol( symbols + 263 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00193;
	hb_xvmSetLine( 751 );
	hb_xvmPushSymbol( symbols + 270 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 291 );
	hb_xvmPushSymbol( symbols + 292 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00193: ;
	hb_xvmSetLine( 754 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 163 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00194;
	hb_xvmSetLine( 755 );
	hb_xvmPushSymbol( symbols + 162 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushSymbol( symbols + 163 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00194: ;
	hb_xvmSetLine( 758 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 166 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00195;
	hb_xvmSetLine( 759 );
	hb_xvmPushSymbol( symbols + 165 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushSymbol( symbols + 166 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00195: ;
	hb_xvmSetLine( 762 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 169 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00196;
	hb_xvmSetLine( 763 );
	hb_xvmPushSymbol( symbols + 168 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushSymbol( symbols + 169 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00196: ;
	hb_xvmSetLine( 766 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 236 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00197;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 236 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00197: ;
	hb_xvmSetLine( 769 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00198;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00198: ;
	hb_xvmSetLine( 772 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 293 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00203;
	hb_xvmSetLine( 773 );
	hb_xvmPushSymbol( symbols + 293 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00202;
lab00199: ;
	hb_xvmSetLine( 774 );
	hb_xvmPushSymbol( symbols + 294 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00200;
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	hb_xvmPushStringConst( "Object", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00201;
lab00200: ;
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Object", 6 );
	if( hb_xvmFunction( 2 ) ) break;
lab00201: ;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 775 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00199;
lab00202: ;
	hb_xvmEnumEnd();
lab00203: ;
	hb_xvmSetLine( 778 );
	hb_xvmPushFuncSymbol( symbols + 295 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 780 );
	hb_xvmPushLocal( 8 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _TBROWSE_BSPECHDENUM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 3 );
	hb_xvmSetLine( 783 );
	hb_xvmLocalSetInt( 6, 0L );
	hb_xvmSetLine( 784 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( "ORDKEYNO", 8 );
	goto lab00002;
lab00001: ;
	hb_xvmPushStringConst( "ARRAYNO", 7 );
lab00002: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 786 );
	hb_xvmPushSymbol( symbols + 150 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 787 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 296 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 3 );
lab00004: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 788 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushStringConst( ".", 1 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 3 );
lab00006: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 789 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
lab00007: ;
	hb_xvmSetLine( 790 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "SELECTOR", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 792 );
	hb_xvmCopyLocals( 3, 5 );
	hb_xvmSetLine( 793 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushSymbol( symbols + 297 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 794 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
lab00008: ;
	hb_xvmSetLine( 796 );
	hb_xvmPushSymbol( symbols + 298 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 797 );
lab00009: ;
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
lab00010: ;
	hb_xvmEnumEnd();
lab00011: ;
	hb_xvmSetLine( 800 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _TBROWSE_BADJCOLUMNS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 2 );
	hb_xvmSetLine( 803 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 804 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( "ORDKEYNO", 8 );
	goto lab00002;
lab00001: ;
	hb_xvmPushStringConst( "ARRAYNO", 7 );
lab00002: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 806 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 299 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 807 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 299 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 808 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmCopyLocals( 5, 6 );
lab00003: ;
	hb_xvmSetLine( 811 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 813 );
	hb_xvmPushFuncSymbol( symbols + 300 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SELECTOR", 8 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 814 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 815 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 816 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 817 );
	hb_xvmPushSymbol( symbols + 297 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 818 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 816 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00007: ;
	hb_xvmSetLine( 822 );
	hb_xvmPushSymbol( symbols + 301 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00008: ;
	hb_xvmSetLine( 825 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _TBROWSE_BINIT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 830 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 302 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 303 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 833 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 304 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 304 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmSetLine( 834 );
	hb_xvmLocalSetInt( 5, 0L );
	hb_xvmSetLine( 835 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 304 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushSymbol( symbols + 304 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmPushSymbol( symbols + 304 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmArrayGen( 1 );
lab00003: ;
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
lab00004: ;
	hb_xvmSetLine( 836 );
	hb_xvmPushFuncSymbol( symbols + 305 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 837 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessThenIntIs( 6L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 182 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 6 );
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 839 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 840 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 841 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
lab00006: ;
	hb_xvmSetLine( 842 );
	goto lab00012;
lab00007: ;
	hb_xvmSetLine( 843 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 844 );
	goto lab00012;
lab00008: ;
	hb_xvmSetLine( 846 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushInteger( 1 );
	goto lab00010;
lab00009: ;
	hb_xvmPushInteger( 0 );
lab00010: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 847 );
	hb_xvmPushSymbol( symbols + 306 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 848 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 849 );
	hb_xvmPushSymbol( symbols + 307 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00011: ;
	hb_xvmSetLine( 851 );
	hb_xvmPushFuncSymbol( symbols + 295 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 852 );
lab00012: ;
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
lab00013: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 853 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00014: ;
	hb_xvmSetLine( 857 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 308 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 858 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushStringConst( "ORDKEYNO", 8 );
	goto lab00016;
lab00015: ;
	hb_xvmPushStringConst( "ARRAYNO", 7 );
lab00016: ;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 859 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00017;
	hb_xvmSetLine( 860 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 308 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 4 ) ) break;
	hb_stackPop();
lab00017: ;
	hb_xvmSetLine( 864 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 309 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 309 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00018: ;
	hb_xvmSetLine( 867 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 222 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 868 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 224 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00019: ;
	hb_xvmSetLine( 871 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _TBROWSE_BBODY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 2 );
	hb_xvmSetLine( 875 );
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 877 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 310 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 310 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 880 );
	hb_xvmPushSymbol( symbols + 311 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushSymbol( symbols + 312 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 5 );
lab00003: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 881 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushSymbol( symbols + 313 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmPushLocal( 5 );
lab00005: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 882 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 883 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocalByRef( 6 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00006: ;
	hb_xvmSetLine( 884 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 885 );
	hb_xvmPushSymbol( symbols + 307 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 295 );
	if( hb_xvmDo( 0 ) ) break;
lab00007: ;
	hb_xvmSetLine( 887 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
lab00008: ;
	hb_xvmEnumEnd();
lab00009: ;
	hb_xvmSetLine( 890 );
	hb_xvmPushSymbol( symbols + 314 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushSymbol( symbols + 315 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00011;
lab00010: ;
	hb_xvmPushLocal( 5 );
lab00011: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 891 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushSymbol( symbols + 316 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00013;
lab00012: ;
	hb_xvmPushLocal( 5 );
lab00013: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 892 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 893 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPopLocal( 5 );
lab00014: ;
	hb_xvmSetLine( 895 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
lab00015: ;
	hb_xvmSetLine( 896 );
	hb_xvmPushFuncSymbol( symbols + 317 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	goto lab00017;
lab00016: ;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
lab00017: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 897 );
	hb_xvmPushFuncSymbol( symbols + 317 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	goto lab00019;
lab00018: ;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
lab00019: ;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 898 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00020;
	hb_xvmPushLocal( 7 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00020;
	hb_xvmPushLocal( 8 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 899 );
	hb_xvmPushSymbol( symbols + 318 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 295 );
	if( hb_xvmDo( 0 ) ) break;
lab00020: ;
	hb_xvmSetLine( 901 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
lab00021: ;
	hb_xvmEnumEnd();
lab00022: ;
	hb_xvmSetLine( 904 );
	hb_xvmPushSymbol( symbols + 319 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmPushSymbol( symbols + 320 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00024;
lab00023: ;
	hb_xvmPushLocal( 5 );
lab00024: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 905 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmPushSymbol( symbols + 321 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00026;
lab00025: ;
	hb_xvmPushLocal( 5 );
lab00026: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 906 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00032;
	hb_xvmSetLine( 907 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 908 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocalByRef( 6 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
lab00027: ;
	hb_xvmSetLine( 909 );
	hb_xvmPushFuncSymbol( symbols + 317 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	goto lab00029;
lab00028: ;
	hb_xvmPushLocal( 6 );
lab00029: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 910 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00030;
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
lab00030: ;
	hb_xvmSetLine( 912 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00027;
lab00031: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 913 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00032;
	hb_xvmPushSymbol( symbols + 322 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 295 );
	if( hb_xvmDo( 0 ) ) break;
lab00032: ;
	hb_xvmSetLine( 917 );
	hb_xvmPushSymbol( symbols + 323 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmPushSymbol( symbols + 324 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00034;
lab00033: ;
	hb_xvmPushLocal( 4 );
lab00034: ;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 918 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00035: ;
	hb_xvmSetLine( 921 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 213 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00043;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 214 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00043;
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 214 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00043;
	hb_xvmSetLine( 922 );
	hb_xvmPushSymbol( symbols + 325 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 923 );
	hb_xvmPushSymbol( symbols + 326 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 924 );
	hb_xvmPushSymbol( symbols + 215 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 155 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 925 );
	hb_xvmPushSymbol( symbols + 327 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushSymbol( symbols + 214 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 926 );
	hb_xvmPushSymbol( symbols + 328 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushSymbol( symbols + 115 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 214 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 927 );
	hb_xvmPushSymbol( symbols + 329 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 928 );
	hb_xvmPushSymbol( symbols + 330 );
	hb_xvmPushSymbol( symbols + 331 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 332 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 333 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 929 );
	hb_xvmPushSymbol( symbols + 218 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushSymbol( symbols + 216 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 930 );
	hb_xvmPushSymbol( symbols + 218 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushSymbol( symbols + 216 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00036;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 12632256 );
#else
	hb_xvmPushLong( 12632256L );
#endif
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmArrayGen( 2 );
	goto lab00037;
lab00036: ;
	hb_xvmPushSymbol( symbols + 216 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
lab00037: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 938 );
	hb_xvmPushSymbol( symbols + 335 );
	hb_xvmPushLocal( 9 );
	{
		static const HB_BYTE codeblock[ 67 ] = {
			4, 0, 0, 0, 36, 164, 3, 100, 36, 165, 3, 176, 78, 1, 12, 0, 
			92, 4, 35, 28, 10, 95, 3, 80, 4, 95, 2, 80, 3, 36, 167, 3, 
			48, 112, 0, 95, 4, 112, 0, 80, 5, 36, 168, 3, 48, 216, 0, 95, 
			5, 112, 0, 48, 77, 1, 95, 5, 112, 0, 1, 80, 1, 36, 169, 3, 
			95, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 939 );
	hb_xvmPushSymbol( symbols + 155 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
	hb_xvmSetLine( 940 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
lab00038: ;
	hb_xvmSetLine( 941 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushSymbol( symbols + 336 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 942 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00038;
lab00039: ;
	hb_xvmEnumEnd();
	goto lab00041;
lab00040: ;
	hb_xvmSetLine( 944 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 214 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 945 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushSymbol( symbols + 336 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00041: ;
	hb_xvmSetLine( 962 );
	{
		static const HB_BYTE codeblock[ 183 ] = {
			1, 0, 0, 0, 36, 180, 3, 48, 112, 0, 95, 1, 112, 0, 36, 181, 
			3, 100, 100, 100, 36, 182, 3, 48, 115, 0, 95, 1, 48, 214, 0, 95, 
			2, 112, 0, 112, 1, 80, 3, 36, 183, 3, 48, 81, 1, 48, 75, 1, 
			95, 2, 112, 0, 95, 3, 121, 112, 2, 80, 4, 36, 184, 3, 95, 4, 
			121, 8, 165, 80, 5, 28, 42, 36, 185, 3, 48, 77, 1, 95, 2, 112, 
			0, 80, 4, 36, 186, 3, 175, 4, 0, 176, 7, 0, 48, 216, 0, 95, 
			2, 112, 0, 12, 1, 15, 28, 5, 122, 25, 4, 95, 4, 80, 4, 36, 
			188, 3, 48, 70, 1, 95, 2, 95, 4, 112, 1, 73, 36, 189, 3, 95, 
			3, 48, 76, 1, 95, 2, 112, 0, 69, 28, 12, 48, 72, 1, 95, 2, 
			95, 3, 112, 1, 73, 36, 191, 3, 95, 5, 28, 24, 48, 74, 1, 48, 
			75, 1, 95, 2, 112, 0, 48, 76, 1, 95, 2, 112, 0, 95, 4, 112, 
			2, 73, 36, 193, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 964 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 338 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00042;
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 338 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00043;
lab00042: ;
	hb_xvmSetLine( 965 );
	hb_xvmPushSymbol( symbols + 339 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00043: ;
	hb_xvmSetLine( 969 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 222 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00044;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00044;
	hb_xvmSetLine( 970 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 224 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00044: ;
	hb_xvmSetLine( 973 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _TBROWSE_BAFTER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 978 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 979 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 988 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushLocal( 4 );
	{
		static const HB_BYTE codeblock[ 70 ] = {
			3, 0, 0, 0, 36, 213, 3, 48, 60, 0, 95, 3, 112, 0, 95, 2, 
			1, 36, 214, 3, 48, 63, 0, 95, 4, 112, 0, 36, 215, 3, 85, 48, 
			206, 0, 95, 3, 112, 0, 74, 176, 207, 0, 12, 0, 119, 28, 19, 36, 
			216, 3, 97, 128, 128, 128, 0, 80, 5, 36, 217, 3, 95, 2, 80, 1, 
			36, 219, 3, 95, 5, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 991 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 276 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 276 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 993 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 277 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 278 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 277 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 995 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00011;
lab00004: ;
	hb_xvmSetLine( 996 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 997 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 340 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushSymbol( symbols + 341 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushSymbol( symbols + 340 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 999 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 342 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushSymbol( symbols + 343 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushSymbol( symbols + 342 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00006: ;
	hb_xvmSetLine( 1001 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 344 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushSymbol( symbols + 345 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushSymbol( symbols + 344 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00007: ;
	hb_xvmSetLine( 1003 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 346 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushSymbol( symbols + 347 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushSymbol( symbols + 346 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00008: ;
	hb_xvmSetLine( 1005 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 348 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushSymbol( symbols + 349 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushSymbol( symbols + 348 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00009: ;
	hb_xvmSetLine( 1007 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 350 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushSymbol( symbols + 351 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushSymbol( symbols + 350 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00010: ;
	hb_xvmSetLine( 995 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00011: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1011 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 352 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 352 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00012: ;
	hb_xvmSetLine( 1014 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 222 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 1015 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 224 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00013: ;
	hb_xvmSetLine( 1018 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _TBROWSE_BEND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 1021 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1023 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 353 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1024 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 353 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1025 );
	hb_xvmCopyLocals( 4, 3 );
lab00001: ;
	hb_xvmSetLine( 1028 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1030 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushSymbol( symbols + 250 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushSymbol( symbols + 354 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1031 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 229 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1032 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 229 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 1035 );
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 355 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1036 );
	hb_xvmPushSymbol( symbols + 288 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 1038 );
	hb_xvmPushSymbol( symbols + 122 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 1039 );
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00004: ;
	hb_xvmSetLine( 1042 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushSymbol( symbols + 222 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 1043 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 223 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 224 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 1046 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

