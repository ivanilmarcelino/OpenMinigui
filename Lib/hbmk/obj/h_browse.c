/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_browse.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEBROWSE );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( AFILL );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( ASIZE );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( GETVSCROLLBARWIDTH );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC( INITDIALOGBROWSE );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( INITBROWSE );
HB_FUNC_EXTERN( ADDSPLITBOXITEM );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( LISTVIEW_SETBKCOLOR );
HB_FUNC_EXTERN( LISTVIEW_SETTEXTBKCOLOR );
HB_FUNC_EXTERN( LISTVIEW_SETTEXTCOLOR );
HB_FUNC( HMG_SETORDER );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( _NOQUOTE );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_EXTERN( MIN );
HB_FUNC_STATIC( HMG_ORDCREATE );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( ADDLISTVIEWBITMAP );
HB_FUNC_EXTERN( MAX );
HB_FUNC_EXTERN( GETBORDERWIDTH );
HB_FUNC_EXTERN( ADDLISTVIEWBITMAPHEADER );
HB_FUNC_EXTERN( INITLISTVIEWCOLUMNS );
HB_FUNC_EXTERN( LISTVIEW_GETCOLUMNWIDTH );
HB_FUNC_EXTERN( SETGRIDCOLUMNHEADERIMAGE );
HB_FUNC_EXTERN( INITVSCROLLBAR );
HB_FUNC_EXTERN( GETHSCROLLBARHEIGHT );
HB_FUNC_EXTERN( INITVSCROLLBARBUTTON );
HB_FUNC( _BROWSEREFRESH );
HB_FUNC_EXTERN( ALIAS );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( ORDLISTCLEAR );
HB_FUNC_EXTERN( ORDCONDSET );
HB_FUNC_EXTERN( RECNO );
HB_FUNC_EXTERN( FIELDPOS );
HB_FUNC_EXTERN( ORDCREATE );
HB_FUNC_EXTERN( STRZERO );
HB_FUNC_EXTERN( RANDOM );
HB_FUNC_EXTERN( FIELDNAME );
HB_FUNC_EXTERN( HB_MACROBLOCK );
HB_FUNC_EXTERN( ORDSETFOCUS );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_STATIC( RESTOREWORKAREA );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC_EXTERN( ORDNUMBER );
HB_FUNC_EXTERN( LISTVIEW_SETSORTHEADER );
HB_FUNC_EXTERN( ORDDESCEND );
HB_FUNC_EXTERN( DBGOTO );
HB_FUNC( _BROWSEUPDATE );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( LISTVIEWGETCOUNTPERPAGE );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( TYPE );
HB_FUNC_EXTERN( RTRIM );
HB_FUNC( _GETBROWSEFIELDVALUE );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( _TEVAL );
HB_FUNC_EXTERN( ADDLISTVIEWITEMS );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DBGOBOTTOM );
HB_FUNC_STATIC( _TYPEEX );
HB_FUNC_EXTERN( TRANSFORM );
HB_FUNC_EXTERN( ROUND );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( INT );
HB_FUNC_EXTERN( RIGHT );
HB_FUNC_EXTERN( HB_STRSHRINK );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( HB_TSTOSTR );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( HB_VALTOSTR );
HB_FUNC( _GETBROWSEFNVALUE );
HB_FUNC_EXTERN( DBSTRUCT );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( UPPER );
HB_FUNC( _BROWSENEXT );
HB_FUNC_EXTERN( LISTVIEW_GETSUBITEMRECT );
HB_FUNC_EXTERN( LISTVIEW_GETFIRSTITEM );
HB_FUNC_STATIC( _BROWSEVSCROLLUPDATE );
HB_FUNC_EXTERN( LISTVIEW_SCROLL );
HB_FUNC_EXTERN( LISTVIEW_SETCURSEL );
HB_FUNC( _BROWSEVSCROLLFASTUPDATE );
HB_FUNC( _BROWSEONCHANGE );
HB_FUNC( _BROWSEPRIOR );
HB_FUNC( _BROWSEHOME );
HB_FUNC( _BROWSEEND );
HB_FUNC( _BROWSEUP );
HB_FUNC( _BROWSEDOWN );
HB_FUNC( _BROWSEGETVALUE );
HB_FUNC_EXTERN( RDDNAME );
HB_FUNC_EXTERN( DBFILTER );
HB_FUNC_EXTERN( INDEXORD );
HB_FUNC_EXTERN( ORDKEYVAL );
HB_FUNC_EXTERN( ORDSCOPE );
HB_FUNC_EXTERN( DELETED );
HB_FUNC( _BROWSESETVALUE );
HB_FUNC_EXTERN( RECCOUNT );
HB_FUNC( _BROWSEDELETE );
HB_FUNC_EXTERN( DBINFO );
HB_FUNC_EXTERN( NETDELETE );
HB_FUNC_EXTERN( DBRUNLOCK );
HB_FUNC_EXTERN( MSGSTOP );
HB_FUNC_EXTERN( DBDELETE );
HB_FUNC( _BROWSEEDIT );
HB_FUNC_STATIC( _BROWSEINPLACEEDIT );
HB_FUNC_STATIC( _BROWSEINPLACEAPPEND );
HB_FUNC_EXTERN( PLAYHAND );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC_EXTERN( HB_UAT );
HB_FUNC_EXTERN( _GETVALUE );
HB_FUNC_EXTERN( NETRECLOCK );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( _GETFOCUSEDCONTROL );
HB_FUNC_EXTERN( GETPARENTFORMNAME );
HB_FUNC_EXTERN( SETFOCUS );
HB_FUNC_EXTERN( GETACTIVEMDIHANDLE );
HB_FUNC_EXTERN( GETACTIVEWINDOW );
HB_FUNC_EXTERN( INPUTBOX );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( DBCOMMIT );
HB_FUNC_EXTERN( _DEFINEMODALWINDOW );
HB_FUNC_EXTERN( _DEFINEHOTKEY );
HB_FUNC_EXTERN( _ISWINDOWACTIVE );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_STATIC( _INPLACEEDITOK );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( DOMETHOD );
HB_FUNC_EXTERN( _DEFINECOMBO );
HB_FUNC_EXTERN( _DEFINETEXTBOX );
HB_FUNC_EXTERN( _DEFINEMASKEDTEXTBOX );
HB_FUNC_EXTERN( _DEFINECHARMASKTEXTBOX );
HB_FUNC_EXTERN( _DEFINEDATEPICK );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( _ENDWINDOW );
HB_FUNC_EXTERN( _SETFOCUS );
HB_FUNC_EXTERN( _ACTIVATEWINDOW );
HB_FUNC_EXTERN( _MDIWINDOWSACTIVATE );
HB_FUNC_EXTERN( INSERTVKEY );
HB_FUNC_STATIC( _INPLACEEDITSAVE );
HB_FUNC_EXTERN( NETERROR );
HB_FUNC( PROCESSINPLACEKBDEDIT );
HB_FUNC_EXTERN( LISTVIEW_GETITEMRECT );
HB_FUNC_STATIC( _BROWSESYNC );
HB_FUNC_EXTERN( _DOCONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( LISTVIEWGETITEMCOUNT );
HB_FUNC_EXTERN( NETAPPEND );
HB_FUNC_EXTERN( ORDKEYCOUNT );
HB_FUNC_EXTERN( ORDKEYNO );
HB_FUNC_EXTERN( SETSCROLLRANGE );
HB_FUNC_EXTERN( SETSCROLLPOS );
HB_FUNC_EXTERN( GETSCROLLPOS );
HB_FUNC( _SETGETBROWSEPROPERTY );
HB_FUNC_STATIC( NETLOCK );
HB_FUNC_EXTERN( SECONDS );
HB_FUNC_EXTERN( RECNO );
HB_FUNC_EXTERN( DBRLOCK );
HB_FUNC_EXTERN( FLOCK );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( NETERR );
HB_FUNC_EXTERN( HMG_SYSWAIT );
HB_FUNC_STATIC( NETMODIFYRECORD );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DBCOMMIT );
HB_FUNC_EXTERN( HMG_ALERT );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC( NETDELETE );
HB_FUNC_EXTERN( DBDELETE );
HB_FUNC( NETRECALL );
HB_FUNC_EXTERN( DBRECALL );
HB_FUNC( NETRECLOCK );
HB_FUNC( NETFILELOCK );
HB_FUNC( NETAPPEND );
HB_FUNC_EXTERN( ORDSETFOCUS );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC( ISLOCKED );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( DBRLOCKLIST );
HB_FUNC( NETERROR );
HB_FUNC( SETNETDELAY );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_BROWSE )
{ "_DEFINEBROWSE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEBROWSE )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "AFILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFILL )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "ASIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASIZE )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "GETVSCROLLBARWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETVSCROLLBARWIDTH )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "INITDIALOGBROWSE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGBROWSE )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "INITBROWSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITBROWSE )}, NULL },
{ "ADDSPLITBOXITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDSPLITBOXITEM )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "LISTVIEW_SETBKCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEW_SETBKCOLOR )}, NULL },
{ "LISTVIEW_SETTEXTBKCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEW_SETTEXTBKCOLOR )}, NULL },
{ "LISTVIEW_SETTEXTCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEW_SETTEXTCOLOR )}, NULL },
{ "HMG_SETORDER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_SETORDER )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "_NOQUOTE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _NOQUOTE )}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "HMG_ORDCREATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_ORDCREATE )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "ADDLISTVIEWBITMAP", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDLISTVIEWBITMAP )}, NULL },
{ "MAX", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAX )}, NULL },
{ "GETBORDERWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBORDERWIDTH )}, NULL },
{ "ADDLISTVIEWBITMAPHEADER", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDLISTVIEWBITMAPHEADER )}, NULL },
{ "INITLISTVIEWCOLUMNS", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITLISTVIEWCOLUMNS )}, NULL },
{ "LISTVIEW_GETCOLUMNWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEW_GETCOLUMNWIDTH )}, NULL },
{ "SETGRIDCOLUMNHEADERIMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETGRIDCOLUMNHEADERIMAGE )}, NULL },
{ "INITVSCROLLBAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITVSCROLLBAR )}, NULL },
{ "GETHSCROLLBARHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETHSCROLLBARHEIGHT )}, NULL },
{ "INITVSCROLLBARBUTTON", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITVSCROLLBARBUTTON )}, NULL },
{ "_BROWSEREFRESH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEREFRESH )}, NULL },
{ "ALIAS", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALIAS )}, NULL },
{ "SELECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SELECT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "ORDLISTCLEAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDLISTCLEAR )}, NULL },
{ "ORDCONDSET", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDCONDSET )}, NULL },
{ "RECNO", {HB_FS_PUBLIC}, {HB_FUNCNAME( RECNO )}, NULL },
{ "FIELDPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDPOS )}, NULL },
{ "ORDCREATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDCREATE )}, NULL },
{ "STRZERO", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRZERO )}, NULL },
{ "RANDOM", {HB_FS_PUBLIC}, {HB_FUNCNAME( RANDOM )}, NULL },
{ "FIELDNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDNAME )}, NULL },
{ "HB_MACROBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_MACROBLOCK )}, NULL },
{ "ORDSETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDSETFOCUS )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "RESTOREWORKAREA", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( RESTOREWORKAREA )}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "ORDNUMBER", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDNUMBER )}, NULL },
{ "LISTVIEW_SETSORTHEADER", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEW_SETSORTHEADER )}, NULL },
{ "ORDDESCEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDDESCEND )}, NULL },
{ "DBGOTO", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTO )}, NULL },
{ "_BROWSEUPDATE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEUPDATE )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "LISTVIEWGETCOUNTPERPAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEWGETCOUNTPERPAGE )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "TYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( TYPE )}, NULL },
{ "__ENUMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "RTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( RTRIM )}, NULL },
{ "_GETBROWSEFIELDVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETBROWSEFIELDVALUE )}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "_TEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _TEVAL )}, NULL },
{ "ADDLISTVIEWITEMS", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDLISTVIEWITEMS )}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "DBGOBOTTOM", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOBOTTOM )}, NULL },
{ "_TYPEEX", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _TYPEEX )}, NULL },
{ "TRANSFORM", {HB_FS_PUBLIC}, {HB_FUNCNAME( TRANSFORM )}, NULL },
{ "ROUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROUND )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "INT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INT )}, NULL },
{ "RIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RIGHT )}, NULL },
{ "HB_STRSHRINK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_STRSHRINK )}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "HB_TSTOSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_TSTOSTR )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "HB_VALTOSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VALTOSTR )}, NULL },
{ "_GETBROWSEFNVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETBROWSEFNVALUE )}, NULL },
{ "DBSTRUCT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSTRUCT )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "_BROWSENEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSENEXT )}, NULL },
{ "LISTVIEW_GETSUBITEMRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEW_GETSUBITEMRECT )}, NULL },
{ "LISTVIEW_GETFIRSTITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEW_GETFIRSTITEM )}, NULL },
{ "_BROWSEVSCROLLUPDATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEVSCROLLUPDATE )}, NULL },
{ "LISTVIEW_SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEW_SCROLL )}, NULL },
{ "LISTVIEW_SETCURSEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEW_SETCURSEL )}, NULL },
{ "_BROWSEVSCROLLFASTUPDATE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEVSCROLLFASTUPDATE )}, NULL },
{ "_BROWSEONCHANGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEONCHANGE )}, NULL },
{ "_BROWSEPRIOR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEPRIOR )}, NULL },
{ "_BROWSEHOME", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEHOME )}, NULL },
{ "_BROWSEEND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEEND )}, NULL },
{ "_BROWSEUP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEUP )}, NULL },
{ "_BROWSEDOWN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEDOWN )}, NULL },
{ "_BROWSEGETVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEGETVALUE )}, NULL },
{ "RDDNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( RDDNAME )}, NULL },
{ "DBFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBFILTER )}, NULL },
{ "INDEXORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( INDEXORD )}, NULL },
{ "ORDKEYVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDKEYVAL )}, NULL },
{ "ORDSCOPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDSCOPE )}, NULL },
{ "DELETED", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELETED )}, NULL },
{ "_BROWSESETVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSESETVALUE )}, NULL },
{ "RECCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RECCOUNT )}, NULL },
{ "_BROWSEDELETE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEDELETE )}, NULL },
{ "DBINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBINFO )}, NULL },
{ "NETDELETE", {HB_FS_PUBLIC}, {HB_FUNCNAME( NETDELETE )}, NULL },
{ "DBRUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBRUNLOCK )}, NULL },
{ "MSGSTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGSTOP )}, NULL },
{ "DBDELETE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBDELETE )}, NULL },
{ "_BROWSEEDIT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEEDIT )}, NULL },
{ "_BROWSEINPLACEEDIT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEINPLACEEDIT )}, NULL },
{ "_BROWSEINPLACEAPPEND", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSEINPLACEAPPEND )}, NULL },
{ "PLAYHAND", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLAYHAND )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "HB_UAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UAT )}, NULL },
{ "_GETVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETVALUE )}, NULL },
{ "NETRECLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( NETRECLOCK )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "_GETFOCUSEDCONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETFOCUSEDCONTROL )}, NULL },
{ "GETPARENTFORMNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPARENTFORMNAME )}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETFOCUS )}, NULL },
{ "GETACTIVEMDIHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETACTIVEMDIHANDLE )}, NULL },
{ "GETACTIVEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETACTIVEWINDOW )}, NULL },
{ "INPUTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INPUTBOX )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "DBCOMMIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCOMMIT )}, NULL },
{ "_DEFINEMODALWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEMODALWINDOW )}, NULL },
{ "_DEFINEHOTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEHOTKEY )}, NULL },
{ "_ISWINDOWACTIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWACTIVE )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "_INPLACEEDITOK", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _INPLACEEDITOK )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "DOMETHOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOMETHOD )}, NULL },
{ "_DEFINECOMBO", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINECOMBO )}, NULL },
{ "_DEFINETEXTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINETEXTBOX )}, NULL },
{ "_DEFINEMASKEDTEXTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEMASKEDTEXTBOX )}, NULL },
{ "_DEFINECHARMASKTEXTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINECHARMASKTEXTBOX )}, NULL },
{ "_DEFINEDATEPICK", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEDATEPICK )}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "_ENDWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDWINDOW )}, NULL },
{ "_SETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFOCUS )}, NULL },
{ "_ACTIVATEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ACTIVATEWINDOW )}, NULL },
{ "_MDIWINDOWSACTIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _MDIWINDOWSACTIVATE )}, NULL },
{ "INSERTVKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSERTVKEY )}, NULL },
{ "_INPLACEEDITSAVE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _INPLACEEDITSAVE )}, NULL },
{ "NETERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( NETERROR )}, NULL },
{ "PROCESSINPLACEKBDEDIT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PROCESSINPLACEKBDEDIT )}, NULL },
{ "LISTVIEW_GETITEMRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEW_GETITEMRECT )}, NULL },
{ "_BROWSESYNC", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BROWSESYNC )}, NULL },
{ "_DOCONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DOCONTROLEVENTPROCEDURE )}, NULL },
{ "LISTVIEWGETITEMCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEWGETITEMCOUNT )}, NULL },
{ "NETAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( NETAPPEND )}, NULL },
{ "ORDKEYCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDKEYCOUNT )}, NULL },
{ "ORDKEYNO", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDKEYNO )}, NULL },
{ "SETSCROLLRANGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETSCROLLRANGE )}, NULL },
{ "SETSCROLLPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETSCROLLPOS )}, NULL },
{ "GETSCROLLPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSCROLLPOS )}, NULL },
{ "_SETGETBROWSEPROPERTY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETGETBROWSEPROPERTY )}, NULL },
{ "NETLOCK", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( NETLOCK )}, NULL },
{ "SECONDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SECONDS )}, NULL },
{ "RECNO", {HB_FS_PUBLIC}, {HB_FUNCNAME( RECNO )}, NULL },
{ "DBRLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBRLOCK )}, NULL },
{ "FLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( FLOCK )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "NETERR", {HB_FS_PUBLIC}, {HB_FUNCNAME( NETERR )}, NULL },
{ "HMG_SYSWAIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_SYSWAIT )}, NULL },
{ "NETMODIFYRECORD", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( NETMODIFYRECORD )}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DBCOMMIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCOMMIT )}, NULL },
{ "HMG_ALERT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_ALERT )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "NETDELETE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( NETDELETE )}, NULL },
{ "DBDELETE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBDELETE )}, NULL },
{ "NETRECALL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( NETRECALL )}, NULL },
{ "DBRECALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBRECALL )}, NULL },
{ "NETRECLOCK", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( NETRECLOCK )}, NULL },
{ "NETFILELOCK", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( NETFILELOCK )}, NULL },
{ "NETAPPEND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( NETAPPEND )}, NULL },
{ "ORDSETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDSETFOCUS )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "ISLOCKED", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ISLOCKED )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "DBRLOCKLIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBRLOCKLIST )}, NULL },
{ "NETERROR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( NETERROR )}, NULL },
{ "SETNETDELAY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETNETDELAY )}, NULL },
{ "(_INITSTATICS00002)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_BROWSE, "h_browse.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_BROWSE
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_BROWSE )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEBROWSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 13, 52 );
	hb_xvmSetLine( 91 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 64 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 65 );
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 65 );
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 55 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 55 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLocalByRef( 25 );
	hb_xvmPushLocalByRef( 26 );
	hb_xvmPushLocalByRef( 27 );
	hb_xvmPushLocalByRef( 28 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 101 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00002: ;
	hb_xvmSetLine( 102 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00004;
lab00003: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00004: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 11 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 107 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 108 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 110 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00006: ;
	hb_xvmSetLine( 113 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 63 );
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmPushLocal( 63 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 2 );
lab00008: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
lab00009: ;
	hb_xvmSetLine( 119 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00010: ;
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushLocal( 63 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 124 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
lab00011: ;
	hb_xvmSetLine( 128 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 129 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 33 );
lab00012: ;
	hb_xvmSetLine( 132 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 135 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 23 );
	goto lab00015;
lab00013: ;
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 139 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00014: ;
	hb_xvmSetLine( 141 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 23 );
	{
		static const HB_BYTE codeblock[ 27 ] = {
			2, 0, 1, 0, 23, 0, 176, 18, 0, 95, 1, 12, 1, 28, 6, 95, 
			1, 25, 3, 121, 165, 95, 255, 95, 2, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00015: ;
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 51 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 51 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 51 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00016: ;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 51 );
	{
		static const HB_BYTE codeblock[ 15 ] = {
			2, 0, 1, 0, 51, 0, 95, 1, 165, 95, 255, 95, 2, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00018;
lab00017: ;
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	hb_xvmPushNil();
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 51 );
lab00018: ;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 44 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushInteger( 0 );
	goto lab00020;
lab00019: ;
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
lab00020: ;
	hb_xvmPopLocal( 59 );
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 161 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 16 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 163 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocalByRef( 45 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 164 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocalByRef( 48 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 165 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 49 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 62 );
	hb_xvmSetLine( 167 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 57 );
	hb_xvmSetLine( 168 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 58 );
	hb_xvmSetLine( 170 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 172 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 53 );
	hb_xvmSetLine( 173 );
	hb_xvmLocalSetInt( 60, 1350565901L );
	hb_xvmSetLine( 175 );
	hb_xvmPushLocal( 45 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00021;
	hb_xvmSetLine( 176 );
	hb_xvmPushLocalByRef( 60 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00021: ;
	hb_xvmSetLine( 179 );
	hb_xvmPushLocal( 63 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 181 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 21, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 56 );
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 43 );
	hb_xvmPushLocal( 58 );
	hb_xvmPushStringConst( "SysListView32", 13 );
	hb_xvmPushLocal( 60 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 59 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 56 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00027;
lab00022: ;
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 53 );
	hb_xvmPushLocal( 43 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 54 );
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushLocal( 60 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 190 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00027;
lab00023: ;
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 53 );
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 240 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00024;
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
lab00024: ;
	hb_xvmSetLine( 206 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 208 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	hb_xvmPushStringConst( "TOOLBAR", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 209 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 29 );
lab00025: ;
	hb_xvmSetLine( 212 );
	hb_xvmPushStringConst( "GRID", 4 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 22L ) ) break;
	hb_xvmSetLine( 214 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 61 );
	hb_xvmSetLine( 216 );
	hb_xvmPushLocal( 61 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00027;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 53 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 59 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 45 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 54 );
	hb_xvmSetLine( 220 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 221 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 223 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 54 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 61 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 232L ) ) break;
	if( hb_xvmDo( 8 ) ) break;
	goto lab00027;
lab00026: ;
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 53 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 59 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 45 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 54 );
lab00027: ;
	hb_xvmSetLine( 235 );
	hb_xvmPushLocal( 63 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00033;
	hb_xvmSetLine( 237 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 238 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 239 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00028: ;
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmSetLine( 243 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00029: ;
	hb_xvmSetLine( 246 );
	hb_xvmPushLocal( 62 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 247 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 16 );
	{
		static const HB_BYTE codeblock[ 29 ] = {
			2, 0, 1, 0, 16, 0, 89, 15, 0, 1, 0, 0, 0, 176, 34, 0, 
			95, 1, 12, 1, 6, 95, 255, 95, 2, 2, 95, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00030: ;
	hb_xvmSetLine( 251 );
	hb_xvmPushLocal( 55 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00031;
	hb_xvmSetLine( 252 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushLocal( 55 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00032;
lab00031: ;
	hb_xvmSetLine( 254 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 11 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 255 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 55 );
lab00032: ;
	hb_xvmSetLine( 259 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmSetLine( 260 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00033: ;
	hb_xvmSetLine( 266 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 57 );
	hb_xvmPushLocal( 58 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 271 );
	hb_xvmPushStringConst( "BROWSE", 6 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 272 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 273 );
	hb_xvmPushLocal( 54 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 274 );
	hb_xvmPushLocal( 53 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 275 );
	hb_xvmPushLocal( 43 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 276 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 277 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 278 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 280 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 282 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 286 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 288 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 292 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 293 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00034;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00035;
lab00034: ;
	hb_xvmPushInteger( -1 );
lab00035: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 294 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00036;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00037;
lab00036: ;
	hb_xvmPushInteger( -1 );
lab00037: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 295 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 296 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 297 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 298 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 299 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 28 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 300 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 301 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 302 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 303 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 304 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 305 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 306 );
	hb_xvmPushLocal( 55 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 307 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 308 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 331 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 40 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 46 );
	hb_xvmPushLocal( 47 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 48 );
	hb_xvmPushLocal( 62 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	goto lab00039;
lab00038: ;
	hb_xvmPushInteger( 0 );
lab00039: ;
	hb_xvmPushLocal( 51 );
	hb_xvmPushLocal( 52 );
	hb_xvmArrayGen( 22 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 332 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 334 );
	hb_xvmPushLocal( 63 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00045;
	hb_xvmSetLine( 336 );
	hb_xvmPushLocal( 62 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00044;
	hb_xvmSetLine( 338 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 340 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 49 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00043;
	hb_xvmSetLine( 342 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 61 );
	goto lab00042;
lab00040: ;
	hb_xvmSetLine( 344 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 49 );
	hb_xvmPushLocal( 61 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00041;
	hb_xvmSetLine( 345 );
	hb_xvmPushLocal( 49 );
	hb_xvmPushLocal( 61 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	hb_xvmPushLocal( 61 );
	if( hb_xvmArrayPop() ) break;
lab00041: ;
	hb_xvmSetLine( 342 );
	if( hb_xvmLocalIncPush( 61 ) ) break;
lab00042: ;
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 49 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 58 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
lab00043: ;
	hb_xvmSetLine( 352 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 58 );
	if( hb_xvmDo( 1 ) ) break;
lab00044: ;
	hb_xvmSetLine( 356 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushLocal( 58 );
	if( hb_xvmDo( 3 ) ) break;
lab00045: ;
	hb_xvmSetLine( 360 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
	hb_xvmSetLine( 361 );
	hb_xvmPushSymbol( symbols + 44 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 58 );
	hb_xvmPushLocal( 57 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 364 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 53 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 65 );
	hb_xvmSetLine( 365 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 54 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 64 );
lab00046: ;
	hb_xvmSetLine( 369 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 50 );
	hb_xvmPushLocal( 58 );
	hb_xvmPushLocal( 65 );
	hb_xvmPushLocal( 64 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 371 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGBROWSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 14, 3 );
	hb_xvmSetLine( 379 );
	hb_xvmLocalSetInt( 10, 0L );
	hb_xvmSetLine( 383 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 384 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 385 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 386 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 388 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 389 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 390 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 391 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 392 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 395 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 4150 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 1 );
lab00002: ;
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	goto lab00004;
lab00003: ;
	hb_xvmPushInteger( 0 );
lab00004: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmAddInt( 1072L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 397 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmPushInteger( 0 );
lab00006: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 398 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 400 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 401 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 15L ) ) break;
lab00007: ;
	hb_xvmSetLine( 404 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 407 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 409 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 17 );
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 410 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 411 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 409 );
	if( hb_xvmLocalIncPush( 17 ) ) break;
lab00009: ;
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 414 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 415 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 17 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 416 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmEqualInt( 1L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 415 );
	if( hb_xvmLocalIncPush( 17 ) ) break;
lab00011: ;
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
lab00012: ;
	hb_xvmSetLine( 421 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	hb_xvmSetLine( 423 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmAddInt( -4L ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 424 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 15 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 16 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 425 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 57 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 57 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00014;
lab00013: ;
	hb_xvmSetLine( 427 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 15 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 428 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 57 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 6 );
lab00014: ;
	hb_xvmSetLine( 431 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 432 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00016;
lab00015: ;
	hb_xvmSetLine( 437 );
	hb_xvmLocalSetInt( 5, 0L );
	hb_xvmSetLine( 439 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 440 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00016: ;
	hb_xvmSetLine( 445 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 446 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 448 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 450 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00017;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 451 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00017: ;
	hb_xvmSetLine( 454 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMG_ORDCREATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 1 );
	hb_xvmSetLine( 466 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 467 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 469 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 470 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 473 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 475 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 477 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 479 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
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
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 20 ) ) break;
	hb_xvmSetLine( 481 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 483 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
lab00003: ;
	hb_xvmSetLine( 484 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Bag", 3 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushFuncSymbol( symbols + 69 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 999999 );
#else
	hb_xvmPushLong( 999999L );
#endif
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 6 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Field->", 7 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 3 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 486 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 487 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "->", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmLocalAdd( 7 );
	hb_xvmSetLine( 490 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 5 ) ) break;
lab00005: ;
	hb_xvmSetLine( 481 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 498 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 499 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 501 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 503 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( RESTOREWORKAREA )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 508 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 509 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 511 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPopAlias() ) break;
lab00002: ;
	hb_xvmSetLine( 514 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_SETORDER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 2 );
	hb_xvmSetLine( 519 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Name", 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 520 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 75 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	hb_xvmPushStringConst( "Name", 4 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushFuncSymbol( symbols + 75 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Name", 4 );
	if( hb_xvmFunction( 2 ) ) break;
lab00002: ;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 526 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 528 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
lab00003: ;
	hb_xvmSetLine( 529 );
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 532 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 534 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 535 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 537 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 538 );
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 541 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 543 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 544 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 546 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 548 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 550 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 551 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushFuncSymbol( symbols + 79 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	goto lab00007;
lab00006: ;
	hb_xvmPushLogical( HB_FALSE );
lab00007: ;
	hb_xvmPopLocal( 2 );
lab00008: ;
	hb_xvmSetLine( 554 );
	hb_xvmCopyLocals( 1, 8 );
	hb_xvmSetLine( 556 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushInteger( -1 );
	goto lab00010;
lab00009: ;
	hb_xvmPushInteger( 1 );
lab00010: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 558 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 559 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 561 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 563 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 565 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 3 ) ) break;
lab00011: ;
	hb_xvmSetLine( 569 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEUPDATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 25, 3 );
	hb_xvmSetLine( 575 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 587 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 589 );
	if( hb_xvmPushAlias() ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 590 );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 593 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 595 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 14L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 597 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 599 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 600 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 602 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 603 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 605 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 607 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 27 );
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 609 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 610 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 611 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
lab00005: ;
	hb_xvmSetLine( 607 );
	if( hb_xvmLocalIncPush( 27 ) ) break;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00007: ;
	hb_xvmSetLine( 618 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 619 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 621 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 622 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 624 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 626 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmPushInteger( 1 );
	goto lab00009;
lab00008: ;
	hb_xvmPushInteger( 2 );
lab00009: ;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 628 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 4105 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 630 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 632 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 633 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 634 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 21 );
lab00010: ;
	hb_xvmSetLine( 637 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 638 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 639 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 19 );
lab00011: ;
	hb_xvmSetLine( 642 );
	hb_xvmPushLocal( 23 );
	hb_xvmArrayDim( 1 );
	hb_xvmPushLocalByRef( 25 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00044;
lab00012: ;
	hb_xvmSetLine( 644 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 646 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00026;
	hb_xvmSetLine( 647 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 649 );
	goto lab00023;
lab00013: ;
	hb_xvmSetLine( 652 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 12 );
	goto lab00024;
lab00014: ;
	hb_xvmSetLine( 655 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmMacroPush( 43 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushInteger( 1 );
	goto lab00016;
lab00015: ;
	hb_xvmPushInteger( 0 );
lab00016: ;
	hb_xvmPopLocal( 12 );
	goto lab00024;
lab00017: ;
	hb_xvmSetLine( 658 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	if( hb_xvmMacroFunc( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmPushLocal( 9 );
	if( hb_xvmMacroPush( 43 ) ) break;
	goto lab00021;
lab00018: ;
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	if( hb_xvmMacroFunc( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushLocal( 9 );
	if( hb_xvmMacroPush( 43 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushInteger( 1 );
	goto lab00021;
lab00019: ;
	hb_xvmPushInteger( 0 );
	goto lab00021;
lab00020: ;
	hb_xvmPushInteger( 0 );
lab00021: ;
	hb_xvmPopLocal( 12 );
	goto lab00024;
lab00022: ;
	hb_xvmSetLine( 661 );
	hb_xvmLocalSetInt( 12, 0L );
	goto lab00024;
lab00023: ;
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00013;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "L", 1 ) )
		{
			hb_stackPop();
			goto lab00014;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "U", 1 ) )
		{
			hb_stackPop();
			goto lab00017;
		}
		{
			hb_stackPop();
			goto lab00022;
		}
	}
lab00024: ;
	hb_xvmSetLine( 665 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 667 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 669 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 670 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 2 ) ) break;
lab00025: ;
	hb_xvmSetLine( 675 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 677 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 678 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 2 ) ) break;
lab00026: ;
	hb_xvmSetLine( 685 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
lab00027: ;
	hb_xvmSetLine( 687 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 689 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmGreaterEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmSetLine( 691 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmSetLine( 692 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 693 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 27 );
	goto lab00030;
lab00028: ;
	hb_xvmSetLine( 694 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmMacroPush( 43 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmSetLine( 695 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 696 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 22 );
	goto lab00031;
lab00029: ;
	hb_xvmSetLine( 693 );
	if( hb_xvmLocalIncPush( 27 ) ) break;
lab00030: ;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
lab00031: ;
	hb_xvmSetLine( 701 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00033;
	hb_xvmSetLine( 702 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00033;
lab00032: ;
	hb_xvmSetLine( 705 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00033: ;
	hb_xvmSetLine( 708 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00036;
	hb_xvmSetLine( 710 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00036;
	hb_xvmSetLine( 711 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmPushFuncSymbol( symbols + 91 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00035;
lab00034: ;
	hb_xvmPushInteger( -1 );
lab00035: ;
	if( hb_xvmDo( 2 ) ) break;
lab00036: ;
	hb_xvmSetLine( 716 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmSetLine( 718 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmSetLine( 719 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmPushFuncSymbol( symbols + 91 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00038;
lab00037: ;
	hb_xvmPushInteger( -1 );
lab00038: ;
	if( hb_xvmDo( 2 ) ) break;
lab00039: ;
	hb_xvmSetLine( 726 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00027;
lab00040: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 728 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 730 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 732 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 734 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00041;
	hb_xvmSetLine( 735 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 736 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 21 );
lab00041: ;
	hb_xvmSetLine( 739 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00042;
	hb_xvmSetLine( 740 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 741 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 19 );
lab00042: ;
	hb_xvmSetLine( 744 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 746 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00043;
	hb_xvmSetLine( 747 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 748 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 749 );
	goto lab00044;
lab00043: ;
	hb_xvmSetLine( 752 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
lab00044: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 754 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00045;
	hb_xvmSetLine( 755 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 17L ) ) break;
lab00045: ;
	hb_xvmSetLine( 758 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
	hb_xvmSetLine( 759 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 18L ) ) break;
lab00046: ;
	hb_xvmSetLine( 762 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 764 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETBROWSEFIELDVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 769 );
	hb_xvmPushStringConst( "Nil", 3 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 770 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 771 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 773 );
	goto lab00020;
lab00001: ;
	hb_xvmSetLine( 780 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmMacroFunc( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00021;
lab00002: ;
	hb_xvmSetLine( 783 );
	hb_xvmPushFuncSymbol( symbols + 98 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmMacroFunc( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 784 );
	hb_xvmPushFuncSymbol( symbols + 100 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 3 );
lab00004: ;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
lab00005: ;
	hb_xvmSetLine( 785 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 786 );
	hb_xvmPushFuncSymbol( symbols + 103 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00005;
lab00006: ;
	goto lab00021;
lab00007: ;
	hb_xvmSetLine( 790 );
	hb_xvmPushFuncSymbol( symbols + 104 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	if( hb_xvmMacroFunc( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00021;
lab00008: ;
	hb_xvmSetLine( 793 );
	hb_xvmPushFuncSymbol( symbols + 105 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushInteger( 1 );
	if( hb_xvmMacroFunc( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00021;
lab00009: ;
	hb_xvmSetLine( 796 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	if( hb_xvmMacroFunc( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00021;
lab00010: ;
	hb_xvmSetLine( 799 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPush( 43 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushStringConst( ".T.", 3 );
	goto lab00012;
lab00011: ;
	hb_xvmPushStringConst( ".F.", 3 );
lab00012: ;
	hb_xvmPopLocal( 3 );
	goto lab00021;
lab00013: ;
	hb_xvmSetLine( 802 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	if( hb_xvmMacroFunc( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushStringConst( "<memo>", 6 );
	goto lab00015;
lab00014: ;
	hb_xvmPushStringConst( "<Memo>", 6 );
lab00015: ;
	hb_xvmPopLocal( 3 );
	goto lab00021;
lab00016: ;
	hb_xvmSetLine( 806 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushFuncSymbol( symbols + 107 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	if( hb_xvmMacroFunc( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00021;
lab00017: ;
	hb_xvmSetLine( 809 );
	hb_xvmPushStringConst( "<General>", 9 );
	hb_xvmPopLocal( 3 );
	goto lab00021;
lab00018: ;
	hb_xvmSetLine( 812 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "UE", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 813 );
	hb_xvmPushStringConst( "<R-Next>", 8 );
	hb_xvmPopLocal( 3 );
	goto lab00021;
lab00019: ;
	hb_xvmSetLine( 814 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "UI", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 815 );
	hb_xvmPushFuncSymbol( symbols + 108 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00021;
lab00020: ;
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "+", 1 ) )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "F", 1 ) )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "I", 1 ) )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "Y", 1 ) )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "B", 1 ) )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "D", 1 ) )
		{
			hb_stackPop();
			goto lab00007;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "T", 1 ) )
		{
			hb_stackPop();
			goto lab00008;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "C", 1 ) )
		{
			hb_stackPop();
			goto lab00009;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "L", 1 ) )
		{
			hb_stackPop();
			goto lab00010;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "M", 1 ) )
		{
			hb_stackPop();
			goto lab00013;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "V", 1 ) )
		{
			hb_stackPop();
			goto lab00016;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "@", 1 ) )
		{
			hb_stackPop();
			goto lab00016;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "G", 1 ) )
		{
			hb_stackPop();
			goto lab00017;
		}
		{
			hb_stackPop();
			goto lab00018;
		}
	}
lab00021: ;
	hb_xvmSetLine( 820 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETBROWSEFNVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 825 );
	hb_xvmPushStringConst( "Nil", 3 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 827 );
	goto lab00008;
lab00001: ;
	hb_xvmSetLine( 830 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmMacroFunc( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00009;
lab00002: ;
	hb_xvmSetLine( 833 );
	hb_xvmPushFuncSymbol( symbols + 104 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	if( hb_xvmMacroFunc( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00009;
lab00003: ;
	hb_xvmSetLine( 836 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPush( 43 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushStringConst( ".T.", 3 );
	goto lab00005;
lab00004: ;
	hb_xvmPushStringConst( ".F.", 3 );
lab00005: ;
	hb_xvmPopLocal( 3 );
	goto lab00009;
lab00006: ;
	hb_xvmSetLine( 839 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	if( hb_xvmMacroFunc( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00009;
lab00007: ;
	hb_xvmSetLine( 842 );
	hb_xvmPushStringConst( "<Memo>", 6 );
	hb_xvmPopLocal( 3 );
	goto lab00009;
lab00008: ;
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushLocal( 1 );
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
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "D", 1 ) )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "L", 1 ) )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "C", 1 ) )
		{
			hb_stackPop();
			goto lab00006;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "M", 1 ) )
		{
			hb_stackPop();
			goto lab00007;
		}
		hb_stackPop();
	}
lab00009: ;
	hb_xvmSetLine( 846 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _TYPEEX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 851 );
	hb_xvmPushInteger( 2 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 856 );
	hb_xvmPushFuncSymbol( symbols + 109 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 858 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 859 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	hb_xvmPushLocal( 3 );
	{
		static const HB_BYTE codeblock[ 30 ] = {
			1, 0, 2, 0, 4, 0, 1, 0, 95, 255, 106, 3, 45, 62, 0, 72, 
			95, 1, 122, 1, 72, 176, 111, 0, 95, 254, 12, 1, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 861 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	hb_xvmPushLocal( 3 );
	{
		static const HB_BYTE codeblock[ 19 ] = {
			1, 0, 1, 0, 1, 0, 95, 1, 122, 1, 176, 111, 0, 95, 255, 12, 
			1, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
lab00002: ;
	hb_xvmSetLine( 864 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
lab00004: ;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 865 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 5 );
lab00006: ;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 867 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSENEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 3 );
	hb_xvmSetLine( 876 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 878 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 879 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 881 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 883 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 885 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 887 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 889 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 890 );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 893 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 894 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 895 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 896 );
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 898 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 899 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 901 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 903 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 904 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 906 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( -1L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 907 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 909 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 910 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 914 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 915 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 919 );
	hb_xvmPushFuncSymbol( symbols + 119 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 921 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEPRIOR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 3 );
	hb_xvmSetLine( 929 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 931 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 932 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 934 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 936 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 938 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 939 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 940 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 941 );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 943 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 944 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 946 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 947 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNegate() ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 949 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 950 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 952 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( -1L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 954 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 955 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 959 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 963 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 965 );
	hb_xvmPushFuncSymbol( symbols + 119 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 967 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEHOME )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 3 );
	hb_xvmSetLine( 975 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 977 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 978 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 980 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 981 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 983 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 984 );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 987 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 989 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 990 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 992 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 993 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 995 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( -1L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 997 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 998 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1000 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1002 );
	hb_xvmPushFuncSymbol( symbols + 119 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1004 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEEND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 3 );
	hb_xvmSetLine( 1012 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 1014 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1015 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1017 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1018 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1020 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 1021 );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 1024 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1026 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1027 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1028 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1030 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1031 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNegate() ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1033 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1034 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( -1L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1036 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1037 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1039 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1041 );
	hb_xvmPushFuncSymbol( symbols + 119 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1043 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEUP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 3 );
	hb_xvmSetLine( 1051 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 1053 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1054 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1056 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1058 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 1060 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 1061 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1062 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1064 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 1065 );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 1068 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1069 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1071 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1072 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1074 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 1075 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1076 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1077 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( -1L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
lab00004: ;
	hb_xvmSetLine( 1080 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1081 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1083 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00007;
lab00005: ;
	hb_xvmSetLine( 1087 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 75L ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 1088 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 1090 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 1094 );
	hb_xvmPushFuncSymbol( symbols + 119 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1096 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 15L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 1097 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 2 ) ) break;
lab00008: ;
	hb_xvmSetLine( 1100 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEDOWN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 3 );
	hb_xvmSetLine( 1109 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 1111 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1112 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1114 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1116 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 1118 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 1120 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 1122 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 1123 );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 1126 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1127 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1129 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 1130 );
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 1133 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1134 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1136 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1137 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1139 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1140 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1142 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( -1L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1144 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1145 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1147 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00007;
lab00005: ;
	hb_xvmSetLine( 1151 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 75L ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 1152 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmInc() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 1154 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 1158 );
	hb_xvmPushFuncSymbol( symbols + 119 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1160 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 15L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 1161 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 2 ) ) break;
lab00008: ;
	hb_xvmSetLine( 1164 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEREFRESH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 3 );
	hb_xvmSetLine( 1172 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1174 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 1176 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1177 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1179 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 1181 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1182 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1184 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 1185 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 4105 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1186 );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 1189 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1190 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1192 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmLessEqualThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 1193 );
	hb_xvmCopyLocals( 5, 11 );
lab00004: ;
	hb_xvmSetLine( 1196 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1198 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushLocal( 10 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
lab00005: ;
	hb_xvmSetLine( 1200 );
	if( hb_xvmPushAlias() ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "SQLRDD", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "SQLEX", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 1201 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1202 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 1206 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 1210 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	if( hb_xvmDo( 0 ) ) break;
lab00008: ;
	hb_xvmSetLine( 1214 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 1215 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushFuncSymbol( symbols + 130 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
lab00009: ;
	hb_xvmSetLine( 1216 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	if( hb_xvmDo( 0 ) ) break;
lab00010: ;
	hb_xvmSetLine( 1220 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushInteger( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 1221 );
	hb_xvmPushFuncSymbol( symbols + 131 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 1222 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	if( hb_xvmDo( 0 ) ) break;
lab00011: ;
	hb_xvmSetLine( 1228 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 1230 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 4105 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1232 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1233 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1235 );
	/* *** END PROC *** */
	break;
lab00012: ;
	hb_xvmSetLine( 1239 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1241 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 1242 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmNegate() ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00013: ;
	hb_xvmSetLine( 1245 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1247 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( -1L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1248 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1250 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1251 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1253 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSESETVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 5 );
	hb_xvmSetLine( 1261 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmLessEqualThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 1262 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 1265 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 4 );
lab00003: ;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 1267 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 242L ) ) break;
	hb_xvmPushStringConst( "BROWSE_ONCHANGE", 15 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1268 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 217L ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1269 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "BROWSE: Value property can't be changed inside ONCHANGE event.", 62 );
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 1273 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1274 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1276 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 1277 );
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 1280 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1282 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 133 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 1283 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1284 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 4105 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1285 );
	hb_xvmPushFuncSymbol( symbols + 119 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1286 );
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 1289 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 133 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 1290 );
	/* *** END PROC *** */
	break;
lab00007: ;
	hb_xvmSetLine( 1293 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmPushLocal( 5 );
lab00009: ;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 1295 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 1297 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1299 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1301 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1303 );
	if( hb_xvmPushAlias() ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "SQLRDD", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "SQLEX", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
lab00010: ;
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 1304 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1305 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 1306 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1307 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1309 );
	/* *** END PROC *** */
	break;
lab00011: ;
	hb_xvmSetLine( 1315 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 1319 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1320 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1322 );
	/* *** END PROC *** */
	break;
lab00012: ;
	hb_xvmSetLine( 1327 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 1329 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1330 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1332 );
	/* *** END PROC *** */
	break;
lab00013: ;
	hb_xvmSetLine( 1336 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmLessThenIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmSetLine( 1337 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 1 ) ) break;
lab00014: ;
	hb_xvmSetLine( 1340 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmNegate() ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1344 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1345 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1346 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1347 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1349 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( -1L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1350 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1352 );
	hb_xvmPushStringConst( "BROWSE_ONCHANGE", 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 242L ) ) break;
	hb_xvmSetLine( 1353 );
	hb_xvmPushFuncSymbol( symbols + 119 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1354 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 242L ) ) break;
	hb_xvmSetLine( 1356 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEGETVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 1364 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1366 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1368 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 1369 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 1372 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1374 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 1375 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 1378 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEDELETE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 3 );
	hb_xvmSetLine( 1387 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 1389 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 1390 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 1393 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1395 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1397 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 1398 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 1401 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1402 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1403 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1405 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 1406 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 1409 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1410 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1412 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 135 );
	hb_xvmPushInteger( 36 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	if( hb_xvmPushAlias() ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "SQLRDD", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "SQLEX", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
lab00006: ;
	hb_xvmSetLine( 1413 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 8 );
lab00007: ;
	hb_xvmSetLine( 1416 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1418 );
	hb_xvmPushFuncSymbol( symbols + 131 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 1420 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 1422 );
	hb_xvmPushFuncSymbol( symbols + 136 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 1423 );
	hb_xvmPushFuncSymbol( symbols + 137 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1424 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1426 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 1427 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00012;
lab00008: ;
	hb_xvmSetLine( 1428 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushInteger( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 1429 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00012;
lab00009: ;
	hb_xvmSetLine( 1432 );
	hb_xvmPushFuncSymbol( symbols + 138 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 261L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00012;
lab00010: ;
	hb_xvmSetLine( 1437 );
	hb_xvmPushFuncSymbol( symbols + 139 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1438 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1440 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 1441 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00012;
lab00011: ;
	hb_xvmSetLine( 1442 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushInteger( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 1443 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 1 ) ) break;
lab00012: ;
	hb_xvmSetLine( 1448 );
	hb_xvmPushFuncSymbol( symbols + 132 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 5 ) ) break;
lab00013: ;
	hb_xvmSetLine( 1452 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1453 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1455 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEEDIT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 8 );
	hb_xvmSetLine( 1468 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1470 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 1471 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1472 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 1473 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 1478 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 1479 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1481 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 135 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 135 );
	hb_xvmPushInteger( 129 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1482 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 1485 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
lab00003: ;
	hb_xvmSetLine( 1486 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 7 ) ) break;
lab00004: ;
	hb_xvmSetLine( 1700 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _BROWSEINPLACEEDIT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 26, 7 );
	hb_xvmSetLine( 1994 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 1996 );
	hb_xvmLocalSetInt( 24, 0L );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 29 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 30 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 1997 );
	hb_xvmPushStringConst( "N", 1 );
	hb_xvmPushStringConst( "C", 1 );
	hb_xvmPushStringConst( "D", 1 );
	hb_xvmPushStringConst( "L", 1 );
	hb_xvmPushStringConst( "M", 1 );
	hb_xvmArrayGen( 5 );
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 2000 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 242L ) ) break;
	hb_xvmPushStringConst( "BROWSE_WHEN", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 2001 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "BROWSE: Editing within WHEN event procedure is not allowed.", 59 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 2003 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 242L ) ) break;
	hb_xvmPushStringConst( "BROWSE_VALID", 12 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 2004 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "BROWSE: Editing within VALID event procedure is not allowed.", 60 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 2007 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 2008 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 2009 );
	hb_xvmPushFuncSymbol( symbols + 142 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 2010 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 2013 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 223L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 2014 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 2017 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 2019 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 2021 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 2023 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 224L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 2025 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushLocal( 18 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
lab00005: ;
	hb_xvmSetLine( 2026 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 2029 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmPushLocal( 18 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 2030 );
	hb_xvmPushFuncSymbol( symbols + 143 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 2031 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00007: ;
	hb_xvmSetLine( 2034 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 2036 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 2038 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmGreaterEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 2040 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 2041 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 29 );
lab00008: ;
	hb_xvmSetLine( 2048 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 2050 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmGreaterEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 2052 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 2054 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 2055 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 238L ) ) break;
	hb_xvmSetLine( 2056 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmSetLine( 2065 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 2067 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 2068 );
	hb_xvmPushFuncSymbol( symbols + 144 );
	hb_xvmPushStringConst( "Edit of this field is not supported.", 36 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 2069 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00010: ;
	hb_xvmSetLine( 2072 );
	hb_xvmPushFuncSymbol( symbols + 145 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 27 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 2073 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 2074 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 2075 );
	hb_xvmPushFuncSymbol( symbols + 111 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "FIELD", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmPushFuncSymbol( symbols + 111 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "_FIELD", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 2076 );
	hb_xvmCopyLocals( 28, 12 );
	goto lab00012;
lab00011: ;
	hb_xvmSetLine( 2079 );
	hb_xvmCopyLocals( 16, 22 );
lab00012: ;
	hb_xvmSetLine( 2083 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 2084 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00013: ;
	hb_xvmSetLine( 2088 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 2090 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2092 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 2094 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 2095 );
	hb_xvmPushFuncSymbol( symbols + 146 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 2096 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 1 ) ) break;
lab00014: ;
	hb_xvmSetLine( 2099 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 135 );
	hb_xvmPushInteger( 36 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	if( hb_xvmPushAlias() ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "SQLRDD", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "SQLEX", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
lab00015: ;
	hb_xvmSetLine( 2100 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 5 );
lab00016: ;
	hb_xvmSetLine( 2104 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 2106 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 147 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_xvmSetLine( 2107 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 238L ) ) break;
	hb_xvmSetLine( 2108 );
	hb_xvmPushFuncSymbol( symbols + 144 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 2110 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2112 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2113 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00017: ;
	hb_xvmSetLine( 2118 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 2120 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 2122 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 2124 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 2125 );
	hb_xvmPushStringConst( "BROWSE_WHEN", 11 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 242L ) ) break;
	hb_xvmSetLine( 2126 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 2127 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 242L ) ) break;
	hb_xvmSetLine( 2128 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmSetLine( 2129 );
	hb_xvmPushFuncSymbol( symbols + 143 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 2131 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2133 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2134 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 238L ) ) break;
	hb_xvmSetLine( 2135 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00018: ;
	hb_xvmSetLine( 2137 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 2138 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 1 ) ) break;
lab00019: ;
	hb_xvmSetLine( 2140 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 2141 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 1 ) ) break;
lab00020: ;
	hb_xvmSetLine( 2143 );
	hb_xvmPushFuncSymbol( symbols + 148 );
	hb_xvmPushFuncSymbol( symbols + 149 );
	hb_xvmPushFuncSymbol( symbols + 150 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 2144 );
	hb_xvmPushFuncSymbol( symbols + 151 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00021: ;
	hb_xvmSetLine( 2152 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 2154 );
	hb_xvmPushFuncSymbol( symbols + 109 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 2156 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 27 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 2157 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 2158 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 21 );
lab00022: ;
	hb_xvmSetLine( 2161 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 2162 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 2164 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 2166 );
	hb_xvmPushStringConst( "X", 1 );
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 2167 );
	hb_xvmLocalSetInt( 24, 1L );
	goto lab00030;
lab00023: ;
	hb_xvmSetLine( 2171 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 2172 );
	goto lab00029;
lab00024: ;
	hb_xvmSetLine( 2177 );
	hb_xvmCopyLocals( 28, 23 );
	goto lab00030;
lab00025: ;
	hb_xvmSetLine( 2180 );
	hb_xvmCopyLocals( 28, 23 );
	hb_xvmSetLine( 2181 );
	hb_xvmLocalSetInt( 24, 1L );
	goto lab00030;
lab00026: ;
	hb_xvmSetLine( 2184 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmPushStringConst( "I", 1 );
	goto lab00028;
lab00027: ;
	hb_xvmPushStringConst( "F", 1 );
lab00028: ;
	hb_xvmPopLocal( 23 );
	goto lab00030;
lab00029: ;
	hb_xvmPushLocal( 28 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "C", 1 ) )
		{
			hb_stackPop();
			goto lab00024;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "D", 1 ) )
		{
			hb_stackPop();
			goto lab00024;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "M", 1 ) )
		{
			hb_stackPop();
			goto lab00024;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "L", 1 ) )
		{
			hb_stackPop();
			goto lab00025;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00026;
		}
		hb_stackPop();
	}
lab00030: ;
	hb_xvmSetLine( 2190 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmPushFuncSymbol( symbols + 152 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00032;
lab00031: ;
	hb_xvmPushFuncSymbol( symbols + 153 );
	if( hb_xvmFunction( 0 ) ) break;
lab00032: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 178L ) ) break;
	hb_xvmSetLine( 2192 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushStringConst( "M", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmSetLine( 2194 );
	hb_xvmPushFuncSymbol( symbols + 154 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushFuncSymbol( symbols + 155 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( "\x8D", 1 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 2196 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 237L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmSetLine( 2197 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 238L ) ) break;
	goto lab00034;
lab00033: ;
	hb_xvmSetLine( 2199 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushStringConst( "FIELD", 5 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmMacroPopAliased( 43 ) ) break;
	hb_xvmSetLine( 2200 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 238L ) ) break;
lab00034: ;
	hb_xvmSetLine( 2203 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00061;
	hb_xvmSetLine( 2204 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 156 );
	if( hb_xvmDo( 0 ) ) break;
	if( hb_xvmPopAlias() ) break;
	hb_xvmSetLine( 2205 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 137 );
	if( hb_xvmDo( 0 ) ) break;
	if( hb_xvmPopAlias() ) break;
	goto lab00061;
lab00035: ;
	hb_xvmSetLine( 2216 );
	hb_xvmPushFuncSymbol( symbols + 157 );
	hb_xvmPushStringConst( "_InPlaceEdit", 12 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 226L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmAddInt( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 225L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 227L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 228L ) ) break;
	if( hb_xvmAddInt( 6L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
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
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 43 ) ) break;
	hb_xvmSetLine( 2219 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 85 );
	{
		static const HB_BYTE codeblock[ 96 ] = {
			0, 0, 2, 0, 23, 0, 17, 0, 176, 159, 0, 106, 13, 95, 73, 110, 
			80, 108, 97, 99, 101, 69, 100, 105, 116, 0, 12, 1, 28, 66, 176, 160, 
			0, 106, 13, 95, 73, 110, 80, 108, 97, 99, 101, 69, 100, 105, 116, 0, 
			106, 10, 67, 111, 110, 116, 114, 111, 108, 95, 49, 0, 106, 6, 118, 97, 
			108, 117, 101, 0, 95, 255, 106, 2, 76, 0, 8, 28, 13, 95, 254, 28, 
			5, 122, 25, 8, 92, 2, 25, 4, 95, 254, 12, 4, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 2222 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 13 );
	{
		static const HB_BYTE codeblock[ 111 ] = {
			0, 0, 9, 0, 10, 0, 2, 0, 18, 0, 22, 0, 12, 0, 3, 0, 
			5, 0, 23, 0, 7, 0, 176, 159, 0, 106, 13, 95, 73, 110, 80, 108, 
			97, 99, 101, 69, 100, 105, 116, 0, 12, 1, 28, 67, 176, 161, 0, 95, 
			255, 176, 75, 0, 106, 13, 95, 73, 110, 80, 108, 97, 99, 101, 69, 100, 
			105, 116, 0, 106, 10, 67, 111, 110, 116, 114, 111, 108, 95, 49, 0, 106, 
			6, 118, 97, 108, 117, 101, 0, 12, 3, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 12, 10, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 2224 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 27 );
	{
		static const HB_BYTE codeblock[ 79 ] = {
			0, 0, 1, 0, 5, 0, 120, 98, 4, 0, 93, 238, 0, 2, 95, 255, 
			28, 7, 176, 162, 0, 20, 0, 176, 159, 0, 106, 13, 95, 73, 110, 80, 
			108, 97, 99, 101, 69, 100, 105, 116, 0, 12, 1, 28, 34, 176, 163, 0, 
			106, 13, 95, 73, 110, 80, 108, 97, 99, 101, 69, 100, 105, 116, 0, 106, 
			8, 82, 69, 76, 69, 65, 83, 69, 0, 12, 2, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 2226 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmSetLine( 2229 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 18 ] = {
			1, 0, 1, 0, 30, 0, 176, 22, 0, 95, 255, 95, 1, 122, 1, 12, 
			2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 2231 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 19 ] = {
			1, 0, 1, 0, 31, 0, 176, 22, 0, 95, 255, 95, 1, 92, 2, 1, 
			12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 2233 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 2235 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 89L ) ) break;
	hb_xvmPushStringConst( "Control_1", 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 337L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 365L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 378L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 379L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 338L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 301L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 302L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 344L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 290L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 291L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 292L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 381L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 373L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 403L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 370L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 371L ) ) break;
	hb_xvmSetLine( 2236 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 2237 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 2238 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 337L ) ) break;
	hb_xvmSetLine( 2239 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 227L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 2240 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00036;
	hb_xvmPushInteger( 1 );
	goto lab00037;
lab00036: ;
	hb_xvmPushLocal( 27 );
lab00037: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 2241 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 2242 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 2243 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 89L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 164 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 337L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 365L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 301L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 302L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 291L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 292L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 344L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 290L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 373L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 370L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 371L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 378L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 379L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 381L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 403L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 43 ) ) break;
	goto lab00060;
lab00038: ;
	hb_xvmSetLine( 2245 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00042;
	hb_xvmSetLine( 2247 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 2249 );
	hb_xvmPushStringConst( "Control_1", 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 404L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 343L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 378L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 379L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 380L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 381L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 338L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 341L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 342L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 296L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 382L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 403L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmSetLine( 2250 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 2251 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 2252 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 227L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 2253 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 228L ) ) break;
	if( hb_xvmAddInt( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 2254 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 2255 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 343L ) ) break;
	hb_xvmSetLine( 2256 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmSetLine( 2257 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmGreaterEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmSetLine( 2258 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00039;
	hb_xvmSetLine( 2259 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 382L ) ) break;
lab00039: ;
	hb_xvmSetLine( 2263 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 2264 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 2265 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 382L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 296L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00040;
	hb_xvmPushFuncSymbol( symbols + 165 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 343L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 378L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 379L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 380L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 381L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 35 ) ) break;
	goto lab00060;
lab00040: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 380L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00041;
	hb_xvmPushFuncSymbol( symbols + 166 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 382L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 403L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 32 ) ) break;
	goto lab00060;
lab00041: ;
	hb_xvmPushFuncSymbol( symbols + 167 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 382L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 296L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 32 ) ) break;
	goto lab00060;
lab00042: ;
	hb_xvmSetLine( 2267 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00043;
	hb_xvmSetLine( 2269 );
	hb_xvmPushStringConst( "Control_1", 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 339L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 340L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 341L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 404L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 338L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 407L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 408L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 410L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 366L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 367L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 282L ) ) break;
	hb_xvmSetLine( 2270 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 2271 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 2272 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 228L ) ) break;
	if( hb_xvmAddInt( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 2273 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 227L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 2274 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 2275 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 340L ) ) break;
	hb_xvmSetLine( 2276 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 339L ) ) break;
	hb_xvmSetLine( 2277 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 2278 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 2279 );
	hb_xvmPushFuncSymbol( symbols + 168 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 339L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 340L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 407L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 408L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 410L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 282L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 366L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 367L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 35 ) ) break;
	goto lab00060;
lab00043: ;
	hb_xvmSetLine( 2281 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
	hb_xvmSetLine( 2283 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 89L ) ) break;
	hb_xvmPushStringConst( "Control_1", 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 337L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 365L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 378L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 379L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 338L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 301L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 302L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 344L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 290L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 291L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 292L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 381L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 373L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 403L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 370L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 371L ) ) break;
	hb_xvmSetLine( 2284 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 2285 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 2286 );
	hb_xvmPushStringConst( ".T.", 3 );
	hb_xvmPushStringConst( ".F.", 3 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 337L ) ) break;
	hb_xvmSetLine( 2287 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 227L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 2288 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00044;
	hb_xvmPushInteger( 1 );
	goto lab00045;
lab00044: ;
	hb_xvmPushInteger( 2 );
lab00045: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 2289 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 2290 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 2291 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 89L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 164 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 337L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 365L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 301L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 302L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 291L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 292L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 344L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 290L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 373L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 370L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 371L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 378L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 379L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 381L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 403L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 43 ) ) break;
	goto lab00060;
lab00046: ;
	hb_xvmSetLine( 2293 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushStringConst( "I", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00053;
	hb_xvmSetLine( 2295 );
	hb_xvmPushStringConst( "Control_1", 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 404L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 343L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 378L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 379L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 380L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 381L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 338L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 341L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 342L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 296L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 382L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 403L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmSetLine( 2296 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 2297 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 2298 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 380L ) ) break;
	hb_xvmSetLine( 2299 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 227L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 2300 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 228L ) ) break;
	if( hb_xvmAddInt( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 2301 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 2302 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00049;
	hb_xvmSetLine( 2303 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmGreaterEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00048;
	hb_xvmSetLine( 2304 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00047;
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00047;
	hb_xvmSetLine( 2305 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 382L ) ) break;
	goto lab00050;
lab00047: ;
	hb_xvmSetLine( 2307 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 343L ) ) break;
	goto lab00050;
lab00048: ;
	hb_xvmSetLine( 2310 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 343L ) ) break;
	goto lab00050;
lab00049: ;
	hb_xvmSetLine( 2313 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 343L ) ) break;
lab00050: ;
	hb_xvmSetLine( 2315 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 2316 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 2317 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 382L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00051;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 296L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00051;
	hb_xvmPushFuncSymbol( symbols + 165 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 343L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 378L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 379L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 380L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 381L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 35 ) ) break;
	goto lab00060;
lab00051: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 380L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_xvmPushFuncSymbol( symbols + 166 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 382L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 403L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 32 ) ) break;
	goto lab00060;
lab00052: ;
	hb_xvmPushFuncSymbol( symbols + 167 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 382L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 296L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 32 ) ) break;
	goto lab00060;
lab00053: ;
	hb_xvmSetLine( 2319 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushStringConst( "F", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00060;
	hb_xvmSetLine( 2321 );
	hb_xvmPushStringConst( "Control_1", 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 404L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 343L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 378L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 379L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 380L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 381L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 338L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 341L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 342L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 296L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 382L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 403L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmSetLine( 2322 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 2323 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 2324 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 380L ) ) break;
	hb_xvmSetLine( 2325 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00056;
	hb_xvmSetLine( 2326 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmGreaterEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00055;
	hb_xvmSetLine( 2327 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00054;
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00054;
	hb_xvmSetLine( 2328 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 382L ) ) break;
	goto lab00057;
lab00054: ;
	hb_xvmSetLine( 2330 );
	hb_xvmPushFuncSymbol( symbols + 169 );
	hb_xvmPushStringConst( "9", 1 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 169 );
	hb_xvmPushStringConst( "9", 1 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 382L ) ) break;
	goto lab00057;
lab00055: ;
	hb_xvmSetLine( 2333 );
	hb_xvmPushFuncSymbol( symbols + 169 );
	hb_xvmPushStringConst( "9", 1 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 169 );
	hb_xvmPushStringConst( "9", 1 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 382L ) ) break;
	goto lab00057;
lab00056: ;
	hb_xvmSetLine( 2336 );
	hb_xvmPushFuncSymbol( symbols + 169 );
	hb_xvmPushStringConst( "9", 1 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 169 );
	hb_xvmPushStringConst( "9", 1 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 382L ) ) break;
lab00057: ;
	hb_xvmSetLine( 2338 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 227L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 2339 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 228L ) ) break;
	if( hb_xvmAddInt( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 2340 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 2341 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 2342 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 2343 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 382L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00058;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 296L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00058;
	hb_xvmPushFuncSymbol( symbols + 165 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 343L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 378L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 379L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 380L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 381L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 35 ) ) break;
	goto lab00060;
lab00058: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 380L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00059;
	hb_xvmPushFuncSymbol( symbols + 166 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 382L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 403L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 32 ) ) break;
	goto lab00060;
lab00059: ;
	hb_xvmPushFuncSymbol( symbols + 167 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 382L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 296L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 32 ) ) break;
lab00060: ;
	hb_xvmSetLine( 2347 );
	hb_xvmPushFuncSymbol( symbols + 170 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 2349 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushStringConst( "Control_1", 9 );
	hb_xvmPushStringConst( "_InPlaceEdit", 12 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 2351 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushStringConst( "_InPlaceEdit", 12 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 4 ) ) break;
lab00061: ;
	hb_xvmSetLine( 2355 );
	hb_xvmPushFuncSymbol( symbols + 173 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 178L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2357 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 178L ) ) break;
	hb_xvmSetLine( 2359 );
	hb_xvmPushFuncSymbol( symbols + 151 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2362 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2364 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2366 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _INPLACEEDITOK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 10 );
	hb_xvmSetLine( 2376 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "X", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 2378 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushFuncSymbol( symbols + 148 );
	hb_xvmPushStringConst( "Control_1", 9 );
	hb_xvmPushStringConst( "_InPlaceEdit", 12 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 343 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 2380 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushFuncSymbol( symbols + 148 );
	hb_xvmPushStringConst( "Control_1", 9 );
	hb_xvmPushStringConst( "_InPlaceEdit", 12 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 335 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 2381 );
	hb_xvmPushFuncSymbol( symbols + 174 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2382 );
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 2388 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 2390 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 2392 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 2394 );
	hb_xvmPushFuncSymbol( symbols + 146 );
	hb_xvmPushStringConst( "Control_1", 9 );
	hb_xvmPushStringConst( "_InPlaceEdit", 12 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 2396 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 2397 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmEqualInt( 1L ) ) break;
	hb_xvmPopLocal( 14 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 2398 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "X", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 2399 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 14 );
lab00004: ;
	hb_xvmSetLine( 2402 );
	hb_xvmPushStringConst( "MemVar", 6 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmLocalAdd( 12 );
	hb_xvmSetLine( 2403 );
	hb_xvmCopyLocals( 12, 11 );
	hb_xvmSetLine( 2404 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmMacroPop( 43 ) ) break;
	hb_xvmSetLine( 2406 );
	hb_xvmPushStringConst( "BROWSE_VALID", 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 242L ) ) break;
	hb_xvmSetLine( 2408 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 2410 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 242L ) ) break;
	hb_xvmSetLine( 2412 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmSetLine( 2414 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 2416 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 2418 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 2420 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 2422 );
	hb_xvmPushFuncSymbol( symbols + 144 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00009;
lab00005: ;
	hb_xvmSetLine( 2424 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 2426 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00011;
lab00006: ;
	hb_xvmSetLine( 2432 );
	hb_xvmPushFuncSymbol( symbols + 144 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00013;
lab00007: ;
	hb_xvmSetLine( 2438 );
	hb_xvmPushFuncSymbol( symbols + 144 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00013;
lab00008: ;
	hb_xvmSetLine( 2444 );
	hb_xvmPushFuncSymbol( symbols + 144 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00009: ;
	goto lab00013;
lab00010: ;
	hb_xvmSetLine( 2450 );
	hb_xvmPushFuncSymbol( symbols + 175 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 8 ) ) break;
lab00011: ;
	goto lab00015;
lab00012: ;
	hb_xvmSetLine( 2456 );
	hb_xvmPushFuncSymbol( symbols + 175 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 8 ) ) break;
lab00013: ;
	goto lab00015;
lab00014: ;
	hb_xvmSetLine( 2464 );
	hb_xvmPushFuncSymbol( symbols + 175 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 8 ) ) break;
lab00015: ;
	hb_xvmSetLine( 2468 );
	hb_xvmPushLocal( 8 );
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 176 );
	if( hb_xvmFunction( 0 ) ) break;
lab00016: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 238L ) ) break;
	hb_xvmSetLine( 2470 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _INPLACEEDITSAVE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 8 );
	hb_xvmSetLine( 2476 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 2478 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 147 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 2479 );
	hb_xvmPushFuncSymbol( symbols + 144 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 231L ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 2480 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 2485 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 2487 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 2488 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualInt( 1L ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 2489 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "X", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 2490 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 4 );
lab00003: ;
	hb_xvmSetLine( 2495 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "->", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmLocalAdd( 2 );
	hb_xvmSetLine( 2496 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "FIELD", 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroPopAliased( 43 ) ) break;
	hb_xvmSetLine( 2498 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 2499 );
	if( hb_xvmPushAlias() ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "SQLRDD", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmPushFuncSymbol( symbols + 126 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "SQLEX", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00004: ;
	hb_xvmSetLine( 2500 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 156 );
	if( hb_xvmDo( 0 ) ) break;
	if( hb_xvmPopAlias() ) break;
lab00005: ;
	hb_xvmSetLine( 2502 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 137 );
	if( hb_xvmDo( 0 ) ) break;
	if( hb_xvmPopAlias() ) break;
lab00006: ;
	hb_xvmSetLine( 2505 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 2507 );
	hb_xvmPushFuncSymbol( symbols + 163 );
	hb_xvmPushStringConst( "_InPlaceEdit", 12 );
	hb_xvmPushStringConst( "RELEASE", 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 2509 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PROCESSINPLACEKBDEDIT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 1 );
	hb_xvmSetLine( 2518 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 2519 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 2522 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
lab00002: ;
	hb_xvmSetLine( 2526 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 2528 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 240L ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 2530 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 240L ) ) break;
	hb_xvmSetLine( 2531 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushInteger( 2 );
	goto lab00004;
lab00003: ;
	hb_xvmPushInteger( 1 );
lab00004: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 239L ) ) break;
lab00005: ;
	hb_xvmSetLine( 2535 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 240L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 223L ) ) break;
	hb_xvmSetLine( 2536 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 239L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 224L ) ) break;
	hb_xvmSetLine( 2538 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 239L ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 2539 );
	hb_xvmPushFuncSymbol( symbols + 178 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 240L ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00007;
lab00006: ;
	hb_xvmSetLine( 2541 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 240L ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 239L ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
lab00007: ;
	hb_xvmSetLine( 2544 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 2546 );
	hb_xvmLocalSetInt( 6, 20L );
	hb_xvmSetLine( 2548 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmNegate() ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 2549 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 2552 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmLessThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 2553 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
lab00009: ;
	hb_xvmSetLine( 2558 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 239L ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 2559 );
	hb_xvmPushFuncSymbol( symbols + 178 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 240L ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 2561 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 240L ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 239L ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
lab00011: ;
	hb_xvmSetLine( 2564 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 225L ) ) break;
	hb_xvmSetLine( 2565 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 226L ) ) break;
	hb_xvmSetLine( 2566 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 227L ) ) break;
	hb_xvmSetLine( 2567 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 228L ) ) break;
	hb_xvmSetLine( 2569 );
	hb_xvmPushFuncSymbol( symbols + 140 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 2571 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 217L ) ) break;
	hb_xvmSetLine( 2572 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 218L ) ) break;
	hb_xvmSetLine( 2574 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 223L ) ) break;
	hb_xvmSetLine( 2575 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 224L ) ) break;
	hb_xvmSetLine( 2576 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 225L ) ) break;
	hb_xvmSetLine( 2577 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 226L ) ) break;
	hb_xvmSetLine( 2578 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 227L ) ) break;
	hb_xvmSetLine( 2579 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 228L ) ) break;
	hb_xvmSetLine( 2581 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 238L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 2583 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 239L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 2585 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmPushInteger( 2 );
	goto lab00013;
lab00012: ;
	hb_xvmPushInteger( 1 );
lab00013: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 239L ) ) break;
	hb_xvmSetLine( 2587 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( -10000 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
lab00014: ;
	goto lab00019;
lab00015: ;
	hb_xvmSetLine( 2595 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	hb_xvmPushInteger( 239 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 2597 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 239L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 2599 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmPushInteger( 2 );
	goto lab00017;
lab00016: ;
	hb_xvmPushInteger( 1 );
lab00017: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 239L ) ) break;
	hb_xvmSetLine( 2601 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( -10000 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00019;
lab00018: ;
	goto lab00002;
lab00019: ;
	hb_xvmSetLine( 2611 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _BROWSESYNC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 2618 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 2619 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 2620 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 2621 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 2623 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2624 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 2626 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 2628 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 2629 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 2632 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2634 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEONCHANGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 2640 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 250L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 2641 );
	hb_xvmPushFuncSymbol( symbols + 179 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 2644 );
	hb_xvmPushFuncSymbol( symbols + 180 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 2646 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _BROWSEINPLACEAPPEND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 3 );
	hb_xvmSetLine( 2655 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 2657 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 2658 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 2660 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 2661 );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 2664 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2666 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 2667 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 2669 );
	hb_xvmPushFuncSymbol( symbols + 133 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 2671 );
	hb_xvmPushFuncSymbol( symbols + 181 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 2672 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2673 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNegate() ) break;
	if( hb_xvmAddInt( 2L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2674 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 3 ) ) break;
lab00004: ;
	hb_xvmSetLine( 2677 );
	hb_xvmPushFuncSymbol( symbols + 182 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 2679 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2680 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 2682 );
	hb_xvmPushFuncSymbol( symbols + 176 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 2683 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 2684 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 2685 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 2687 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 2689 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 2691 );
	hb_xvmPushFuncSymbol( symbols + 119 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 2694 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 240L ) ) break;
	hb_xvmSetLine( 2695 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 239L ) ) break;
	hb_xvmSetLine( 2697 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _BROWSEVSCROLLUPDATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 2707 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 2709 );
	hb_xvmPushFuncSymbol( symbols + 183 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 2710 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 2711 );
	hb_xvmPushFuncSymbol( symbols + 184 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 2712 );
	hb_xvmCopyLocals( 4, 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 2714 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 2715 );
	hb_xvmPushFuncSymbol( symbols + 133 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 3 );
lab00002: ;
	hb_xvmSetLine( 2718 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 2720 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmLessThenIntIs( 100L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 2721 );
	hb_xvmPushFuncSymbol( symbols + 185 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 2722 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 2724 );
	hb_xvmPushFuncSymbol( symbols + 185 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 2725 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMultByInt( 100L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDivide() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
lab00004: ;
	hb_xvmSetLine( 2730 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BROWSEVSCROLLFASTUPDATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 2739 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 2741 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 2743 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 2744 );
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 2747 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmLessThenIntIs( 100L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 2748 );
	hb_xvmPushFuncSymbol( symbols + 187 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 2749 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 2750 );
	hb_xvmPushFuncSymbol( symbols + 185 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 2751 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 2756 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETGETBROWSEPROPERTY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 2761 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 2762 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 2764 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "BROWSE", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 2766 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 2767 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 2769 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
lab00002: ;
	hb_xvmSetLine( 2774 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( NETLOCK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 3 );
	hb_xvmSFrame( symbols + 216 );
	hb_xvmSetLine( 51 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 56 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 57 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushStatic( 1 );
	hb_xvmPopLocal( 3 );
lab00002: ;
	hb_xvmSetLine( 59 );
	hb_xvmPushFuncSymbol( symbols + 190 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 62 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 63 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushNil();
	goto lab00004;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 191 );
	if( hb_xvmFunction( 0 ) ) break;
lab00004: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 64 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 176, 192, 0, 95, 1, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 5 );
	goto lab00008;
lab00005: ;
	hb_xvmSetLine( 65 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 66 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 193, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPopLocal( 5 );
	goto lab00008;
lab00006: ;
	hb_xvmSetLine( 67 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 68 );
	hb_xvmCopyLocals( 2, 6 );
	hb_xvmSetLine( 69 );
	{
		static const HB_BYTE codeblock[ 18 ] = {
			1, 0, 0, 0, 176, 194, 0, 95, 1, 20, 1, 176, 195, 0, 12, 0, 
			68, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 5 );
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 72 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 190 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 77 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 78 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 4 );
	goto lab00010;
lab00009: ;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 196 );
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00\xD0\?", 10, 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00008;
lab00010: ;
	hb_xvmSetLine( 86 );
	hb_xvmPushLocal( 4 );
	hb_xvmPopStatic( 2 );
	hb_xvmSetLine( 88 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( NETMODIFYRECORD )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 118 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 189 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 121 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 122 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 125 );
	hb_xvmPushFuncSymbol( symbols + 195 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 126 );
	hb_xvmPushFuncSymbol( symbols + 198 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 199 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 129 );
	hb_xvmPushFuncSymbol( symbols + 200 );
	hb_xvmPushStringConst( "Failed to ", 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Record -> ", 11 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 201 );
	hb_xvmPushFuncSymbol( symbols + 191 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 132 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( NETDELETE )
{
   do {
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 197 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 203, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "DELETE", 6 );
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( NETRECALL )
{
   do {
	hb_xvmSetLine( 166 );
	hb_xvmPushFuncSymbol( symbols + 197 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 205, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "RECALL", 6 );
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( NETRECLOCK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSFrame( symbols + 216 );
	hb_xvmSetLine( 189 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStatic( 1 );
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 189 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( NETFILELOCK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSFrame( symbols + 216 );
	hb_xvmSetLine( 214 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStatic( 1 );
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 216 );
	hb_xvmPushFuncSymbol( symbols + 189 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( NETAPPEND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSFrame( symbols + 216 );
	hb_xvmSetLine( 248 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStatic( 1 );
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 249 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 2 );
lab00002: ;
	hb_xvmSetLine( 251 );
	hb_xvmPushFuncSymbol( symbols + 209 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 253 );
	hb_xvmPushFuncSymbol( symbols + 189 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 255 );
	hb_xvmPushFuncSymbol( symbols + 210 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 209 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 259 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ISLOCKED )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 284 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 191 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 286 );
	hb_xvmPushFuncSymbol( symbols + 212 );
	hb_xvmPushFuncSymbol( symbols + 213 );
	if( hb_xvmFunction( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 1, 0, 1, 0, 95, 1, 95, 255, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreaterThenInt( 0L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( NETERROR )
{
   do {
	hb_xvmSFrame( symbols + 216 );
	hb_xvmSetLine( 306 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmNot() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETNETDELAY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSFrame( symbols + 216 );
	hb_xvmSetLine( 328 );
	hb_xvmPushStatic( 1 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 330 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 331 );
	hb_xvmPushLocal( 1 );
	hb_xvmPopStatic( 1 );
lab00001: ;
	hb_xvmSetLine( 334 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 216, 2 );
	hb_xvmSFrame( symbols + 216 );
	hb_xvmPushInteger( 1 );
	hb_xvmPopStatic( 1 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 2 );
	/* *** END PROC *** */
   } while( 0 );
}

