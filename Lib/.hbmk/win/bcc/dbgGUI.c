/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "dbgGUI.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( INITGUICODEBLOCKS );
HB_FUNC_EXTERN( HMG_DEBUGGER );
HB_FUNC( PROCINITGUIDEBUGGER );
HB_FUNC_EXTERN( DOEVENTS );
HB_FUNC_EXTERN( RELEASEALLWINDOWS );
HB_FUNC( DEBUGGERMESSAGEBOX );
HB_FUNC( UPDATEINFO );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( PVALUE );
HB_FUNC_EXTERN( HB_VALTOEXP );
HB_FUNC_EXTERN( HB_VALTOSTR );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( MESSAGEBOXINDIRECT );
HB_FUNC_EXTERN( MSGYESNO );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( DOMETHOD );
HB_FUNC( HMG_ACTIVATEMAINWINDOWFIRST );
HB_FUNC_EXTERN( _GETSYSFONT );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( _DEFINEWINDOW );
HB_FUNC( AJUSTCONTROLSIZE );
HB_FUNC( DELETEDBGFORM );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( _DEFINEHOTKEY );
HB_FUNC( ONKEYPRESS );
HB_FUNC( ONKEYPRESS_TRANSPARENCY );
HB_FUNC( HMG_SHOWEVENTMONITOR );
HB_FUNC_EXTERN( ISVISTAORLATER );
HB_FUNC_EXTERN( HB_GCALL );
HB_FUNC_EXTERN( EMPTYWORKINGSET );
HB_FUNC( HOTKEYHELP );
HB_FUNC( SHOWHIDESPLITBOX );
HB_FUNC( MENUOPTION );
HB_FUNC_EXTERN( _DEFINEMAINMENU );
HB_FUNC_EXTERN( _DEFINEMENUPOPUP );
HB_FUNC_EXTERN( _DEFINEMENUITEM );
HB_FUNC_EXTERN( _DEFINESEPARATOR );
HB_FUNC_EXTERN( _ENDMENUPOPUP );
HB_FUNC_EXTERN( _ENDMENU );
HB_FUNC_EXTERN( _DEFINESPLITBOX );
HB_FUNC_EXTERN( _BEGINTOOLBAR );
HB_FUNC_EXTERN( _DEFINETOOLBUTTON );
HB_FUNC_EXTERN( _ENDTOOLBAR );
HB_FUNC_EXTERN( _ENDSPLITBOX );
HB_FUNC_EXTERN( _DEFINELABEL );
HB_FUNC_EXTERN( GETSYSTEMMETRICS );
HB_FUNC_EXTERN( _DEFINEOWNERBUTTON );
HB_FUNC( UPDATEGRIDS );
HB_FUNC_EXTERN( _BEGINTAB );
HB_FUNC_EXTERN( _BEGINTABPAGE );
HB_FUNC_EXTERN( _DEFINECOMBO );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( _DEFINEGRID );
HB_FUNC( GETFORECOLORSOURCECODE );
HB_FUNC_STATIC( LOADPRG );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( _ENDTABPAGE );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( _GETGRIDCELLVALUE );
HB_FUNC( GETFORECOLORWATCH );
HB_FUNC_EXTERN( _DEFINETEXTBOX );
HB_FUNC( GETFORECOLORVARS );
HB_FUNC( UPDATEGRIDREC );
HB_FUNC_EXTERN( _BEGINFRAME );
HB_FUNC_EXTERN( _DEFINECHECKBOX );
HB_FUNC_EXTERN( _DEFINESPINNER );
HB_FUNC( LOADSETTINGS );
HB_FUNC( SAVESETTINGS );
HB_FUNC_EXTERN( _DEFINEIMAGE );
HB_FUNC_EXTERN( SHELLEXECUTE );
HB_FUNC_EXTERN( SETWINDOWCURSOR );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( _ENDTAB );
HB_FUNC_EXTERN( _ENDWINDOW );
HB_FUNC( ADDDBGFORM );
HB_FUNC_EXTERN( SETMENU );
HB_FUNC( ENABLECONFIG );
HB_FUNC( MENUCHECKRUNMODE );
HB_FUNC_EXTERN( GETDESKTOPREALHEIGHT );
HB_FUNC_EXTERN( GETDESKTOPAREA );
HB_FUNC_EXTERN( GETDESKTOPREALWIDTH );
HB_FUNC_EXTERN( _ACTIVATEWINDOW );
HB_FUNC_EXTERN( HB_FILEEXISTS );
HB_FUNC( RESTORESETTINGS );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC( TOCURSOR );
HB_FUNC( TOGGLEBREAKPOINT );
HB_FUNC( ADDWATCHINI );
HB_FUNC( QUIT );
HB_FUNC_EXTERN( SHOWWINDOW );
HB_FUNC( GETSPLITBOXHANDLE );
HB_FUNC_EXTERN( HIDEWINDOW );
HB_FUNC_EXTERN( REDRAWWINDOW );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( HB_ADEL );
HB_FUNC( VIEWVARS );
HB_FUNC( DISPLAYVARS );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( GETCONTROLNAMEBYHANDLE );
HB_FUNC_EXTERN( GETFOCUS );
HB_FUNC_EXTERN( PLAYEXCLAMATION );
HB_FUNC( GET_HMG_SYSDATA );
HB_FUNC( AJUST );
HB_FUNC( ONKEYPRESS_DISPLAYVARS );
HB_FUNC( PUT_HMG_SYSDATA );
HB_FUNC_EXTERN( HB_HVALUEAT );
HB_FUNC_EXTERN( SETTOOLTIPBALLOON );
HB_FUNC_EXTERN( PUTFILE );
HB_FUNC_EXTERN( GETFILE );
HB_FUNC( REPAINTGRIDROW );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( HB_OSNEWLINE );
HB_FUNC( UPDATEGRIDWATCH );
HB_FUNC( EVALUATEEXP );
HB_FUNC_EXTERN( __DBGVALTOSTR );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( _PUSHKEY );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( GETFORMNAMEBYHANDLE );
HB_FUNC_EXTERN( SETLAYEREDWINDOWATTRIBUTES );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC( TABCTRL_ADJUSTRECT );
HB_FUNC( ADJUSTCTRLINTAB );
HB_FUNC_EXTERN( _SETCONTROLSIZEPOS );
HB_FUNC( UPDATEGRIDCALLSTACK );
HB_FUNC( UPDATEGRIDVARS );
HB_FUNC( UPDATEGRIDAREAS );
HB_FUNC_EXTERN( HB_AINS );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC( HMG_UTF8REMOVEBOM );
HB_FUNC_STATIC( HMG_ISCURRENTCODEPAGEUNICODE );
HB_FUNC_EXTERN( HB_STRISUTF8 );
HB_FUNC_EXTERN( HB_TRANSLATE );
HB_FUNC_EXTERN( HB_UTF8TOSTR );
HB_FUNC_EXTERN( RTRIM );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( LISTVIEW_ENSUREVISIBLE );
HB_FUNC_EXTERN( ENABLEWINDOW );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC( HMG_ISUTF8WITHBOM );
HB_FUNC_EXTERN( HB_BLEFT );
HB_FUNC_EXTERN( HB_BLEN );
HB_FUNC_EXTERN( HB_BSUBSTR );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_DBGGUI )
{ "INITGUICODEBLOCKS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITGUICODEBLOCKS )}, NULL },
{ "_BGUIRELEASEFORMDEBUGGER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HMG_DEBUGGER", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_DEBUGGER )}, NULL },
{ "PROCINITGUIDEBUGGER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PROCINITGUIDEBUGGER )}, NULL },
{ "_BGUIDOEVENTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DOEVENTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOEVENTS )}, NULL },
{ "_BGUIRELEASEALLWINDOWS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "RELEASEALLWINDOWS", {HB_FS_PUBLIC}, {HB_FUNCNAME( RELEASEALLWINDOWS )}, NULL },
{ "_BGUIMESSAGEBOX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DEBUGGERMESSAGEBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DEBUGGERMESSAGEBOX )}, NULL },
{ "_BGUIUPDATEINFO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "UPDATEINFO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UPDATEINFO )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "PVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PVALUE )}, NULL },
{ "HB_VALTOEXP", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VALTOEXP )}, NULL },
{ "HB_VALTOSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VALTOSTR )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "MESSAGEBOXINDIRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( MESSAGEBOXINDIRECT )}, NULL },
{ "MSGYESNO", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGYESNO )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "DOMETHOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOMETHOD )}, NULL },
{ "HMG_ACTIVATEMAINWINDOWFIRST", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_ACTIVATEMAINWINDOWFIRST )}, NULL },
{ "_GETSYSFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETSYSFONT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "_DEFINEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEWINDOW )}, NULL },
{ "AJUSTCONTROLSIZE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( AJUSTCONTROLSIZE )}, NULL },
{ "DELETEDBGFORM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DELETEDBGFORM )}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "_DEFINEHOTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEHOTKEY )}, NULL },
{ "ONKEYPRESS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ONKEYPRESS )}, NULL },
{ "ONKEYPRESS_TRANSPARENCY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ONKEYPRESS_TRANSPARENCY )}, NULL },
{ "HMG_SHOWEVENTMONITOR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_SHOWEVENTMONITOR )}, NULL },
{ "ISVISTAORLATER", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISVISTAORLATER )}, NULL },
{ "HB_GCALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_GCALL )}, NULL },
{ "EMPTYWORKINGSET", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTYWORKINGSET )}, NULL },
{ "HOTKEYHELP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HOTKEYHELP )}, NULL },
{ "SHOWHIDESPLITBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SHOWHIDESPLITBOX )}, NULL },
{ "MENUOPTION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MENUOPTION )}, NULL },
{ "_DEFINEMAINMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEMAINMENU )}, NULL },
{ "_DEFINEMENUPOPUP", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEMENUPOPUP )}, NULL },
{ "_DEFINEMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEMENUITEM )}, NULL },
{ "_DEFINESEPARATOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINESEPARATOR )}, NULL },
{ "_ENDMENUPOPUP", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDMENUPOPUP )}, NULL },
{ "_ENDMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDMENU )}, NULL },
{ "_DEFINESPLITBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINESPLITBOX )}, NULL },
{ "_BEGINTOOLBAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( _BEGINTOOLBAR )}, NULL },
{ "_DEFINETOOLBUTTON", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINETOOLBUTTON )}, NULL },
{ "_ENDTOOLBAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDTOOLBAR )}, NULL },
{ "_ENDSPLITBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDSPLITBOX )}, NULL },
{ "_DEFINELABEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINELABEL )}, NULL },
{ "GETSYSTEMMETRICS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSTEMMETRICS )}, NULL },
{ "_DEFINEOWNERBUTTON", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEOWNERBUTTON )}, NULL },
{ "UPDATEGRIDS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UPDATEGRIDS )}, NULL },
{ "_BEGINTAB", {HB_FS_PUBLIC}, {HB_FUNCNAME( _BEGINTAB )}, NULL },
{ "_BEGINTABPAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _BEGINTABPAGE )}, NULL },
{ "_DEFINECOMBO", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINECOMBO )}, NULL },
{ "GETSOURCEFILES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "_DEFINEGRID", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEGRID )}, NULL },
{ "GETFORECOLORSOURCECODE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETFORECOLORSOURCECODE )}, NULL },
{ "LOADPRG", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( LOADPRG )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "_ENDTABPAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDTABPAGE )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "_GETGRIDCELLVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETGRIDCELLVALUE )}, NULL },
{ "GETFORECOLORWATCH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETFORECOLORWATCH )}, NULL },
{ "_DEFINETEXTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINETEXTBOX )}, NULL },
{ "GETFORECOLORVARS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETFORECOLORVARS )}, NULL },
{ "UPDATEGRIDREC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UPDATEGRIDREC )}, NULL },
{ "_BEGINFRAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( _BEGINFRAME )}, NULL },
{ "_DEFINECHECKBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINECHECKBOX )}, NULL },
{ "_LCBTRACE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETCBTRACE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LCBTRACE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LANIMATESTOPBP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LANIMATESTOPTP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_DEFINESPINNER", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINESPINNER )}, NULL },
{ "_NSPEED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LOADSETTINGS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( LOADSETTINGS )}, NULL },
{ "SAVESETTINGS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SAVESETTINGS )}, NULL },
{ "_DEFINEIMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEIMAGE )}, NULL },
{ "SHELLEXECUTE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SHELLEXECUTE )}, NULL },
{ "SETWINDOWCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWCURSOR )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "_ENDTAB", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDTAB )}, NULL },
{ "_ENDWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDWINDOW )}, NULL },
{ "ADDDBGFORM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ADDDBGFORM )}, NULL },
{ "SETMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETMENU )}, NULL },
{ "ENABLECONFIG", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ENABLECONFIG )}, NULL },
{ "MENUCHECKRUNMODE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MENUCHECKRUNMODE )}, NULL },
{ "GETDESKTOPREALHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDESKTOPREALHEIGHT )}, NULL },
{ "GETDESKTOPAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDESKTOPAREA )}, NULL },
{ "GETDESKTOPREALWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDESKTOPREALWIDTH )}, NULL },
{ "_ACTIVATEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ACTIVATEWINDOW )}, NULL },
{ "HB_FILEEXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FILEEXISTS )}, NULL },
{ "CSETTINGSFILENAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "RESTORESETTINGS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RESTORESETTINGS )}, NULL },
{ "_CSETTINGSFILENAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "ANIMATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "STEP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TRACE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TOCURSOR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TOCURSOR )}, NULL },
{ "SETNEXTROUTINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PAUSE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TOGGLEBREAKPOINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TOGGLEBREAKPOINT )}, NULL },
{ "ADDWATCHINI", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ADDWATCHINI )}, NULL },
{ "QUIT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( QUIT )}, NULL },
{ "SHOWWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( SHOWWINDOW )}, NULL },
{ "GETSPLITBOXHANDLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETSPLITBOXHANDLE )}, NULL },
{ "HIDEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( HIDEWINDOW )}, NULL },
{ "REDRAWWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( REDRAWWINDOW )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "HB_ADEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ADEL )}, NULL },
{ "VIEWVARS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( VIEWVARS )}, NULL },
{ "VARGETNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VARGETVALTYPE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VARGETVALUE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DISPLAYVARS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DISPLAYVARS )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "GETCONTROLNAMEBYHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLNAMEBYHANDLE )}, NULL },
{ "GETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFOCUS )}, NULL },
{ "GETARRAYINFO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETHASHINFO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETOBJECTINFO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PLAYEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLAYEXCLAMATION )}, NULL },
{ "CLASSNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GET_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GET_HMG_SYSDATA )}, NULL },
{ "AJUST", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( AJUST )}, NULL },
{ "ONKEYPRESS_DISPLAYVARS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ONKEYPRESS_DISPLAYVARS )}, NULL },
{ "PUT_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PUT_HMG_SYSDATA )}, NULL },
{ "HB_HVALUEAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_HVALUEAT )}, NULL },
{ "SETTOOLTIPBALLOON", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIPBALLOON )}, NULL },
{ "PUTFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PUTFILE )}, NULL },
{ "SAVESETTINGS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFILE )}, NULL },
{ "RESTORESETTINGS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LANIMATESTOPBP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LANIMATESTOPTP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NSPEED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "QUIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "REPAINTGRIDROW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( REPAINTGRIDROW )}, NULL },
{ "ISVALIDSTOPLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BREAKPOINTTOGGLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETNEXTVALIDSTOPLINEEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "HB_OSNEWLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_OSNEWLINE )}, NULL },
{ "UPDATEGRIDWATCH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UPDATEGRIDWATCH )}, NULL },
{ "SETTOCURSOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EVALUATEEXP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( EVALUATEEXP )}, NULL },
{ "GETEXPRVALUE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGVALTOSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGVALTOSTR )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "_PUSHKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( _PUSHKEY )}, NULL },
{ "WATCHDELETE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "WATCHPOINTADD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TRACEPOINTADD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "WATCHSETEXPR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "GETFORMNAMEBYHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMNAMEBYHANDLE )}, NULL },
{ "SETLAYEREDWINDOWATTRIBUTES", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETLAYEREDWINDOWATTRIBUTES )}, NULL },
{ "GETBREAKPOINTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETWATCH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "TABCTRL_ADJUSTRECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TABCTRL_ADJUSTRECT )}, NULL },
{ "ADJUSTCTRLINTAB", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ADJUSTCTRLINTAB )}, NULL },
{ "_SETCONTROLSIZEPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETCONTROLSIZEPOS )}, NULL },
{ "UPDATEGRIDCALLSTACK", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UPDATEGRIDCALLSTACK )}, NULL },
{ "UPDATEGRIDVARS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UPDATEGRIDVARS )}, NULL },
{ "UPDATEGRIDAREAS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UPDATEGRIDAREAS )}, NULL },
{ "GETPROCSTACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETVARS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "WATCHGETINFO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_AINS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_AINS )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "WATCHCOUNT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETAREAS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "GETREC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LOADSOURCEFILE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__ENUMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HMG_UTF8REMOVEBOM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_UTF8REMOVEBOM )}, NULL },
{ "HMG_ISCURRENTCODEPAGEUNICODE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_ISCURRENTCODEPAGEUNICODE )}, NULL },
{ "HB_STRISUTF8", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_STRISUTF8 )}, NULL },
{ "HB_TRANSLATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_TRANSLATE )}, NULL },
{ "HB_UTF8TOSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UTF8TOSTR )}, NULL },
{ "RTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( RTRIM )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "ACURRENTLINEINFO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LISTVIEW_ENSUREVISIBLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTVIEW_ENSUREVISIBLE )}, NULL },
{ "ENABLEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ENABLEWINDOW )}, NULL },
{ "LDEACTIVATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LDEACTIVATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "HMG_ISUTF8WITHBOM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_ISUTF8WITHBOM )}, NULL },
{ "HB_BLEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BLEFT )}, NULL },
{ "HB_BLEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BLEN )}, NULL },
{ "HB_BSUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BSUBSTR )}, NULL },
{ "(_INITSTATICS00016)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_DBGGUI, "dbgGUI.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_DBGGUI
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_DBGGUI )
   #include "hbiniseg.h"
#endif

HB_FUNC( INITGUICODEBLOCKS )
{
   do {
	hb_xvmSetLine( 78 );
	hb_xvmPushSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 7 ] = {
			176, 3, 0, 9, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 79 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 5, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 80 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 7, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 81 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 9, 0, 164, 123, 1, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 82 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 11, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 84 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DEBUGGERMESSAGEBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSetLine( 89 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 90 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 91 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00003: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 3 );
	hb_xvmSetLine( 90 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "HMG Debugger", 12 );
	hb_xvmPushInteger( 4225 );
	hb_xvmPushInteger( 32001 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 94 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushStringConst( "Are you sure you want to EXIT the program \?", 43 );
	hb_xvmPushStringConst( "HMG Debugger", 12 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 5 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmDo( 0 ) ) break;
lab00005: ;
	hb_xvmSetLine( 100 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PROCINITGUIDEBUGGER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 1 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 140 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 141 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "RELEASE", 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 142 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 212L ) ) break;
	hb_xvmPushInteger( 10 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 213L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 10 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 153 );
	hb_xvmPushStringConst( "_dbgIcon", 8 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 423L ) ) break;
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "HMG Debugger  ( Ctrl+H - Help )", 31 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
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
		static const HB_BYTE codeblock[ 6 ] = {
			176, 27, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 26 ] = {
			176, 28, 0, 106, 18, 95, 72, 77, 71, 95, 70, 111, 114, 109, 68, 101, 
			98, 117, 103, 103, 101, 114, 0, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 106 ] = {
			176, 29, 0, 98, 24, 0, 93, 254, 0, 1, 106, 6, 87, 73, 68, 84, 
			72, 0, 12, 2, 93, 138, 2, 35, 28, 25, 176, 30, 0, 98, 24, 0, 
			93, 254, 0, 1, 106, 6, 87, 73, 68, 84, 72, 0, 93, 138, 2, 20, 
			3, 176, 29, 0, 98, 24, 0, 93, 254, 0, 1, 106, 7, 72, 69, 73, 
			71, 72, 84, 0, 12, 2, 93, 38, 2, 35, 28, 26, 176, 30, 0, 98, 
			24, 0, 93, 254, 0, 1, 106, 7, 72, 69, 73, 71, 72, 84, 0, 93, 
			38, 2, 20, 3, 176, 27, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
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
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 27, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	{
		static const HB_BYTE codeblock[ 2 ] = {
			9, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
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
	hb_xvmSetLine( 171 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 13 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 32, 0, 92, 13, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 172 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 46 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 32, 0, 92, 46, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 27 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 32, 0, 92, 27, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 107 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 33, 0, 92, 107, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 109 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 33, 0, 92, 109, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 120 );
	{
		static const HB_BYTE codeblock[ 72 ] = {
			176, 30, 0, 106, 18, 95, 72, 77, 71, 95, 70, 111, 114, 109, 68, 101, 
			98, 117, 103, 103, 101, 114, 0, 106, 8, 84, 111, 112, 77, 111, 115, 116, 
			0, 176, 29, 0, 106, 18, 95, 72, 77, 71, 95, 70, 111, 114, 109, 68, 
			101, 98, 117, 103, 103, 101, 114, 0, 106, 8, 84, 111, 112, 77, 111, 115, 
			116, 0, 12, 2, 68, 12, 3, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 179 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 68 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 34, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 77 );
	{
		static const HB_BYTE codeblock[ 21 ] = {
			176, 35, 0, 12, 0, 28, 14, 176, 36, 0, 20, 0, 176, 37, 0, 12, 
			0, 25, 3, 100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 72 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 38, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 122 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 39, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 114 );
	{
		static const HB_BYTE codeblock[ 7 ] = {
			176, 40, 0, 122, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 119 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 2, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 188 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 121 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 3, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 116 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 4, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 190 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 118 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 5, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 116 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 6, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 114 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 7, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 120 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 8, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 194 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 88 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 12, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 197 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushStringConst( "Run", 3 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "&Animate", 8 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "F3", 2 );
	if( hb_xvmPlus() ) break;
	{
		static const HB_BYTE codeblock[ 7 ] = {
			176, 40, 0, 122, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "Menu_Animate", 12 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 199 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "&Step", 5 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "F8", 2 );
	if( hb_xvmPlus() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 2, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "Menu_Step", 9 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "T&race", 6 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "F10", 3 );
	if( hb_xvmPlus() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 3, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "Menu_Trace", 10 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "&Go", 3 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "F5", 2 );
	if( hb_xvmPlus() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 4, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "Menu_Go", 7 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "To &Cursor", 10 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "F7", 2 );
	if( hb_xvmPlus() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 5, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "Menu_ToCursor", 13 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "&Next Routine", 13 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Ctrl+F5", 7 );
	if( hb_xvmPlus() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 6, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "Menu_Next", 9 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 204 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "&Pause", 6 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Ctrl+F3", 7 );
	if( hb_xvmPlus() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 7, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "Menu_Pause", 10 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushStringConst( "Point", 5 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 209 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "&BreakPoint", 11 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "F9", 2 );
	if( hb_xvmPlus() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 8, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 210 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "&TracePoint", 11 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 9, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 211 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "&WatchPoint", 11 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 10, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 214 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushStringConst( "Setting", 7 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "S&etting", 8 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 11, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "Menu_Setting", 12 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 216 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushStringConst( "Quit", 4 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 219 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushStringConst( "&Quit", 5 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 12, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 220 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 221 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 223 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 225 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushStringConst( "ToolBar_1", 9 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 85 );
	hb_xvmPushInteger( 79 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_1", 8 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "&Animate", 8 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 7 ] = {
			176, 40, 0, 122, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_animate", 8 );
	hb_xvmPushStringConst( "Run in Animate mode", 19 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 227 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_2", 8 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "&Step", 5 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 2, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_step", 5 );
	hb_xvmPushStringConst( "Run in Single Step mode", 23 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 228 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_3", 8 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "T&race", 6 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 3, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_trace", 6 );
	hb_xvmPushStringConst( "Run in Trace mode", 17 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_4", 8 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "&Go", 3 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 4, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_go", 3 );
	hb_xvmPushStringConst( "Run in Go mode", 14 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 230 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_5", 8 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "To &Cursor", 10 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 5, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_tocursor", 9 );
	hb_xvmPushStringConst( "Run until current Cursor Position", 33 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_6", 8 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "&Next Routine", 13 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 6, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_next", 5 );
	hb_xvmPushStringConst( "Run until Next Routine", 22 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 232 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_7", 8 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "&Pause", 6 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 7, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_pause", 6 );
	hb_xvmPushStringConst( "Pause any run mode", 18 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 233 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 235 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushStringConst( "ToolBar_2", 9 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 85 );
	hb_xvmPushInteger( 79 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 236 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_8", 8 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "&BreakPoint", 11 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 8, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_breakpoint", 11 );
	hb_xvmPushStringConst( "Toggle BreakPoint", 17 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 237 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_9", 8 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "&TracePoint", 11 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 9, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_tracepoint", 11 );
	hb_xvmPushStringConst( "Add TracePoint", 14 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 239 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_10", 9 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "&WatchPoint", 11 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 10, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_watchpoint", 11 );
	hb_xvmPushStringConst( "Add WatchPoint", 14 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_11", 9 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "S&etting", 8 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 11, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_configuration", 14 );
	hb_xvmPushStringConst( "Enable/Disable Setting", 22 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 243 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "Button_12", 9 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "&Quit", 5 );
	if( hb_xvmMacroText() ) break;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 40, 0, 92, 12, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_quit", 5 );
	hb_xvmPushStringConst( "Exit the debugger and closing the application", 45 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 244 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 246 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushStringConst( "Label_1", 7 );
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 36 ) ) break;
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Refresh", 7 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 55, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushInteger( 100 );
	hb_xvmPushInteger( 30 );
	hb_xvmPushStringConst( "_refresh", 8 );
	hb_xvmPushStringConst( "Refresh Grid Data", 17 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Calibri", 7 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 33 ) ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Calibri", 7 );
	hb_xvmPushInteger( 11 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 55, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 26 ) ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushStringConst( "Source", 6 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 264 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 55, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 60 ] = {
			176, 29, 0, 106, 18, 95, 72, 77, 71, 95, 70, 111, 114, 109, 68, 101, 
			98, 117, 103, 103, 101, 114, 0, 106, 20, 67, 111, 109, 98, 111, 66, 111, 
			120, 95, 83, 111, 117, 114, 99, 101, 67, 111, 100, 101, 0, 106, 6, 86, 
			65, 76, 85, 69, 0, 12, 3, 165, 82, 6, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
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
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
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
	if( hb_xvmDo( 43 ) ) break;
	hb_xvmSetLine( 266 );
	{
		static const HB_BYTE codeblock[ 51 ] = {
			2, 0, 0, 0, 95, 2, 103, 2, 0, 8, 28, 28, 103, 4, 0, 103, 
			6, 0, 1, 92, 2, 1, 28, 16, 93, 255, 0, 93, 236, 0, 93, 139, 
			0, 4, 3, 0, 25, 14, 93, 220, 0, 93, 220, 0, 93, 220, 0, 4, 
			3, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 268 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 270 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 272 );
	hb_xvmPushStringConst( "Grid_SourceCode_", 16 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 5 );
	hb_xvmSetLine( 273 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Line", 4 );
	hb_xvmPushStringConst( "Source Code", 11 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 63, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 63, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 270 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 291 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 293 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushStringConst( "Stack", 5 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 295 );
	{
		static const HB_BYTE codeblock[ 13 ] = {
			93, 220, 0, 93, 220, 0, 93, 220, 0, 4, 3, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 296 );
	{
		static const HB_BYTE codeblock[ 75 ] = {
			2, 0, 0, 0, 176, 67, 0, 176, 68, 0, 106, 15, 71, 114, 105, 100, 
			95, 67, 97, 108, 108, 83, 116, 97, 99, 107, 0, 106, 18, 95, 72, 77, 
			71, 95, 70, 111, 114, 109, 68, 101, 98, 117, 103, 103, 101, 114, 0, 95, 
			2, 92, 2, 12, 4, 12, 1, 28, 11, 121, 92, 100, 121, 4, 3, 0, 
			25, 10, 121, 121, 93, 128, 0, 4, 3, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 309 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Level", 5 );
	hb_xvmPushStringConst( "File", 4 );
	hb_xvmPushStringConst( "Function", 8 );
	hb_xvmPushStringConst( "Line", 4 );
	hb_xvmArrayGen( 4 );
	hb_xvmPushInteger( 80 );
	hb_xvmPushInteger( 150 );
	hb_xvmPushInteger( 300 );
	hb_xvmPushInteger( 100 );
	hb_xvmArrayGen( 4 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushStringConst( "Press ENTER to see the source code", 34 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 4 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmArrayGen( 4 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 311 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 313 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushStringConst( "Watch", 5 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 315 );
	{
		static const HB_BYTE codeblock[ 13 ] = {
			93, 220, 0, 93, 220, 0, 93, 220, 0, 4, 3, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 328 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Number", 6 );
	hb_xvmPushStringConst( "Type", 4 );
	hb_xvmPushStringConst( "Expression", 10 );
	hb_xvmPushStringConst( "ValType", 7 );
	hb_xvmPushStringConst( "Value", 5 );
	hb_xvmPushStringConst( "ValidExpr", 9 );
	hb_xvmArrayGen( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushInteger( 100 );
	hb_xvmArrayGen( 6 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushStringConst( "Press ENTER to edit the expression and press DELETE to delete item", 66 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 69, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 69, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 69, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 69, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 69, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 69, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmArrayGen( 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmArrayGen( 6 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 331 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 139 );
	hb_xvmPushInteger( 69 );
	hb_xvmPushInteger( 19 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Watch number", 12 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 36 ) ) break;
	hb_xvmSetLine( 332 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 335 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushStringConst( "Label_WatchType", 15 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 139 );
	hb_xvmPushInteger( 69 );
	hb_xvmPushInteger( 19 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Watch type", 10 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 36 ) ) break;
	hb_xvmSetLine( 338 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushStringConst( "Watch expression", 16 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 231 );
	hb_xvmPushInteger( 186 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 139 );
	hb_xvmPushInteger( 69 );
	hb_xvmPushInteger( 19 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 35 ) ) break;
	hb_xvmSetLine( 339 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 341 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "ColumnWIDTH", 11 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 342 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "ColumnWIDTH", 11 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchType", 15 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 343 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "ColumnWIDTH", 11 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushInteger( 350 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 345 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 347 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushStringConst( "Evaluate", 8 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 359 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Expression", 10 );
	hb_xvmPushStringConst( "Value", 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushInteger( 325 );
	hb_xvmPushDouble( * ( double * ) "UUUUU\x15" "k@", 255, 255 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushStringConst( "Press ENTER to copy expression for evaluate and press DELETE to delete item", 75 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 128 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 364 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushStringConst( "Label_Calc", 10 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Enter the expression to evaluate:", 33 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushInteger( 139 );
	hb_xvmPushInteger( 69 );
	hb_xvmPushInteger( 19 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "e.g. VarName, FuncName( param1, ... ), VarName := Value, Arr[i,1] := Value, etc.", 80 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 36 ) ) break;
	hb_xvmSetLine( 367 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushStringConst( "e.g. VarName, FuncName( param1, ... ), VarName := Value, Arr[i,1] := Value, etc.", 80 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 231 );
	hb_xvmPushInteger( 186 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 139 );
	hb_xvmPushInteger( 69 );
	hb_xvmPushInteger( 19 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 35 ) ) break;
	hb_xvmSetLine( 369 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 371 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushStringConst( "Variables", 9 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 373 );
	{
		static const HB_BYTE codeblock[ 13 ] = {
			93, 220, 0, 93, 220, 0, 93, 220, 0, 4, 3, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 387 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Level", 5 );
	hb_xvmPushStringConst( "Scope", 5 );
	hb_xvmPushStringConst( "Name", 4 );
	hb_xvmPushStringConst( "Type", 4 );
	hb_xvmPushStringConst( "Value", 5 );
	hb_xvmArrayGen( 5 );
	hb_xvmPushInteger( 80 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushInteger( 300 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushInteger( 100 );
	hb_xvmArrayGen( 5 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushStringConst( "Press ENTER for inspect the value of variables", 46 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 71, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 71, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 71, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 71, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 12 ] = {
			2, 0, 0, 0, 176, 71, 0, 95, 2, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmArrayGen( 5 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmArrayGen( 5 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 128 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 389 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 391 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushStringConst( "Areas", 5 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 404 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Alias", 5 );
	hb_xvmPushStringConst( "Area", 4 );
	hb_xvmPushStringConst( "RDD Name", 8 );
	hb_xvmPushStringConst( "Reccount", 8 );
	hb_xvmPushStringConst( "Recno", 5 );
	hb_xvmPushStringConst( "Bof", 3 );
	hb_xvmPushStringConst( "Eof", 3 );
	hb_xvmPushStringConst( "Found", 5 );
	hb_xvmPushStringConst( "Deleted", 7 );
	hb_xvmPushStringConst( "dbFilter", 8 );
	hb_xvmPushStringConst( "ordName", 7 );
	hb_xvmPushStringConst( "ordKey", 6 );
	hb_xvmArrayGen( 12 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 12 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushStringConst( "Available work areas", 20 );
	{
		static const HB_BYTE codeblock[ 55 ] = {
			176, 72, 0, 20, 0, 176, 21, 0, 106, 18, 95, 72, 77, 71, 95, 70, 
			111, 114, 109, 68, 101, 98, 117, 103, 103, 101, 114, 0, 106, 11, 71, 114, 
			105, 100, 95, 65, 114, 101, 97, 115, 0, 106, 9, 83, 69, 84, 70, 79, 
			67, 85, 83, 0, 12, 3, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 128 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 416 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Name", 4 );
	hb_xvmPushStringConst( "Type", 4 );
	hb_xvmPushStringConst( "Length", 6 );
	hb_xvmPushStringConst( "Value", 5 );
	hb_xvmArrayGen( 4 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 4 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushStringConst( "Value of current RECORD in the selected work area", 49 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 418 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 420 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushStringConst( "Setting", 7 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 422 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 20 ) ) break;
	hb_xvmSetLine( 424 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushStringConst( "CheckBox_Config1", 16 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Allow Tracing of Code Blocks", 28 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 84 ] = {
			48, 75, 0, 176, 2, 0, 12, 0, 176, 29, 0, 106, 18, 95, 72, 77, 
			71, 95, 70, 111, 114, 109, 68, 101, 98, 117, 103, 103, 101, 114, 0, 106, 
			17, 67, 104, 101, 99, 107, 66, 111, 120, 95, 67, 111, 110, 102, 105, 103, 
			49, 0, 106, 6, 86, 65, 76, 85, 69, 0, 12, 3, 112, 1, 73, 48, 
			76, 0, 176, 2, 0, 12, 0, 48, 77, 0, 176, 2, 0, 12, 0, 112, 
			0, 112, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 32 ) ) break;
	hb_xvmSetLine( 425 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushStringConst( "CheckBox_Config2", 16 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Stop at BreakPoint in Animate mode", 34 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 63 ] = {
			48, 78, 0, 176, 2, 0, 12, 0, 176, 29, 0, 106, 18, 95, 72, 77, 
			71, 95, 70, 111, 114, 109, 68, 101, 98, 117, 103, 103, 101, 114, 0, 106, 
			17, 67, 104, 101, 99, 107, 66, 111, 120, 95, 67, 111, 110, 102, 105, 103, 
			50, 0, 106, 6, 86, 65, 76, 85, 69, 0, 12, 3, 112, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 32 ) ) break;
	hb_xvmSetLine( 426 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushStringConst( "CheckBox_Config3", 16 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Stop at TracePoint in Animate mode", 34 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 63 ] = {
			48, 79, 0, 176, 2, 0, 12, 0, 176, 29, 0, 106, 18, 95, 72, 77, 
			71, 95, 70, 111, 114, 109, 68, 101, 98, 117, 103, 103, 101, 114, 0, 106, 
			17, 67, 104, 101, 99, 107, 66, 111, 120, 95, 67, 111, 110, 102, 105, 103, 
			51, 0, 106, 6, 86, 65, 76, 85, 69, 0, 12, 3, 112, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 32 ) ) break;
	hb_xvmSetLine( 428 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushStringConst( "Label_Config", 12 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Speed in Animate mode ( in milliseconds ) ", 42 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 36 ) ) break;
	hb_xvmSetLine( 429 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushStringConst( "Spinner_Config", 14 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushInteger( 0 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65534 );
#else
	hb_xvmPushLong( 65534L );
#endif
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 61 ] = {
			48, 81, 0, 176, 2, 0, 12, 0, 176, 29, 0, 106, 18, 95, 72, 77, 
			71, 95, 70, 111, 114, 109, 68, 101, 98, 117, 103, 103, 101, 114, 0, 106, 
			15, 83, 112, 105, 110, 110, 101, 114, 95, 67, 111, 110, 102, 105, 103, 0, 
			106, 6, 86, 65, 76, 85, 69, 0, 12, 3, 112, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 30 ) ) break;
	hb_xvmSetLine( 431 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushStringConst( "Button_Config1", 14 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Load", 4 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 82, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_open", 5 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
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
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 33 ) ) break;
	hb_xvmSetLine( 432 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushStringConst( "Button_Config2", 14 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Save", 4 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 83, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "_save", 5 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
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
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 33 ) ) break;
	hb_xvmSetLine( 434 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 436 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushStringConst( "About", 5 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 438 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushStringConst( "Image_1", 7 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "_about", 6 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 87 ] = {
			176, 85, 0, 121, 106, 5, 111, 112, 101, 110, 0, 106, 13, 114, 117, 110, 
			100, 108, 108, 51, 50, 46, 101, 120, 101, 0, 106, 54, 117, 114, 108, 46, 
			100, 108, 108, 44, 70, 105, 108, 101, 80, 114, 111, 116, 111, 99, 111, 108, 
			72, 97, 110, 100, 108, 101, 114, 32, 104, 116, 116, 112, 58, 47, 47, 115, 
			114, 118, 101, 116, 46, 98, 108, 111, 103, 115, 112, 111, 116, 46, 99, 111, 
			109, 0, 100, 122, 12, 6, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "Click here for open the blog of author: http://srvet.blogspot.com", 65 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 440 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "Image_1", 7 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 32649 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 442 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 444 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 446 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 448 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 450 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 186L ) ) break;
	hb_xvmPopStatic( 8 );
	hb_xvmSetLine( 451 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 187L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 453 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 454 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushInteger( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 455 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Setting", 12 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 456 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CLOSABLE", 8 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 457 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	hb_xvmPushInteger( 700 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 458 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 459 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 460 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 96 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 461 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "SHOW", 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 463 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 464 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 466 );
	hb_xvmPushFuncSymbol( symbols + 98 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 467 );
	hb_xvmPushFuncSymbol( symbols + 100 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 469 );
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 472 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "REDRAW", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 473 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmDo( 0 ) ) break;
lab00006: ;
	hb_xvmSetLine( 477 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HOTKEYHELP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 483 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 484 );
	hb_xvmPushStatic( 9 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopStatic( 9 );
	hb_xvmSetLine( 485 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Alt+D", 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "On/Off", 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 486 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Alt +/-", 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Transparency", 12 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 487 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Alt+F9", 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Top/Bottom", 10 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 488 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Alt+M", 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Release Memory", 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 489 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "F11", 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "ToolBar/Menu", 12 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 490 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Alt+X", 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Quit", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 491 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 492 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Ctrl+H", 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Help", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 493 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "F3", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Animate", 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 494 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "F8", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Step", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 495 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "F10", 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Trace", 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 496 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "F5", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Go", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 497 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "F7", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "To Cursor", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 498 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Ctrl+F5", 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Next Routine", 12 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 499 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Ctrl+F3", 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Pause", 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 500 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "F9", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "BreakPoint", 10 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 501 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "HMG Debugger - Help", 19 );
	hb_xvmPushInteger( 4224 );
	hb_xvmPushInteger( 32001 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 502 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 9 );
	hb_xvmSetLine( 504 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MENUOPTION )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 509 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 510 );
	goto lab00013;
lab00001: ;
	hb_xvmSetLine( 511 );
	hb_xvmPushSymbol( symbols + 103 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00014;
lab00002: ;
	hb_xvmSetLine( 512 );
	hb_xvmPushSymbol( symbols + 104 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00014;
lab00003: ;
	hb_xvmSetLine( 513 );
	hb_xvmPushSymbol( symbols + 105 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00014;
lab00004: ;
	hb_xvmSetLine( 514 );
	hb_xvmPushSymbol( symbols + 106 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00014;
lab00005: ;
	hb_xvmSetLine( 515 );
	hb_xvmPushFuncSymbol( symbols + 107 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00014;
lab00006: ;
	hb_xvmSetLine( 516 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00014;
lab00007: ;
	hb_xvmSetLine( 517 );
	hb_xvmPushSymbol( symbols + 109 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00014;
lab00008: ;
	hb_xvmSetLine( 518 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00014;
lab00009: ;
	hb_xvmSetLine( 519 );
	hb_xvmPushFuncSymbol( symbols + 111 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00014;
lab00010: ;
	hb_xvmSetLine( 520 );
	hb_xvmPushFuncSymbol( symbols + 111 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00014;
lab00011: ;
	hb_xvmSetLine( 521 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00014;
lab00012: ;
	hb_xvmSetLine( 522 );
	hb_xvmPushFuncSymbol( symbols + 112 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00014;
lab00013: ;
	hb_xvmPushLocal( 1 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 1L )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 3L )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 4L )
		{
			hb_stackPop();
			goto lab00004;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 5L )
		{
			hb_stackPop();
			goto lab00005;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 6L )
		{
			hb_stackPop();
			goto lab00006;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 7L )
		{
			hb_stackPop();
			goto lab00007;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 8L )
		{
			hb_stackPop();
			goto lab00008;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 9L )
		{
			hb_stackPop();
			goto lab00009;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 10L )
		{
			hb_stackPop();
			goto lab00010;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 11L )
		{
			hb_stackPop();
			goto lab00011;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 12L )
		{
			hb_stackPop();
			goto lab00012;
		}
		hb_stackPop();
	}
lab00014: ;
	hb_xvmSetLine( 525 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MENUCHECKRUNMODE )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 530 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Animate", 12 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 531 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Step", 9 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 532 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Trace", 10 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 533 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Go", 7 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 534 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_ToCursor", 13 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 535 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Next", 9 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 536 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Pause", 10 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 537 );
	goto lab00008;
lab00001: ;
	hb_xvmSetLine( 538 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Animate", 12 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00009;
lab00002: ;
	hb_xvmSetLine( 539 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Step", 9 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00009;
lab00003: ;
	hb_xvmSetLine( 540 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Trace", 10 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00009;
lab00004: ;
	hb_xvmSetLine( 541 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Go", 7 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00009;
lab00005: ;
	hb_xvmSetLine( 542 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_ToCursor", 13 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00009;
lab00006: ;
	hb_xvmSetLine( 543 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Next", 9 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00009;
lab00007: ;
	hb_xvmSetLine( 544 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Pause", 10 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmPushLocal( 1 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 1L )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 3L )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 4L )
		{
			hb_stackPop();
			goto lab00004;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 5L )
		{
			hb_stackPop();
			goto lab00005;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 6L )
		{
			hb_stackPop();
			goto lab00006;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 7L )
		{
			hb_stackPop();
			goto lab00007;
		}
		hb_stackPop();
	}
lab00009: ;
	hb_xvmSetLine( 547 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SHOWHIDESPLITBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 552 );
	hb_xvmPushStatic( 7 );
	if( hb_xvmNot() ) break;
	hb_xvmPopStatic( 7 );
	hb_xvmSetLine( 553 );
	hb_xvmPushStatic( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 554 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 187L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 555 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 557 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 558 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 187L ) ) break;
	hb_xvmPushStatic( 8 );
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 560 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 561 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_1", 7 );
	hb_xvmPushStringConst( "HANDLE", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 563 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ADDDBGFORM )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 568 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 570 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DELETEDBGFORM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 575 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 576 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 577 );
	hb_xvmPushFuncSymbol( symbols + 119 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
lab00001: ;
	hb_xvmSetLine( 580 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( VIEWVARS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 587 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 588 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreaterEqualThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "ITEMCOUNT", 9 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 589 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 590 );
	hb_xvmPushStatic( 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 591 );
	hb_xvmPushSymbol( symbols + 121 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 592 );
	hb_xvmPushSymbol( symbols + 122 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 593 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 594 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 3 ) ) break;
lab00001: ;
	hb_xvmSetLine( 597 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DISPLAYVARS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 11, 3 );
	hb_xvmSetLine( 603 );
	hb_xvmPushStringConst( "_HMG_dbgVar_", 12 );
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 4 );
	hb_xvmSetLine( 604 );
	hb_xvmPushStringConst( "GridVars2", 9 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 610 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 611 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "'", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 612 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "\"", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 613 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "]", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 614 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "[", 1 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 615 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( ":", 1 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 617 );
	hb_xvmPushFuncSymbol( symbols + 126 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLocalByRef( 11 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 618 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 619 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushStringConst( "GridVars2", 9 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 3 );
lab00002: ;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 620 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 622 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 623 );
	hb_xvmPushSymbol( symbols + 128 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 624 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "H", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 625 );
	hb_xvmPushSymbol( symbols + 129 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 626 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "O", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 627 );
	hb_xvmPushSymbol( symbols + 130 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 628 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 629 );
	hb_xvmPushFuncSymbol( symbols + 131 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 630 );
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 633 );
	hb_xvmPushFuncSymbol( symbols + 131 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 634 );
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 637 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 638 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "RELEASE", 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 639 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmDo( 0 ) ) break;
lab00007: ;
	hb_xvmSetLine( 642 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "AH", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 643 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( " [ 1 ... ", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ] ", 3 );
	hb_xvmLocalAdd( 6 );
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 645 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( " is of class: ", 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmLocalAdd( 6 );
lab00009: ;
	hb_xvmSetLine( 648 );
	hb_xvmPushFuncSymbol( symbols + 133 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 649 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 33L ) ) break;
	hb_xvmSetLine( 651 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 661 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 650 );
	hb_xvmPushInteger( 550 );
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
		static const HB_BYTE codeblock[ 18 ] = {
			0, 0, 2, 0, 4, 0, 5, 0, 176, 134, 0, 95, 255, 95, 254, 12, 
			2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 14 ] = {
			0, 0, 1, 0, 4, 0, 176, 28, 0, 95, 255, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 18 ] = {
			0, 0, 2, 0, 4, 0, 5, 0, 176, 134, 0, 95, 255, 95, 254, 12, 
			2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
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
	{
		static const HB_BYTE codeblock[ 18 ] = {
			0, 0, 2, 0, 4, 0, 5, 0, 176, 134, 0, 95, 255, 95, 254, 12, 
			2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
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
	hb_xvmSetLine( 675 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Name", 4 );
	hb_xvmPushStringConst( "Type", 4 );
	hb_xvmPushStringConst( "Value", 5 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushInteger( 300 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushInteger( 100 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushStringConst( "Press ENTER for inspect the value of variables", 46 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	{
		static const HB_BYTE codeblock[ 9 ] = {
			0, 0, 1, 0, 3, 0, 95, 255, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 9 ] = {
			0, 0, 1, 0, 3, 0, 95, 255, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 9 ] = {
			0, 0, 1, 0, 3, 0, 95, 255, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmArrayGen( 3 );
	{
		static const HB_BYTE codeblock[ 13 ] = {
			93, 220, 0, 93, 220, 0, 93, 220, 0, 4, 3, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 13 ] = {
			93, 220, 0, 93, 220, 0, 93, 220, 0, 4, 3, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 13 ] = {
			93, 220, 0, 93, 220, 0, 93, 220, 0, 4, 3, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmArrayGen( 3 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmPushInteger( 220 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 677 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 107 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 33, 0, 92, 107, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 678 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 109 );
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 33, 0, 92, 109, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 680 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 68 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 34, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 682 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 13 );
	{
		static const HB_BYTE codeblock[ 32 ] = {
			0, 0, 5, 0, 4, 0, 5, 0, 2, 0, 10, 0, 3, 0, 176, 135, 
			0, 92, 13, 95, 255, 95, 254, 95, 253, 95, 252, 95, 251, 12, 6, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 683 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 27 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			0, 0, 1, 0, 4, 0, 176, 135, 0, 92, 27, 95, 255, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 685 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ColumnAUTOFIT", 13 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 686 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ColumnAUTOFIT", 13 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 688 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ColumnWIDTH", 11 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmLessThenIntIs( 300L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 689 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ColumnWIDTH", 11 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 300 );
	if( hb_xvmDo( 5 ) ) break;
lab00010: ;
	hb_xvmSetLine( 691 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ColumnWIDTH", 11 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmLessThenIntIs( 100L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 692 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ColumnWIDTH", 11 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmDo( 5 ) ) break;
lab00011: ;
	hb_xvmSetLine( 695 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 696 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 698 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "Center", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 699 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 700 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 702 );
	hb_xvmPushFuncSymbol( symbols + 136 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 704 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ONKEYPRESS_DISPLAYVARS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 6 );
	hb_xvmSetLine( 710 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 711 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 712 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 713 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 715 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 716 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 717 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "H", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 718 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushFuncSymbol( symbols + 137 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 719 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "O", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 720 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 723 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 27L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 724 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "RELEASE", 7 );
	if( hb_xvmDo( 2 ) ) break;
lab00004: ;
	hb_xvmSetLine( 727 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( AJUST )
{
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 732 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 45 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 733 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 46 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 3L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 734 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 735 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 737 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GET_HMG_SYSDATA )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 742 );
	hb_xvmPushInteger( 21 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 743 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 744 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 210L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 745 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 427L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 746 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 428L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 747 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 429L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 748 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 430L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 749 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 750 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 30L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 751 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 752 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 753 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 11L ) ) break;
	hb_xvmSetLine( 754 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 12L ) ) break;
	hb_xvmSetLine( 755 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 13L ) ) break;
	hb_xvmSetLine( 756 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 14L ) ) break;
	hb_xvmSetLine( 757 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 260L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 15L ) ) break;
	hb_xvmSetLine( 758 );
	hb_xvmPushFuncSymbol( symbols + 138 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 16L ) ) break;
	hb_xvmSetLine( 759 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 207L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmSetLine( 760 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 199L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 18L ) ) break;
	hb_xvmSetLine( 761 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 46L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 19L ) ) break;
	hb_xvmSetLine( 762 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 20L ) ) break;
	hb_xvmSetLine( 763 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 21L ) ) break;
	hb_xvmSetLine( 765 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PUT_HMG_SYSDATA )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 770 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 33L ) ) break;
	hb_xvmSetLine( 771 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 210L ) ) break;
	hb_xvmSetLine( 772 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 427L ) ) break;
	hb_xvmSetLine( 773 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 428L ) ) break;
	hb_xvmSetLine( 774 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 429L ) ) break;
	hb_xvmSetLine( 775 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 430L ) ) break;
	hb_xvmSetLine( 776 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 37L ) ) break;
	hb_xvmSetLine( 777 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 8L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 30L ) ) break;
	hb_xvmSetLine( 778 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 431L ) ) break;
	hb_xvmSetLine( 779 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 10L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 21L ) ) break;
	hb_xvmSetLine( 780 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 35L ) ) break;
	hb_xvmSetLine( 781 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 12L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 36L ) ) break;
	hb_xvmSetLine( 782 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 34L ) ) break;
	hb_xvmSetLine( 783 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 14L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 41L ) ) break;
	hb_xvmSetLine( 784 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 15L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 260L ) ) break;
	hb_xvmSetLine( 785 );
	hb_xvmPushFuncSymbol( symbols + 138 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 786 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 207L ) ) break;
	hb_xvmSetLine( 787 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 199L ) ) break;
	hb_xvmSetLine( 788 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 46L ) ) break;
	hb_xvmSetLine( 789 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 212L ) ) break;
	hb_xvmSetLine( 790 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 21L ) ) break;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 213L ) ) break;
	hb_xvmSetLine( 792 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SAVESETTINGS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 799 );
	hb_xvmPushStatic( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopStatic( 10 );
	hb_xvmSetLine( 800 );
	hb_xvmPushFuncSymbol( symbols + 139 );
	hb_xvmPushStringConst( "HMG Debugger Files", 18 );
	hb_xvmPushStringConst( "*.dbg", 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushStringConst( "HMG Debugger: Save Settings", 27 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 801 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 802 );
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 803 );
	hb_xvmPushSymbol( symbols + 140 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 805 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 10 );
	hb_xvmSetLine( 807 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( LOADSETTINGS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 814 );
	hb_xvmPushStatic( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopStatic( 11 );
	hb_xvmSetLine( 815 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushStringConst( "HMG Debugger Files", 18 );
	hb_xvmPushStringConst( "*.dbg", 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushStringConst( "HMG Debugger: Load Settings", 27 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 816 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 817 );
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 818 );
	hb_xvmPushFuncSymbol( symbols + 100 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 820 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 11 );
	hb_xvmSetLine( 822 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RESTORESETTINGS )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 827 );
	hb_xvmPushSymbol( symbols + 142 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 828 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config1", 16 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 829 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config2", 16 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushSymbol( symbols + 143 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 830 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config3", 16 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushSymbol( symbols + 144 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 831 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Spinner_Config", 14 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushSymbol( symbols + 145 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 833 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( QUIT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 840 );
	hb_xvmPushStatic( 12 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopStatic( 12 );
	hb_xvmSetLine( 841 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Are you sure you want to EXIT the program \?", 43 );
	hb_xvmPushStringConst( "HMG Debugger", 12 );
	hb_xvmPushInteger( 4225 );
	hb_xvmPushInteger( 32001 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 842 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 843 );
	hb_xvmPushSymbol( symbols + 146 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 845 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 12 );
	hb_xvmSetLine( 847 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( REPAINTGRIDROW )
{
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 852 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ITEM", 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 853 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ITEM", 4 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 855 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TOGGLEBREAKPOINT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 861 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 862 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 863 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "ITEM", 4 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 865 );
	hb_xvmPushStatic( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopStatic( 13 );
	hb_xvmSetLine( 866 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 867 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 868 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Toggle BreakPoint : First select the line in the source code.", 61 );
	hb_xvmPushStringConst( "HMG Debugger", 12 );
	hb_xvmPushInteger( 4224 );
	hb_xvmPushInteger( 32001 );
	if( hb_xvmDo( 5 ) ) break;
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 869 );
	hb_xvmPushSymbol( symbols + 148 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 870 );
	hb_xvmPushSymbol( symbols + 149 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 871 );
	hb_xvmPushFuncSymbol( symbols + 147 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 873 );
	hb_xvmPushStringConst( "Toggle BreakPoint : Invalid line of code ( # ", 45 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " )", 2 );
	hb_xvmLocalAdd( 4 );
	hb_xvmSetLine( 874 );
	hb_xvmPushSymbol( symbols + 150 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 875 );
	hb_xvmPushSymbol( symbols + 148 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 876 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 151 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "The next valid line of code is ( # ", 35 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " )", 2 );
	hb_xvmLocalAdd( 4 );
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 878 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 151 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "It does not exist next valid line of code in this file", 54 );
	hb_xvmLocalAdd( 4 );
lab00005: ;
	hb_xvmSetLine( 880 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "HMG Debugger", 12 );
	hb_xvmPushInteger( 4224 );
	hb_xvmPushInteger( 32001 );
	if( hb_xvmDo( 5 ) ) break;
lab00006: ;
	hb_xvmSetLine( 882 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 13 );
	hb_xvmSetLine( 884 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ADDWATCHINI )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 889 );
	hb_xvmPushLocal( 1 );
	hb_xvmPopStatic( 5 );
	hb_xvmSetLine( 890 );
	hb_xvmPushStatic( 5 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushStatic( 5 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
lab00001: ;
	hb_xvmSetLine( 891 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 892 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 894 );
	hb_xvmPushFuncSymbol( symbols + 153 );
	if( hb_xvmDo( 0 ) ) break;
lab00003: ;
	hb_xvmSetLine( 896 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 897 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushStringConst( "New", 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 898 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchType", 15 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushStatic( 5 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushStringConst( "WatchPoint", 10 );
	goto lab00005;
lab00004: ;
	hb_xvmPushStringConst( "TracePoint", 10 );
lab00005: ;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 899 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 900 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
lab00006: ;
	hb_xvmSetLine( 903 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TOCURSOR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 909 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 910 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 911 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "ITEM", 4 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 913 );
	hb_xvmPushStatic( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopStatic( 14 );
	hb_xvmSetLine( 914 );
	hb_xvmPushSymbol( symbols + 148 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 915 );
	hb_xvmPushSymbol( symbols + 154 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	goto lab00005;
lab00002: ;
	hb_xvmSetLine( 917 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 918 );
	hb_xvmPushStringConst( "To Cursor : Invalid line of code ( # ", 37 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " )", 2 );
	hb_xvmLocalAdd( 4 );
	hb_xvmSetLine( 919 );
	hb_xvmPushSymbol( symbols + 150 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 920 );
	hb_xvmPushSymbol( symbols + 148 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 921 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 151 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "The next valid line of code is ( # ", 35 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " )", 2 );
	hb_xvmLocalAdd( 4 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 923 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 151 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "It does not exist next valid line of code in this file", 54 );
	hb_xvmLocalAdd( 4 );
lab00004: ;
	hb_xvmSetLine( 925 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "HMG Debugger", 12 );
	hb_xvmPushInteger( 4224 );
	hb_xvmPushInteger( 32001 );
	if( hb_xvmDo( 5 ) ) break;
lab00005: ;
	hb_xvmSetLine( 927 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 14 );
	hb_xvmSetLine( 929 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( EVALUATEEXP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 935 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 936 );
	hb_xvmPushSymbol( symbols + 156 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 937 );
	hb_xvmPushFuncSymbol( symbols + 157 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 938 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "AddItem", 7 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 4 ) ) break;
lab00001: ;
	hb_xvmSetLine( 941 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ONKEYPRESS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 1 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 949 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 953 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 954 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 955 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00021;
lab00001: ;
	hb_xvmSetLine( 956 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 27L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 957 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushStatic( 6 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 958 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00021;
lab00002: ;
	hb_xvmSetLine( 961 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 962 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 963 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 964 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 965 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00021;
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 966 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 967 );
	hb_xvmPushFuncSymbol( symbols + 159 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 968 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 969 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 970 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00021;
lab00003: ;
	hb_xvmSetLine( 974 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 976 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 977 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 978 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "wp,tp", 5 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 979 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 980 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchType", 15 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "wp", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushStringConst( "WatchPoint", 10 );
	goto lab00005;
lab00004: ;
	hb_xvmPushStringConst( "TracePoint", 10 );
lab00005: ;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 981 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 982 );
	hb_xvmPushInteger( 4 );
	hb_xvmPopStatic( 5 );
	hb_xvmSetLine( 983 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "ENABLED", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 984 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 985 );
	hb_xvmPushFuncSymbol( symbols + 160 );
	hb_xvmPushInteger( 35 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00021;
lab00006: ;
	hb_xvmSetLine( 987 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 46L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 988 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Are sure you want to DELETE the item # ", 39 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " \?", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "HMG Debugger", 12 );
	hb_xvmPushInteger( 4225 );
	hb_xvmPushInteger( 32001 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 989 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 990 );
	hb_xvmPushSymbol( symbols + 161 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 991 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "DeleteItem", 10 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00007: ;
	hb_xvmSetLine( 993 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00021;
lab00008: ;
	hb_xvmSetLine( 996 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 997 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 998 );
	hb_xvmPushFuncSymbol( symbols + 159 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 999 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmPushStatic( 5 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmPushStatic( 5 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmPushStatic( 5 );
	if( hb_xvmEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
lab00009: ;
	hb_xvmSetLine( 1000 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 1001 );
	hb_xvmPushStatic( 5 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 1002 );
	hb_xvmPushSymbol( symbols + 162 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00012;
lab00010: ;
	hb_xvmSetLine( 1003 );
	hb_xvmPushStatic( 5 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 1004 );
	hb_xvmPushSymbol( symbols + 163 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00012;
lab00011: ;
	hb_xvmSetLine( 1005 );
	hb_xvmPushStatic( 5 );
	if( hb_xvmEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmPushLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 1006 );
	hb_xvmPushSymbol( symbols + 164 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00012: ;
	hb_xvmSetLine( 1008 );
	hb_xvmPushFuncSymbol( symbols + 153 );
	if( hb_xvmDo( 0 ) ) break;
lab00013: ;
	hb_xvmSetLine( 1011 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 27L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
lab00014: ;
	hb_xvmSetLine( 1012 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1013 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchType", 15 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1014 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1015 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1016 );
	hb_xvmPushInteger( 0 );
	hb_xvmPopStatic( 5 );
	goto lab00021;
lab00015: ;
	hb_xvmSetLine( 1017 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 46L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 1018 );
	hb_xvmPushFuncSymbol( symbols + 165 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 256 );
	hb_xvmPushInteger( 46 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1019 );
	hb_xvmPushFuncSymbol( symbols + 165 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 257 );
	hb_xvmPushInteger( 46 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00021;
lab00016: ;
	hb_xvmSetLine( 1022 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 1023 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00017;
	hb_xvmSetLine( 1024 );
	hb_xvmPushFuncSymbol( symbols + 155 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1025 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00021;
lab00017: ;
	hb_xvmSetLine( 1026 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 46L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 1027 );
	hb_xvmPushFuncSymbol( symbols + 165 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 256 );
	hb_xvmPushInteger( 46 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1028 );
	hb_xvmPushFuncSymbol( symbols + 165 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 257 );
	hb_xvmPushInteger( 46 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00021;
lab00018: ;
	hb_xvmSetLine( 1031 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 1032 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00019;
	hb_xvmSetLine( 1033 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1034 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00021;
lab00019: ;
	hb_xvmSetLine( 1035 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 46L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 1036 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "DeleteItem", 10 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1037 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00021;
lab00020: ;
	hb_xvmSetLine( 1040 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 1041 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 1042 );
	hb_xvmPushFuncSymbol( symbols + 120 );
	if( hb_xvmDo( 0 ) ) break;
lab00021: ;
	hb_xvmSetLine( 1047 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ONKEYPRESS_TRANSPARENCY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 1062 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 107L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 1064 );
	hb_xvmPushStaticByRef( 15 );
	hb_xvmPushInteger( 50 );
	if( hb_xvmMinusEqPop() ) break;
	hb_xvmSetLine( 1065 );
	hb_xvmPushStatic( 15 );
	if( hb_xvmLessThenIntIs( 50L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushInteger( 50 );
	goto lab00002;
lab00001: ;
	hb_xvmPushStatic( 15 );
lab00002: ;
	hb_xvmPopStatic( 15 );
	hb_xvmSetLine( 1066 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 1067 );
	hb_xvmPushFuncSymbol( symbols + 166 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1068 );
	hb_xvmPushFuncSymbol( symbols + 167 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushStatic( 15 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1066 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	goto lab00010;
lab00005: ;
	hb_xvmSetLine( 1071 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 109L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 1073 );
	hb_xvmPushStaticByRef( 15 );
	hb_xvmPushInteger( 50 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 1074 );
	hb_xvmPushStatic( 15 );
	if( hb_xvmGreaterThenIntIs( 255L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmPushInteger( 255 );
	goto lab00007;
lab00006: ;
	hb_xvmPushStatic( 15 );
lab00007: ;
	hb_xvmPopStatic( 15 );
	hb_xvmSetLine( 1075 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 1076 );
	hb_xvmPushFuncSymbol( symbols + 166 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1077 );
	hb_xvmPushFuncSymbol( symbols + 167 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushStatic( 15 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1075 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00009: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00010: ;
	hb_xvmSetLine( 1082 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ENABLECONFIG )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 1086 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1087 );
	hb_xvmPushStatic( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1088 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_11", 9 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 1089 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Setting", 12 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1091 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Setting", 12 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 1092 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Menu_Setting", 12 );
	hb_xvmPushStringConst( "CHECKED", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1093 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_11", 9 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	hb_xvmSetLine( 1095 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushInteger( 7 );
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 1097 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config1", 16 );
	hb_xvmPushStringConst( "ENABLED", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1098 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config2", 16 );
	hb_xvmPushStringConst( "ENABLED", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1099 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config3", 16 );
	hb_xvmPushStringConst( "ENABLED", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1100 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Config", 12 );
	hb_xvmPushStringConst( "ENABLED", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1101 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Spinner_Config", 14 );
	hb_xvmPushStringConst( "ENABLED", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1102 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Config1", 14 );
	hb_xvmPushStringConst( "ENABLED", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1103 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Config2", 14 );
	hb_xvmPushStringConst( "ENABLED", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1105 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETFORECOLORSOURCECODE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 1111 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushStatic( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1112 );
	hb_xvmPushSymbol( symbols + 168 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1113 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 1114 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1115 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 1113 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1119 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 224L ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 128 );
	hb_xvmArrayGen( 3 );
	goto lab00005;
lab00004: ;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
lab00005: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETFORECOLORWATCH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 1124 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 128 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1125 );
	hb_xvmPushSymbol( symbols + 169 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1126 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "tp", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1127 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 1130 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETFORECOLORVARS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1135 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 128 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1136 );
	hb_xvmPushFuncSymbol( symbols + 170 );
	hb_xvmPushLocalByRef( 1 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 223L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1138 );
	hb_xvmPushStringConst( "Public", 6 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 34 );
	hb_xvmPushInteger( 139 );
	hb_xvmPushInteger( 34 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 1139 );
	hb_xvmPushStringConst( "Private", 7 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 139 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 1140 );
	hb_xvmPushStringConst( "Static", 6 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 69 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 1141 );
	hb_xvmPushStringConst( "Local", 5 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushInteger( 160 );
	hb_xvmPushInteger( 32 );
	hb_xvmPushInteger( 240 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 1144 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( AJUSTCONTROLSIZE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 1152 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_1", 7 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushStatic( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "SplitBox", 8 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 0 );
lab00002: ;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "\x1F\x85\xEB" "Q\xB8\x1E\xD5\?", 10, 2 );
	if( hb_xvmMult() ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1155 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	hb_xvmPushStatic( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "SplitBox", 8 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushInteger( 0 );
lab00004: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1156 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1157 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 3L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1158 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 4L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1161 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDec() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 46 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1162 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDec() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 45 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1165 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Image_1", 7 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Image_1", 7 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1166 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Image_1", 7 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Image_1", 7 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1169 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00007;
lab00005: ;
	hb_xvmSetLine( 1170 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1171 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 1172 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1173 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1174 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00006: ;
	hb_xvmSetLine( 1176 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00\x04@", 10, 1 );
	if( hb_xvmMult() ) break;
	hb_xvmLocalAdd( 2 );
	hb_xvmSetLine( 1177 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1178 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00\x04@", 10, 1 );
	if( hb_xvmMult() ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1179 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "HEIGHT", 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1169 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00007: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 1183 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1186 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1187 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1188 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1189 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1191 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchType", 15 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1192 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchType", 15 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1194 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1195 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchType", 15 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchType", 15 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1196 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Watch", 13 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchNro", 14 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_WatchType", 15 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1199 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1200 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1201 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Calc", 10 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1202 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Calc", 10 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1203 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Calc", 10 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Calc", 9 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1204 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Calc", 10 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Calc", 10 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1205 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Calc", 10 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1208 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1211 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1212 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1213 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1214 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1215 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1218 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1219 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config1", 16 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00\xF8\?", 10, 2 );
	if( hb_xvmMult() ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1220 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config1", 16 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1221 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config2", 16 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00\x10@", 10, 2 );
	if( hb_xvmMult() ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1222 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config2", 16 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1223 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config3", 16 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00\x1A@", 10, 2 );
	if( hb_xvmMult() ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1224 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config3", 16 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1225 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Config", 12 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00#@", 10, 2 );
	if( hb_xvmMult() ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1226 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Config", 12 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1227 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Spinner_Config", 14 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Config", 12 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1228 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Spinner_Config", 14 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Config", 12 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_Config", 12 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1229 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Config1", 14 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config1", 16 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1230 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Config1", 14 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Config1", 14 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1231 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Config2", 14 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "CheckBox_Config2", 16 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1232 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Config2", 14 );
	hb_xvmPushStringConst( "COL", 3 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "COL", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Frame_Config", 12 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Config2", 14 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1234 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ADJUSTCTRLINTAB )
{
   do {
	hb_xvmFrame( 7, 3 );
	hb_xvmSetLine( 1240 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1241 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WIDTH", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1242 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "HEIGHT", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 1243 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 5 );
	hb_xvmSetLine( 1244 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 4 );
	hb_xvmSetLine( 1245 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 45 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1246 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 46 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 3L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1247 );
	hb_xvmPushFuncSymbol( symbols + 173 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 1249 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( UPDATEGRIDS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 1258 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 8L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 1259 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "HIDE", 4 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00012;
lab00001: ;
	hb_xvmSetLine( 1261 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 1262 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "HIDE", 4 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1263 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopStatic( 6 );
	hb_xvmSetLine( 1264 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00005;
lab00002: ;
	hb_xvmSetLine( 1265 );
	hb_xvmPushStatic( 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1266 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "SHOW", 4 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 1268 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "HIDE", 4 );
	if( hb_xvmDo( 3 ) ) break;
lab00004: ;
	hb_xvmSetLine( 1264 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00005: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	goto lab00012;
lab00006: ;
	hb_xvmSetLine( 1272 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 1273 );
	hb_xvmPushFuncSymbol( symbols + 174 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1274 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "SHOW", 4 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1275 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00012;
lab00007: ;
	hb_xvmSetLine( 1277 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 1278 );
	hb_xvmPushFuncSymbol( symbols + 153 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1279 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "SHOW", 4 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1280 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00012;
lab00008: ;
	hb_xvmSetLine( 1282 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 1283 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "HIDE", 4 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1284 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "TextBox_Calc", 12 );
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00012;
lab00009: ;
	hb_xvmSetLine( 1286 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 1287 );
	hb_xvmPushFuncSymbol( symbols + 175 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1288 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "SHOW", 4 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00012;
lab00010: ;
	hb_xvmSetLine( 1290 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 6L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 1291 );
	hb_xvmPushFuncSymbol( symbols + 176 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1292 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1293 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "SHOW", 4 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1294 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "SETFOCUS", 8 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00012;
lab00011: ;
	hb_xvmSetLine( 1296 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Tab_1", 5 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 7L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 1297 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Button_Refresh", 14 );
	hb_xvmPushStringConst( "HIDE", 4 );
	if( hb_xvmDo( 3 ) ) break;
lab00012: ;
	hb_xvmSetLine( 1301 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( UPDATEGRIDCALLSTACK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 0 );
	hb_xvmSetLine( 1307 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1308 );
	hb_xvmPushSymbol( symbols + 177 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1309 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "DisableUpdate", 13 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1310 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "DeleteAllItems", 14 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1311 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1312 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "AddItem", 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1311 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00002: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1314 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "EnableUpdate", 12 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1315 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1316 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_CallStack", 14 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1318 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( UPDATEGRIDVARS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 1324 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopStatic( 3 );
	hb_xvmSetLine( 1325 );
	hb_xvmPushSymbol( symbols + 178 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1326 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1327 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "DisableUpdate", 13 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1328 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "DeleteAllItems", 14 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1329 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 1330 );
	hb_xvmPushStringConst( "_HMG_", 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1331 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "AddItem", 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1332 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushStatic( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 1334 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1329 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1336 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "ColumnAUTOFIT", 13 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1337 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "ColumnAUTOFIT", 13 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1338 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "ColumnAUTOFIT", 13 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1339 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "EnableUpdate", 12 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1340 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1341 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Vars", 9 );
	hb_xvmPushStringConst( "Refresh", 7 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1343 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( UPDATEGRIDWATCH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 1349 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "DisableUpdate", 13 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1350 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "DeleteAllItems", 14 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1351 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 1352 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1353 );
	hb_xvmPushFuncSymbol( symbols + 180 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushFuncSymbol( symbols + 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1354 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPushStringConst( ".T.", 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushStringConst( "Yes", 3 );
	goto lab00003;
lab00002: ;
	hb_xvmPushStringConst( "No", 2 );
lab00003: ;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1355 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "AddItem", 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1351 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00004: ;
	hb_xvmPushSymbol( symbols + 182 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1357 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "EnableUpdate", 12 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1358 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Watch", 10 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1360 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( UPDATEGRIDAREAS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 1365 );
	hb_xvmPushSymbol( symbols + 183 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1366 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "DisableUpdate", 13 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1367 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "DeleteAllItems", 14 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1368 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1369 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "AddItem", 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1368 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00002: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1371 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "ColumnsAutoFitH", 15 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1372 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "EnableUpdate", 12 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1373 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1375 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( UPDATEGRIDREC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 0 );
	hb_xvmSetLine( 1381 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Areas", 10 );
	hb_xvmPushStringConst( "VALUE", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1382 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1383 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 1385 );
	hb_xvmPushStringConst( "*", 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1386 );
	hb_xvmPushFuncSymbol( symbols + 184 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
lab00002: ;
	hb_xvmSetLine( 1388 );
	hb_xvmPushSymbol( symbols + 185 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1389 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "DisableUpdate", 13 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1390 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "DeleteAllItems", 14 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1391 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 1392 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "AddItem", 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1391 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1394 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 1395 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "ColumnAutoFit", 13 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 1397 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "ColumnAutoFitH", 14 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 4 ) ) break;
lab00006: ;
	hb_xvmSetLine( 1399 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "ColumnAutoFitH", 14 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1400 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "ColumnAutoFitH", 14 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1401 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 1402 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "ColumnAutoFit", 13 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 1404 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "ColumnAutoFitH", 14 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmDo( 4 ) ) break;
lab00008: ;
	hb_xvmSetLine( 1406 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "EnableUpdate", 12 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1407 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Grid_Rec", 8 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1409 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( LOADPRG )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 1415 );
	hb_xvmPushSymbol( symbols + 186 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1416 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "DisableUpdate", 13 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1417 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00001: ;
	hb_xvmSetLine( 1418 );
	hb_xvmPushSymbol( symbols + 187 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1419 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 1420 );
	hb_xvmPushFuncSymbol( symbols + 188 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 1422 );
	hb_xvmPushFuncSymbol( symbols + 189 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 190 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 1423 );
	hb_xvmPushFuncSymbol( symbols + 191 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushNil();
	hb_xvmPushStringConst( "UTF8", 4 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 1424 );
	hb_xvmPushFuncSymbol( symbols + 189 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmPushFuncSymbol( symbols + 190 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1425 );
	hb_xvmPushFuncSymbol( symbols + 192 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushNil();
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00004: ;
	hb_xvmSetLine( 1427 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "AddItem", 7 );
	hb_xvmPushFuncSymbol( symbols + 181 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 7 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 193 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1428 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00005: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 1429 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "ColumnsAUTOFIT", 14 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1430 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "EnableUpdate", 12 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1431 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1433 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMG_ISCURRENTCODEPAGEUNICODE )
{
   do {
	hb_xvmSetLine( 1437 );
	hb_xvmPushFuncSymbol( symbols + 194 );
	hb_xvmPushInteger( 114 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "UTF8", 4 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_ACTIVATEMAINWINDOWFIRST )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1442 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 444L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1443 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1444 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPop( 444L ) ) break;
lab00001: ;
	hb_xvmSetLine( 1447 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( UPDATEINFO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 1454 );
	hb_xvmPushSymbol( symbols + 195 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 1455 );
	hb_xvmPushSymbol( symbols + 195 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1456 );
	hb_xvmPushSymbol( symbols + 195 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1457 );
	hb_xvmPushSymbol( symbols + 195 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1458 );
	hb_xvmPushSymbol( symbols + 195 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1459 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1461 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_1", 7 );
	hb_xvmPushStringConst( "FONTCOLOR", 9 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
lab00002: ;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1462 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "Label_1", 7 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushSymbol( symbols + 195 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1464 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 1465 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 6 );
lab00003: ;
	hb_xvmSetLine( 1468 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushStatic( 4 );
	hb_xvmPushStatic( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 1469 );
	hb_xvmPushFuncSymbol( symbols + 147 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushStatic( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1471 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopStatic( 16 );
	hb_xvmSetLine( 1472 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStringConst( "ComboBox_SourceCode", 19 );
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushStatic( 16 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1473 );
	hb_xvmPushStatic( 16 );
	hb_xvmPopStatic( 6 );
	hb_xvmSetLine( 1475 );
	hb_xvmPushLocal( 4 );
	hb_xvmPopStatic( 2 );
	hb_xvmSetLine( 1476 );
	hb_xvmPushFuncSymbol( symbols + 196 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushStatic( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1478 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushStatic( 4 );
	hb_xvmPushStatic( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 1479 );
	hb_xvmPushFuncSymbol( symbols + 147 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushStatic( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1481 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1482 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_FormDebugger", 17 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushStatic( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "VALUE", 5 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00004: ;
	hb_xvmSetLine( 1485 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1487 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_SHOWEVENTMONITOR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmSetLine( 1495 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1496 );
	hb_xvmPushFuncSymbol( symbols + 197 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1495 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00002: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1503 );
	hb_xvmPushSymbol( symbols + 198 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 1504 );
	hb_xvmPushSymbol( symbols + 103 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 1505 );
	hb_xvmPushSymbol( symbols + 199 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 1506 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 1507 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1506 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	goto lab00008;
lab00005: ;
	hb_xvmSetLine( 1510 );
	hb_xvmPushSymbol( symbols + 109 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 1511 );
	hb_xvmPushSymbol( symbols + 199 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 1512 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1513 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00007;
lab00006: ;
	hb_xvmSetLine( 1514 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1513 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00007: ;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
lab00008: ;
	hb_xvmSetLine( 1518 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETSPLITBOXHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1524 );
	hb_xvmPushFuncSymbol( symbols + 200 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1525 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 1526 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 1529 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_ISUTF8WITHBOM )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1535 );
	hb_xvmPushFuncSymbol( symbols + 202 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushStringConst( "\xEF\xBB\xBF", 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "\xEF\xBB\xBF", 3 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_UTF8REMOVEBOM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1540 );
	hb_xvmPushFuncSymbol( symbols + 201 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1541 );
	hb_xvmPushFuncSymbol( symbols + 204 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	hb_xvmPushStringConst( "\xEF\xBB\xBF", 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 1544 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 205, 16 );
	hb_xvmSFrame( symbols + 205 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopStatic( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPopStatic( 2 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopStatic( 3 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopStatic( 4 );
	hb_xvmPushInteger( 0 );
	hb_xvmPopStatic( 5 );
	hb_xvmPushInteger( 1 );
	hb_xvmPopStatic( 6 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopStatic( 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPopStatic( 8 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 9 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 10 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 11 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 12 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 13 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 14 );
	hb_xvmPushInteger( 255 );
	hb_xvmPopStatic( 15 );
	hb_xvmPushInteger( 1 );
	hb_xvmPopStatic( 16 );
	/* *** END PROC *** */
   } while( 0 );
}

#line 1548 "dbgGUI.prg"

#include <mgdefs.h>
#include <commctrl.h>

HB_FUNC ( TABCTRL_ADJUSTRECT )
{
   HWND hWnd    = (HWND) HB_PARNL (1);
   BOOL fLarger = (BOOL) hb_parl (2);
   RECT Rect = {0,0,0,0};

   TabCtrl_AdjustRect( hWnd, fLarger, &Rect );

   hb_reta (4);
   hb_storvnl (Rect.left,   -1, 1);
   hb_storvnl (Rect.top,    -1, 2);
   hb_storvnl (Rect.right,  -1, 3);
   hb_storvnl (Rect.bottom, -1, 4);
}

HB_FUNC (GETDESKTOPREALTOP)
{
   RECT Rect;
   SystemParametersInfo ( SPI_GETWORKAREA, 0, &Rect, 0 );
   hb_retni ( Rect.top );
}

HB_FUNC (GETDESKTOPREALLEFT)
{
   RECT Rect;
   SystemParametersInfo ( SPI_GETWORKAREA, 0, &Rect, 0 );
   hb_retni ( Rect.left );
}

