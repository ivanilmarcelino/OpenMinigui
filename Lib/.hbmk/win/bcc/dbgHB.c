/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "dbgHB.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( __DBGENTRY );
HB_FUNC_EXTERN( __DBGSETENTRY );
HB_FUNC( HMGDEBUGGER );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_STATIC( HMGDEBUGGER_NEW );
HB_FUNC_STATIC( HMGDEBUGGER_ACTIVATE );
HB_FUNC_STATIC( HMGDEBUGGER_LOADCALLSTACK );
HB_FUNC_STATIC( HMGDEBUGGER_HANDLEEVENT );
HB_FUNC_STATIC( HMGDEBUGGER_GO );
HB_FUNC_STATIC( HMGDEBUGGER_STEP );
HB_FUNC_STATIC( HMGDEBUGGER_ANIMATE );
HB_FUNC_STATIC( HMGDEBUGGER_PAUSE );
HB_FUNC_STATIC( HMGDEBUGGER_TRACE );
HB_FUNC_STATIC( HMGDEBUGGER_SETCBTRACE );
HB_FUNC_STATIC( HMGDEBUGGER_SETNEXTROUTINE );
HB_FUNC_STATIC( HMGDEBUGGER_GETSOURCEFILES );
HB_FUNC_STATIC( HMGDEBUGGER_GETNEXTVALIDSTOPLINE );
HB_FUNC_STATIC( HMGDEBUGGER_GETNEXTVALIDSTOPLINEEX );
HB_FUNC_STATIC( HMGDEBUGGER_ISVALIDSTOPLINE );
HB_FUNC_STATIC( HMGDEBUGGER_SETTOCURSOR );
HB_FUNC_STATIC( HMGDEBUGGER_QUIT );
HB_FUNC_STATIC( HMGDEBUGGER_EXIT );
HB_FUNC_STATIC( HMGDEBUGGER_BREAKPOINTCOUNT );
HB_FUNC_STATIC( HMGDEBUGGER_ISBREAKPOINT );
HB_FUNC_STATIC( HMGDEBUGGER_BREAKPOINTTOGGLE );
HB_FUNC_STATIC( HMGDEBUGGER_BREAKPOINTDELETE );
HB_FUNC_STATIC( HMGDEBUGGER_BREAKPOINTDELETEALL );
HB_FUNC_STATIC( HMGDEBUGGER_BREAKPOINTADDFUNC );
HB_FUNC_STATIC( HMGDEBUGGER_GETEXPRVALUE );
HB_FUNC_STATIC( HMGDEBUGGER_WATCHCOUNT );
HB_FUNC_STATIC( HMGDEBUGGER_WATCHDELETEALL );
HB_FUNC_STATIC( HMGDEBUGGER_WATCHDELETE );
HB_FUNC_STATIC( HMGDEBUGGER_WATCHGETINFO );
HB_FUNC_STATIC( HMGDEBUGGER_WATCHSETEXPR );
HB_FUNC_STATIC( HMGDEBUGGER_WATCHPOINTADD );
HB_FUNC_STATIC( HMGDEBUGGER_TRACEPOINTADD );
HB_FUNC_STATIC( HMGDEBUGGER_SETPATHFORFILES );
HB_FUNC_STATIC( HMGDEBUGGER_LOADSOURCEFILE );
HB_FUNC_STATIC( HMGDEBUGGER_GETCODELINEINFO );
HB_FUNC_STATIC( HMGDEBUGGER_DOCOMMAND );
HB_FUNC_STATIC( HMGDEBUGGER_RESTORESETTINGS );
HB_FUNC_STATIC( HMGDEBUGGER_SAVESETTINGS );
HB_FUNC_STATIC( HMGDEBUGGER_VARGETINFO );
HB_FUNC_STATIC( HMGDEBUGGER_VARGETNAME );
HB_FUNC_STATIC( HMGDEBUGGER_VARGETVALTYPE );
HB_FUNC_STATIC( HMGDEBUGGER_VARGETVALUE );
HB_FUNC_STATIC( HMGDEBUGGER_VARSETVALUE );
HB_FUNC_STATIC( HMGDEBUGGER_GETAREAS );
HB_FUNC_STATIC( HMGDEBUGGER_GETREC );
HB_FUNC_STATIC( HMGDEBUGGER_GETARRAYINFO );
HB_FUNC_STATIC( HMGDEBUGGER_GETHASHINFO );
HB_FUNC_STATIC( HMGDEBUGGER_GETOBJECTINFO );
HB_FUNC_STATIC( HMGDEBUGGER_GETBREAKPOINTS );
HB_FUNC_STATIC( HMGDEBUGGER_GETWATCH );
HB_FUNC_STATIC( HMGDEBUGGER_GETVARS );
HB_FUNC_STATIC( HMGDEBUGGER_GETPROCSTACK );
HB_FUNC_EXTERN( PROCINITGUIDEBUGGER );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( GETENV );
HB_FUNC_EXTERN( __DBGPROCLEVEL );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( PROCNAME );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( PROCLINE );
HB_FUNC_EXTERN( HB_MILLISECONDS );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( HB_RELEASECPU );
HB_FUNC( __DBGRESETRUNFLAGS );
HB_FUNC_EXTERN( __DBGSETTRACE );
HB_FUNC_EXTERN( __DBGSETGO );
HB_FUNC_EXTERN( __DBGSETNEXTROUTINE );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( __DBGSETCBTRACE );
HB_FUNC_EXTERN( __DBGGETSOURCEFILES );
HB_FUNC_EXTERN( PROCFILE );
HB_FUNC_EXTERN( __DBGISVALIDSTOPLINE );
HB_FUNC_EXTERN( __DBGSETTOCURSOR );
HB_FUNC_EXTERN( __DBGSETQUIT );
HB_FUNC_EXTERN( __DBGISBREAK );
HB_FUNC_EXTERN( __DBGDELBREAK );
HB_FUNC_EXTERN( __DBGADDBREAK );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( ERRORBLOCK );
HB_FUNC_EXTERN( __BREAKBLOCK );
HB_FUNC_EXTERN( __DBGGETEXPRVALUE );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC( __DBGVALTOSTR );
HB_FUNC_EXTERN( __DBGDELWATCH );
HB_FUNC_EXTERN( HB_ADEL );
HB_FUNC_EXTERN( __DBGSETWATCH );
HB_FUNC_EXTERN( __DBGADDWATCH );
HB_FUNC_EXTERN( AADD );
HB_FUNC_STATIC( __DBGPATHTOARRAY );
HB_FUNC_EXTERN( HB_FILEEXISTS );
HB_FUNC_EXTERN( HB_PS );
HB_FUNC_EXTERN( HB_MEMOREAD );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_STATIC( __DBGTEXTTOARRAY );
HB_FUNC_EXTERN( AT );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( ISDIGIT );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( HB_ASCIIISALPHA );
HB_FUNC_EXTERN( MIN );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC_EXTERN( HB_MEMOWRIT );
HB_FUNC_EXTERN( __DBGVMVARGGET );
HB_FUNC_EXTERN( __DBGVMVARLGET );
HB_FUNC_EXTERN( __DBGVMVARSGET );
HB_FUNC_EXTERN( __DBGVMVARGSET );
HB_FUNC_EXTERN( __DBGVMVARLSET );
HB_FUNC_EXTERN( __DBGVMVARSSET );
HB_FUNC_EXTERN( USED );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( ALIAS );
HB_FUNC_EXTERN( RDDNAME );
HB_FUNC_EXTERN( RECCOUNT );
HB_FUNC_EXTERN( RECNO );
HB_FUNC_EXTERN( BOF );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( DELETED );
HB_FUNC_EXTERN( DBFILTER );
HB_FUNC_EXTERN( ORDNAME );
HB_FUNC_EXTERN( ORDKEY );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( DBSTRUCT );
HB_FUNC_EXTERN( FIELDGET );
HB_FUNC_EXTERN( LTRIM );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( HB_HVALUEAT );
HB_FUNC_EXTERN( HB_HKEYAT );
HB_FUNC_EXTERN( __OBJGETMSGLIST );
HB_FUNC_EXTERN( __OBJGETMETHODLIST );
HB_FUNC_STATIC( __DBGOBJGETVALUE );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( HB_TTOC );
HB_FUNC_EXTERN( BREAK );
HB_FUNC_EXTERN( __DBGSENDMSG );
HB_FUNC_EXTERN( HB_OSPATHLISTSEPARATOR );
HB_FUNC_EXTERN( HB_OSPATHDELIMITERS );
HB_FUNC_EXTERN( RIGHT );
HB_FUNC_EXTERN( HB_STRSHRINK );
HB_FUNC_EXTERN( HB_ATOKENS );
HB_FUNC( HMG_DEBUGGER );
HB_FUNC_EXTERN( __DBGGETBREAKPOINTS );
HB_FUNC_EXTERN( __MVDBGINFO );
HB_FUNC_EXTERN( HB_HALLOCATE );
HB_FUNC_EXTERN( HB_FILEMATCH );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_DBGHB )
{ "__DBGENTRY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "__DBGSETENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGSETENTRY )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HMGDEBUGGER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER )}, NULL },
{ "_PINFO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NPROCLEVEL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ACALLSTACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_AMODULES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ABREAKPOINTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACTIVATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HMGDEBUGGER_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_NEW )}, NULL },
{ "HMGDEBUGGER_ACTIVATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_ACTIVATE )}, NULL },
{ "HMGDEBUGGER_LOADCALLSTACK", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_LOADCALLSTACK )}, NULL },
{ "HMGDEBUGGER_HANDLEEVENT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_HANDLEEVENT )}, NULL },
{ "HMGDEBUGGER_GO", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GO )}, NULL },
{ "HMGDEBUGGER_STEP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_STEP )}, NULL },
{ "HMGDEBUGGER_ANIMATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_ANIMATE )}, NULL },
{ "HMGDEBUGGER_PAUSE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_PAUSE )}, NULL },
{ "HMGDEBUGGER_TRACE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_TRACE )}, NULL },
{ "HMGDEBUGGER_SETCBTRACE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_SETCBTRACE )}, NULL },
{ "HMGDEBUGGER_SETNEXTROUTINE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_SETNEXTROUTINE )}, NULL },
{ "HMGDEBUGGER_GETSOURCEFILES", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETSOURCEFILES )}, NULL },
{ "HMGDEBUGGER_GETNEXTVALIDSTOPLINE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETNEXTVALIDSTOPLINE )}, NULL },
{ "HMGDEBUGGER_GETNEXTVALIDSTOPLINEEX", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETNEXTVALIDSTOPLINEEX )}, NULL },
{ "HMGDEBUGGER_ISVALIDSTOPLINE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_ISVALIDSTOPLINE )}, NULL },
{ "HMGDEBUGGER_SETTOCURSOR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_SETTOCURSOR )}, NULL },
{ "HMGDEBUGGER_QUIT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_QUIT )}, NULL },
{ "HMGDEBUGGER_EXIT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_EXIT )}, NULL },
{ "HMGDEBUGGER_BREAKPOINTCOUNT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_BREAKPOINTCOUNT )}, NULL },
{ "HMGDEBUGGER_ISBREAKPOINT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_ISBREAKPOINT )}, NULL },
{ "HMGDEBUGGER_BREAKPOINTTOGGLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_BREAKPOINTTOGGLE )}, NULL },
{ "HMGDEBUGGER_BREAKPOINTDELETE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_BREAKPOINTDELETE )}, NULL },
{ "HMGDEBUGGER_BREAKPOINTDELETEALL", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_BREAKPOINTDELETEALL )}, NULL },
{ "HMGDEBUGGER_BREAKPOINTADDFUNC", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_BREAKPOINTADDFUNC )}, NULL },
{ "HMGDEBUGGER_GETEXPRVALUE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETEXPRVALUE )}, NULL },
{ "HMGDEBUGGER_WATCHCOUNT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_WATCHCOUNT )}, NULL },
{ "HMGDEBUGGER_WATCHDELETEALL", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_WATCHDELETEALL )}, NULL },
{ "HMGDEBUGGER_WATCHDELETE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_WATCHDELETE )}, NULL },
{ "HMGDEBUGGER_WATCHGETINFO", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_WATCHGETINFO )}, NULL },
{ "HMGDEBUGGER_WATCHSETEXPR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_WATCHSETEXPR )}, NULL },
{ "HMGDEBUGGER_WATCHPOINTADD", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_WATCHPOINTADD )}, NULL },
{ "HMGDEBUGGER_TRACEPOINTADD", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_TRACEPOINTADD )}, NULL },
{ "HMGDEBUGGER_SETPATHFORFILES", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_SETPATHFORFILES )}, NULL },
{ "HMGDEBUGGER_LOADSOURCEFILE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_LOADSOURCEFILE )}, NULL },
{ "HMGDEBUGGER_GETCODELINEINFO", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETCODELINEINFO )}, NULL },
{ "HMGDEBUGGER_DOCOMMAND", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_DOCOMMAND )}, NULL },
{ "HMGDEBUGGER_RESTORESETTINGS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_RESTORESETTINGS )}, NULL },
{ "HMGDEBUGGER_SAVESETTINGS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_SAVESETTINGS )}, NULL },
{ "HMGDEBUGGER_VARGETINFO", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_VARGETINFO )}, NULL },
{ "HMGDEBUGGER_VARGETNAME", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_VARGETNAME )}, NULL },
{ "HMGDEBUGGER_VARGETVALTYPE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_VARGETVALTYPE )}, NULL },
{ "HMGDEBUGGER_VARGETVALUE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_VARGETVALUE )}, NULL },
{ "HMGDEBUGGER_VARSETVALUE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_VARSETVALUE )}, NULL },
{ "HMGDEBUGGER_GETAREAS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETAREAS )}, NULL },
{ "HMGDEBUGGER_GETREC", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETREC )}, NULL },
{ "HMGDEBUGGER_GETARRAYINFO", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETARRAYINFO )}, NULL },
{ "HMGDEBUGGER_GETHASHINFO", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETHASHINFO )}, NULL },
{ "HMGDEBUGGER_GETOBJECTINFO", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETOBJECTINFO )}, NULL },
{ "HMGDEBUGGER_GETBREAKPOINTS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETBREAKPOINTS )}, NULL },
{ "HMGDEBUGGER_GETWATCH", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETWATCH )}, NULL },
{ "HMGDEBUGGER_GETVARS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETVARS )}, NULL },
{ "HMGDEBUGGER_GETPROCSTACK", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMGDEBUGGER_GETPROCSTACK )}, NULL },
{ "PROCINITGUIDEBUGGER", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCINITGUIDEBUGGER )}, NULL },
{ "ADDINLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGUICREATEFORMDEBUGGER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGUIRELEASEFORMDEBUGGER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGUIUPDATEINFO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGUIDOEVENTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGUIRELEASEALLWINDOWS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LGUISHOWMESSAGEBOX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGUIMESSAGEBOX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PINFO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LOADCALLSTACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LACTIVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LACTIVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETPATHFORFILES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETENV", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETENV )}, NULL },
{ "GUICREATEFORMDEBUGGER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HANDLEEVENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_APROCSTACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NPROCLEVEL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGPROCLEVEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGPROCLEVEL )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "ACALLSTACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "APROCSTACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PROCNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCNAME )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "PROCLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCLINE )}, NULL },
{ "HB_MILLISECONDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_MILLISECONDS )}, NULL },
{ "AWATCH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETEXPRVALUE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "ISBREAKPOINT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ABREAKPOINTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ACURRENTLINEINFO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CSETTINGSFILENAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LDEACTIVATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GUIUPDATEINFO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LANIMATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LANIMATESTOPTP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LANIMATESTOPBP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PAUSE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LEXITLOOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LEXITLOOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GUIDOEVENTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_RELEASECPU", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_RELEASECPU )}, NULL },
{ "NSPEED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGRESETRUNFLAGS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( __DBGRESETRUNFLAGS )}, NULL },
{ "_LANIMATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "STEP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGSETTRACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGSETTRACE )}, NULL },
{ "__DBGSETGO", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGSETGO )}, NULL },
{ "__DBGSETNEXTROUTINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGSETNEXTROUTINE )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "__DBGSETCBTRACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGSETCBTRACE )}, NULL },
{ "_LCBTRACE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGGETSOURCEFILES", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGGETSOURCEFILES )}, NULL },
{ "PROCFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCFILE )}, NULL },
{ "__DBGISVALIDSTOPLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGISVALIDSTOPLINE )}, NULL },
{ "GETNEXTVALIDSTOPLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GUIMESSAGEBOX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGSETTOCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGSETTOCURSOR )}, NULL },
{ "__DBGSETQUIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGSETQUIT )}, NULL },
{ "GUIRELEASEALLWINDOWS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETBREAKPOINTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGISBREAK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGISBREAK )}, NULL },
{ "__DBGDELBREAK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGDELBREAK )}, NULL },
{ "ISVALIDSTOPLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGADDBREAK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGADDBREAK )}, NULL },
{ "BREAKPOINTCOUNT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "ERRORBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( ERRORBLOCK )}, NULL },
{ "__BREAKBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __BREAKBLOCK )}, NULL },
{ "__DBGGETEXPRVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGGETEXPRVALUE )}, NULL },
{ "OPERATION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DESCRIPTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "ARGS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "__DBGVALTOSTR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( __DBGVALTOSTR )}, NULL },
{ "__DBGDELWATCH", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGDELWATCH )}, NULL },
{ "HB_ADEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ADEL )}, NULL },
{ "WATCHCOUNT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGSETWATCH", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGSETWATCH )}, NULL },
{ "__DBGADDWATCH", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGADDWATCH )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "_APATHFORFILES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGPATHTOARRAY", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( __DBGPATHTOARRAY )}, NULL },
{ "HB_FILEEXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FILEEXISTS )}, NULL },
{ "APATHFORFILES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_PS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PS )}, NULL },
{ "HB_MEMOREAD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_MEMOREAD )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "NTABWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGTEXTTOARRAY", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( __DBGTEXTTOARRAY )}, NULL },
{ "AT", {HB_FS_PUBLIC}, {HB_FUNCNAME( AT )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "ISDIGIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISDIGIT )}, NULL },
{ "BREAKPOINTTOGGLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "HB_ASCIIISALPHA", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ASCIIISALPHA )}, NULL },
{ "BREAKPOINTADDFUNC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TRACEPOINTADD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "WATCHPOINTADD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETCBTRACE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LANIMATESTOPBP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LANIMATESTOPTP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NSPEED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "DOCOMMAND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CSETTINGSFILENAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "LCBTRACE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_MEMOWRIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_MEMOWRIT )}, NULL },
{ "VARGETVALUE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__DBGVMVARGGET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGVMVARGGET )}, NULL },
{ "__DBGVMVARLGET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGVMVARLGET )}, NULL },
{ "__DBGVMVARSGET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGVMVARSGET )}, NULL },
{ "__DBGVMVARGSET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGVMVARGSET )}, NULL },
{ "__DBGVMVARLSET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGVMVARLSET )}, NULL },
{ "__DBGVMVARSSET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGVMVARSSET )}, NULL },
{ "USED", {HB_FS_PUBLIC}, {HB_FUNCNAME( USED )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "ALIAS", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALIAS )}, NULL },
{ "RDDNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( RDDNAME )}, NULL },
{ "RECCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RECCOUNT )}, NULL },
{ "RECNO", {HB_FS_PUBLIC}, {HB_FUNCNAME( RECNO )}, NULL },
{ "BOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( BOF )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "DELETED", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELETED )}, NULL },
{ "DBFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBFILTER )}, NULL },
{ "ORDNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDNAME )}, NULL },
{ "ORDKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDKEY )}, NULL },
{ "SELECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SELECT )}, NULL },
{ "DBSTRUCT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSTRUCT )}, NULL },
{ "FIELDGET", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDGET )}, NULL },
{ "LTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( LTRIM )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "HB_HVALUEAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_HVALUEAT )}, NULL },
{ "HB_HKEYAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_HKEYAT )}, NULL },
{ "__OBJGETMSGLIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJGETMSGLIST )}, NULL },
{ "__OBJGETMETHODLIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJGETMETHODLIST )}, NULL },
{ "__DBGOBJGETVALUE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( __DBGOBJGETVALUE )}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "HB_TTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_TTOC )}, NULL },
{ "CLASSNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BREAK", {HB_FS_PUBLIC}, {HB_FUNCNAME( BREAK )}, NULL },
{ "__DBGSENDMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGSENDMSG )}, NULL },
{ "HB_OSPATHLISTSEPARATOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_OSPATHLISTSEPARATOR )}, NULL },
{ "HB_OSPATHDELIMITERS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_OSPATHDELIMITERS )}, NULL },
{ "RIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RIGHT )}, NULL },
{ "HB_STRSHRINK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_STRSHRINK )}, NULL },
{ "HB_ATOKENS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ATOKENS )}, NULL },
{ "HMG_DEBUGGER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_DEBUGGER )}, NULL },
{ "__DBGGETBREAKPOINTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGGETBREAKPOINTS )}, NULL },
{ "__MVDBGINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVDBGINFO )}, NULL },
{ "HB_HALLOCATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_HALLOCATE )}, NULL },
{ "HB_FILEMATCH", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FILEMATCH )}, NULL },
{ "AMODULES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "(_INITSTATICS00003)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_DBGHB, "dbgHB.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_DBGHB
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_DBGHB )
   #include "hbiniseg.h"
#endif

HB_FUNC( __DBGENTRY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 6 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 98 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 6L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 102 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 7L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 104 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 105 );
	hb_xvmPushSymbol( symbols + 2 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopStatic( 1 );
	hb_xvmSetLine( 106 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 109 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 110 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 111 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 112 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 114 );
	hb_xvmPushSymbol( symbols + 9 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 118 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMGDEBUGGER )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 155 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushStaticByRef( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSeqAlways();
	do {
	hb_xvmLocalSetInt( 1, 1L );
	hb_xvmPushSymbol( symbols + 2 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "HMGDebugger", 11 );
	hb_xvmArrayGen( 0 );
	hb_xvmPushSymbol( symbols + 3 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 157 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "pInfo", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 159 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aCallStack", 10 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 160 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aProcStack", 10 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 161 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nProcLevel", 10 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 162 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aModules", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 163 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aBreakPoints", 12 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 164 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aWatch", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 165 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aCurrentLineInfo", 16 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 167 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lExitLoop", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 169 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "init.dbg", 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cSettingsFileName", 17 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 170 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aPathForFiles", 13 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 172 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nTabWidth", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 174 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lAnimate", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 175 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lAnimateStopBP", 14 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 176 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lAnimateStopTP", 14 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 177 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lCBTrace", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 178 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nSpeed", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 180 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lActive", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 181 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lDeactivate", 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 183 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 184 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Activate", 8 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 185 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LoadCallStack", 13 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 186 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "HandleEvent", 11 );
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 188 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Go", 2 );
	hb_xvmPushSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 189 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Step", 4 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 190 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Animate", 7 );
	hb_xvmPushSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 191 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Pause", 5 );
	hb_xvmPushSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 192 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Trace", 5 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 193 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SetCBTrace", 10 );
	hb_xvmPushSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 194 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SetNextRoutine", 14 );
	hb_xvmPushSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 195 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetSourceFiles", 14 );
	hb_xvmPushSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 196 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetNextValidStopLine", 20 );
	hb_xvmPushSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 197 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetNextValidStopLineEx", 22 );
	hb_xvmPushSymbol( symbols + 27 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 198 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "IsValidStopLine", 15 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 199 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SetToCursor", 11 );
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 200 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Quit", 4 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 201 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Exit", 4 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 203 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "BreakPointCount", 15 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 204 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "IsBreakPoint", 12 );
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 205 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "BreakPointToggle", 16 );
	hb_xvmPushSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 206 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "BreakPointDelete", 16 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 207 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "BreakPointDeleteAll", 19 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 208 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "BreakPointAddFunc", 17 );
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 210 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetExprValue", 12 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 211 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WatchCount", 10 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 212 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WatchDeleteAll", 14 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 213 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WatchDelete", 11 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 214 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WatchGetInfo", 12 );
	hb_xvmPushSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 215 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WatchSetExpr", 12 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 216 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WatchPointAdd", 13 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 217 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "TracepointAdd", 13 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 219 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SetPathForFiles", 15 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 220 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LoadSourceFile", 14 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 221 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetCodeLineInfo", 15 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 223 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "DoCommand", 9 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 224 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "RestoreSettings", 15 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 225 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SaveSettings", 12 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 227 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VarGetInfo", 10 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 228 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VarGetName", 10 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 229 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VarGetValType", 13 );
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 230 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VarGetValue", 11 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 231 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VarSetValue", 11 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 233 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetAreas", 8 );
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 234 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetRec", 6 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 235 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetArrayInfo", 12 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 236 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetHashInfo", 11 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 237 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetObjectInfo", 13 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 239 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetBreakPoints", 14 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 240 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetWatch", 8 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 241 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetVars", 7 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 242 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetProcStack", 12 );
	hb_xvmPushSymbol( symbols + 65 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 245 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 7 ] = {
			176, 66, 0, 120, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bGUICreateFormDebugger", 22 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 246 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 2 ] = {
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bGUIReleaseFormDebugger", 23 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 247 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 2 ] = {
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bGUIUpdateInfo", 14 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 248 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 2 ] = {
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bGUIDoEvents", 12 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 249 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 2 ] = {
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bGUIReleaseAllWindows", 21 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 250 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 2 ] = {
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bGUIMessageBox", 14 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 251 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lGUIShowMessageBox", 18 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 253 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GUICreateFormDebugger", 21 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 48, 68, 0, 48, 69, 0, 95, 1, 112, 0, 112, 0, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 254 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GUIReleaseFormDebugger", 22 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 48, 68, 0, 48, 70, 0, 95, 1, 112, 0, 112, 0, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 255 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GUIUpdateInfo", 13 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 48, 68, 0, 48, 71, 0, 95, 1, 112, 0, 112, 0, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 256 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GUIDoEvents", 11 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 48, 68, 0, 48, 72, 0, 95, 1, 112, 0, 112, 0, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 257 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GUIReleaseAllWindows", 20 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 48, 68, 0, 48, 73, 0, 95, 1, 112, 0, 112, 0, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 258 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GUIMessageBox", 13 );
	{
		static const HB_BYTE codeblock[ 31 ] = {
			1, 0, 0, 0, 48, 74, 0, 95, 1, 112, 0, 28, 18, 48, 68, 0, 
			48, 75, 0, 95, 1, 112, 0, 164, 146, 1, 0, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 260 );
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushStaticByRef( 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 80 );
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
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_NEW )
{
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 265 );
	hb_xvmPushSelf();
	hb_xvmPopStatic( 1 );
	hb_xvmSetLine( 267 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_ACTIVATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 271 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 272 );
	hb_xvmPushSymbol( symbols + 82 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 273 );
	hb_xvmPushSymbol( symbols + 83 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 274 );
	hb_xvmPushSymbol( symbols + 84 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 275 );
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushStringConst( "PATH", 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 276 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 278 );
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 280 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_LOADCALLSTACK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 0 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 291 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 292 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmArrayDim( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 293 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDec() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 294 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 296 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00005;
lab00002: ;
	hb_xvmSetLine( 297 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 298 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 15 ] = {
			1, 0, 1, 0, 4, 0, 95, 1, 92, 4, 1, 95, 255, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 299 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 301 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmArrayPop() ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 303 );
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 95 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "(", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushNil();
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 6 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmArrayPop() ) break;
lab00004: ;
	hb_xvmSetLine( 296 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00005: ;
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 307 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_HANDLEEVENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 10, 0 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 314 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 3 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 316 );
	hb_xvmPushFuncSymbol( symbols + 98 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 318 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 320 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 321 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 322 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 323 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 324 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 326 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 327 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "tp", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 328 );
	hb_xvmPushSymbol( symbols + 100 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 329 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00003: ;
	hb_xvmSetLine( 330 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 331 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 332 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 3 );
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 334 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 4L ) ) break;
lab00005: ;
	hb_xvmSetLine( 326 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 339 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 340 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushStringConst( "< TP: TracePoint >", 18 );
	if( hb_xvmPlusEqPop() ) break;
lab00007: ;
	hb_xvmSetLine( 343 );
	hb_xvmPushSymbol( symbols + 103 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 344 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushStringConst( "< BP: BreakPoint >", 18 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 345 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 4 );
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 347 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushSymbol( symbols + 104 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 34 ] = {
			1, 0, 1, 0, 7, 0, 95, 1, 92, 3, 1, 95, 255, 8, 21, 31, 
			18, 73, 95, 1, 92, 3, 1, 106, 4, 40, 98, 41, 0, 95, 255, 72, 
			8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 348 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmPushStatic( 3 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 349 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushStringConst( "< BP: BreakPoint FUNCTION >", 27 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 350 );
	hb_xvmPushLocal( 7 );
	hb_xvmPopStatic( 3 );
	hb_xvmSetLine( 351 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 4 );
lab00009: ;
	hb_xvmSetLine( 355 );
	hb_xvmPushSymbol( symbols + 105 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushSymbol( symbols + 106 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmArrayGen( 6 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 357 );
	hb_xvmPushSymbol( symbols + 107 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 358 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00010: ;
	hb_xvmSetLine( 361 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 363 );
	hb_xvmPushSymbol( symbols + 109 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushSymbol( symbols + 110 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
lab00011: ;
	hb_xvmPushSymbol( symbols + 111 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
lab00012: ;
	hb_xvmSetLine( 364 );
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00013: ;
	hb_xvmSetLine( 367 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00014: ;
	hb_xvmSetLine( 382 );
	hb_xvmPushSymbol( symbols + 114 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_xvmSetLine( 383 );
	hb_xvmPushSymbol( symbols + 115 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 384 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 385 );
	hb_xvmPushSymbol( symbols + 109 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 386 );
	hb_xvmPushSymbol( symbols + 117 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
lab00015: ;
	hb_xvmSetLine( 387 );
	hb_xvmPushFuncSymbol( symbols + 98 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushSymbol( symbols + 117 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushSymbol( symbols + 114 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 388 );
	hb_xvmPushSymbol( symbols + 115 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 389 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00015;
lab00016: ;
	hb_xvmSetLine( 392 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00017: ;
	hb_xvmSetLine( 396 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_ANIMATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 403 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 404 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 405 );
	hb_xvmPushSymbol( symbols + 119 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 407 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_STEP )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 411 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 412 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 413 );
	hb_xvmPushSymbol( symbols + 119 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 414 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 416 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_TRACE )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 420 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 421 );
	hb_xvmPushSymbol( symbols + 120 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 422 );
	hb_xvmPushFuncSymbol( symbols + 121 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 424 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GO )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 428 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 429 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 430 );
	hb_xvmPushSymbol( symbols + 119 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 431 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 433 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_SETNEXTROUTINE )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 437 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 438 );
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 439 );
	hb_xvmPushSymbol( symbols + 119 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 440 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 442 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_PAUSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 446 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 447 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 448 );
	hb_xvmPushSymbol( symbols + 119 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 450 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_SETCBTRACE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 454 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 455 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 456 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 457 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 459 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETSOURCEFILES )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 463 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 465 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETNEXTVALIDSTOPLINE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 473 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 474 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 475 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 476 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 477 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmInc() ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 478 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 479 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 477 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00004: ;
	hb_xvmPushLocal( 4 );
	if( hb_xvmAddInt( 1501L ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 483 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETNEXTVALIDSTOPLINEEX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 489 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 490 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 491 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 492 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmInc() ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 493 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 494 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 492 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00004: ;
	hb_xvmPushLocal( 2 );
	if( hb_xvmAddInt( 1501L ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 498 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_ISVALIDSTOPLINE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 502 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 503 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 504 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushSymbol( symbols + 130 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 2 );
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 506 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_SETTOCURSOR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 510 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 511 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 512 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushSymbol( symbols + 130 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 2 );
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 513 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 514 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "SetToCursor: Invalid File Name (", 32 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ") and/or Line Number (", 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 515 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 517 );
	hb_xvmPushFuncSymbol( symbols + 132 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 518 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 520 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_QUIT )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 524 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 525 );
	hb_xvmPushFuncSymbol( symbols + 133 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 526 );
	hb_xvmPushNil();
	hb_xvmPopStatic( 1 );
	hb_xvmSetLine( 527 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 528 );
	hb_xvmPushSymbol( symbols + 134 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 530 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_EXIT )
{
   do {
	hb_xvmSetLine( 534 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 537 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_BREAKPOINTCOUNT )
{
   do {
	hb_xvmSetLine( 541 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 135 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_ISBREAKPOINT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 545 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 547 );
	hb_xvmPushFuncSymbol( symbols + 136 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmInc() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_BREAKPOINTTOGGLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 553 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 554 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 555 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushSymbol( symbols + 130 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 2 );
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 556 );
	hb_xvmPushSymbol( symbols + 103 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 557 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 558 );
	hb_xvmPushFuncSymbol( symbols + 137 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 559 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNegate() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 560 );
	hb_xvmPushSymbol( symbols + 138 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 561 );
	hb_xvmPushFuncSymbol( symbols + 139 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 562 );
	hb_xvmPushSymbol( symbols + 103 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 563 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 565 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "ToggleBreakPoint: Invalid File Name (", 37 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ") and/or Line Number (", 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 566 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_BREAKPOINTDELETE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 573 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 574 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmGreaterEqualThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 140 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 575 );
	hb_xvmPushFuncSymbol( symbols + 137 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 577 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "BreakPointDelete: Invalid BreakPoint Number (", 45 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 578 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 581 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_BREAKPOINTDELETEALL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 587 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 588 );
	hb_xvmPushSymbol( symbols + 140 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 589 );
	hb_xvmPushFuncSymbol( symbols + 137 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 588 );
	if( hb_xvmLocalAddInt( 1, -1 ) ) break;
	hb_xvmPushLocal( 1 );
lab00003: ;
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 592 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_BREAKPOINTADDFUNC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 596 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 597 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 598 );
	hb_xvmPushFuncSymbol( symbols + 142 );
	hb_xvmPushFuncSymbol( symbols + 143 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 599 );
	hb_xvmPushFuncSymbol( symbols + 139 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 601 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "BreakPointAddFunc: Invalid Function Name", 40 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 602 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 605 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETEXPRVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 612 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 613 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 614 );
	hb_xvmPushFuncSymbol( symbols + 144 );
	hb_xvmPushFuncSymbol( symbols + 145 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 615 );
	hb_xvmSeqBegin();
	for( ;; ) {
	hb_xvmSetLine( 616 );
	hb_xvmPushFuncSymbol( symbols + 146 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 617 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 618 );
	hb_xvmPushStringConst( "Syntax error", 12 );
	hb_xvmPopLocal( 3 );
lab00002: ;
	hb_xvmSetLine( 619 );
	if( hb_xvmSeqEndTest() ) break;
	goto lab00004;
	}
	hb_xvmSetLine( 620 );
	if( hb_xvmSeqRecover() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 621 );
	hb_xvmPushSymbol( symbols + 147 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( ": ", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushSymbol( symbols + 148 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmLocalAdd( 3 );
	hb_xvmSetLine( 622 );
	hb_xvmPushFuncSymbol( symbols + 149 );
	hb_xvmPushSymbol( symbols + 150 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 623 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( "; arguments:", 12 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 624 );
	hb_xvmPushFuncSymbol( symbols + 151 );
	hb_xvmPushSymbol( symbols + 150 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 28 ] = {
			1, 0, 1, 0, 3, 0, 96, 255, 255, 106, 2, 32, 0, 176, 143, 0, 
			176, 152, 0, 95, 1, 12, 1, 12, 1, 72, 139, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 626 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 628 );
	hb_xvmPushFuncSymbol( symbols + 144 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 630 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_WATCHCOUNT )
{
   do {
	hb_xvmSetLine( 634 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_WATCHDELETE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 638 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 639 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 640 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "WatchDelete: Invalid Watch number (", 35 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 641 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 643 );
	hb_xvmPushFuncSymbol( symbols + 153 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 644 );
	hb_xvmPushFuncSymbol( symbols + 154 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 646 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_WATCHDELETEALL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 652 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 653 );
	hb_xvmPushSymbol( symbols + 155 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 654 );
	hb_xvmPushFuncSymbol( symbols + 153 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 655 );
	hb_xvmPushFuncSymbol( symbols + 154 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 653 );
	if( hb_xvmLocalAddInt( 1, -1 ) ) break;
	hb_xvmPushLocal( 1 );
lab00003: ;
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 658 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_WATCHGETINFO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 667 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmArrayGen( 0 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 668 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 669 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "WatchGetInfo: Invalid Watch number (", 36 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 670 );
	hb_xvmArrayGen( 0 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 672 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 673 );
	hb_xvmPushSymbol( symbols + 100 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 674 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 675 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 676 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 678 );
	hb_xvmPushStringConst( "U", 1 );
	hb_xvmPopLocal( 3 );
lab00005: ;
	hb_xvmSetLine( 681 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_WATCHSETEXPR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 687 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 688 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 689 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "WatchSetExpr: Invalid expression type (", 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 690 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 692 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00004: ;
	hb_xvmSetLine( 693 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "WatchSetExpr: Invalid Watch number (", 36 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 694 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 696 );
	hb_xvmPushFuncSymbol( symbols + 143 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 697 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 698 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "tp", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00007;
lab00006: ;
	hb_xvmPushLogical( HB_FALSE );
lab00007: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 699 );
	hb_xvmPushFuncSymbol( symbols + 156 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 700 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 701 );
	hb_xvmPushSymbol( symbols + 100 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 702 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 4L ) ) break;
lab00008: ;
	hb_xvmSetLine( 705 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_WATCHPOINTADD )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 711 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 712 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 713 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "WatchPointAdd: Invalid expression type (", 40 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 714 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 716 );
	hb_xvmPushFuncSymbol( symbols + 143 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 717 );
	hb_xvmPushStringConst( "wp", 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 718 );
	hb_xvmPushFuncSymbol( symbols + 157 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 719 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 721 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_TRACEPOINTADD )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 727 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 728 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 729 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "TracePointAdd: Invalid expression type (", 40 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 730 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 732 );
	hb_xvmPushFuncSymbol( symbols + 143 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 733 );
	hb_xvmPushStringConst( "tp", 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 734 );
	hb_xvmPushFuncSymbol( symbols + 157 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 735 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 736 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 737 );
	hb_xvmPushSymbol( symbols + 100 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 739 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_SETPATHFORFILES )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 743 );
	hb_xvmPushSymbol( symbols + 159 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 160 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_LOADSOURCEFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 748 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 749 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 750 );
	hb_xvmPushFuncSymbol( symbols + 161 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 751 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 752 );
	hb_xvmPushSymbol( symbols + 162 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushFuncSymbol( symbols + 163 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 5 );
	hb_xvmSetLine( 753 );
	hb_xvmPushFuncSymbol( symbols + 161 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 754 );
	hb_xvmCopyLocals( 5, 1 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 751 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 162 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
lab00004: ;
	hb_xvmSetLine( 759 );
	hb_xvmPushFuncSymbol( symbols + 161 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 760 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "LoadSourceFile: File Not Found (", 32 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 762 );
	hb_xvmPushFuncSymbol( symbols + 164 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 763 );
	hb_xvmPushFuncSymbol( symbols + 165 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "\x09", 1 );
	hb_xvmPushFuncSymbol( symbols + 166 );
	hb_xvmPushSymbol( symbols + 167 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 764 );
	hb_xvmPushFuncSymbol( symbols + 168 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
lab00006: ;
	hb_xvmSetLine( 767 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETCODELINEINFO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 775 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 776 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmArrayGen( 0 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 777 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 778 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 779 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 780 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 781 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 782 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 783 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	hb_xvmArrayGen( 4 );
	hb_xvmPopLocal( 5 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 785 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "GetCodeLineInfo: Invalid ProcLevel ( #", 38 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( " )", 2 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 788 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_DOCOMMAND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 793 );
	hb_xvmCopyLocals( 1, 2 );
	hb_xvmSetLine( 794 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 795 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 796 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 799 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 800 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 801 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 804 );
	hb_xvmPushFuncSymbol( symbols + 143 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 805 );
	hb_xvmPushFuncSymbol( symbols + 169 );
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 806 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 807 );
	hb_xvmPushFuncSymbol( symbols + 143 );
	hb_xvmPushFuncSymbol( symbols + 170 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmInc() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 808 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 809 );
	hb_xvmPushFuncSymbol( symbols + 169 );
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 810 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 811 );
	hb_xvmPushFuncSymbol( symbols + 143 );
	hb_xvmPushFuncSymbol( symbols + 170 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmInc() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 812 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
lab00004: ;
	hb_xvmSetLine( 815 );
	hb_xvmPushFuncSymbol( symbols + 142 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 818 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "//", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "!", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "#", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
lab00005: ;
	hb_xvmSetLine( 819 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 821 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "BREAKPOINT", 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "BP", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
lab00007: ;
	hb_xvmSetLine( 822 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 823 );
	hb_xvmPushSymbol( symbols + 173 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 174 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	goto lab00032;
lab00008: ;
	hb_xvmSetLine( 824 );
	hb_xvmPushFuncSymbol( symbols + 175 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
lab00009: ;
	hb_xvmSetLine( 825 );
	hb_xvmPushSymbol( symbols + 176 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00032;
lab00010: ;
	hb_xvmSetLine( 827 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00032;
lab00011: ;
	hb_xvmSetLine( 830 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "TRACEPOINT", 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "TP", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
lab00012: ;
	hb_xvmSetLine( 831 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 832 );
	hb_xvmPushSymbol( symbols + 177 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00032;
lab00013: ;
	hb_xvmSetLine( 834 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00032;
lab00014: ;
	hb_xvmSetLine( 837 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "WATCHPOINT", 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "WP", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
lab00015: ;
	hb_xvmSetLine( 838 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 839 );
	hb_xvmPushSymbol( symbols + 178 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00032;
lab00016: ;
	hb_xvmSetLine( 841 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00032;
lab00017: ;
	hb_xvmSetLine( 844 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "CODEBLOCKTRACE", 14 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "CBTRACE", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
lab00018: ;
	hb_xvmSetLine( 845 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( ".T.,TRUE,YES", 12 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 846 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00032;
lab00019: ;
	hb_xvmSetLine( 847 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( ".F.,FALSE,NO", 12 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 848 );
	hb_xvmPushSymbol( symbols + 179 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00032;
lab00020: ;
	hb_xvmSetLine( 850 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00032;
lab00021: ;
	hb_xvmSetLine( 853 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "ANIMATEBREAKPOINT", 17 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00022;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "ANIMATEBP", 9 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
lab00022: ;
	hb_xvmSetLine( 854 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( ".T.,TRUE,YES", 12 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 855 );
	hb_xvmPushSymbol( symbols + 180 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00032;
lab00023: ;
	hb_xvmSetLine( 856 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( ".F.,FALSE,NO", 12 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 857 );
	hb_xvmPushSymbol( symbols + 180 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00032;
lab00024: ;
	hb_xvmSetLine( 859 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00032;
lab00025: ;
	hb_xvmSetLine( 862 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "ANIMATETRACEPOINT", 17 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00026;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "ANIMATETP", 9 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
lab00026: ;
	hb_xvmSetLine( 863 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( ".T.,TRUE,YES", 12 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 864 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00032;
lab00027: ;
	hb_xvmSetLine( 865 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( ".F.,FALSE,NO", 12 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 866 );
	hb_xvmPushSymbol( symbols + 181 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00032;
lab00028: ;
	hb_xvmSetLine( 868 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00032;
lab00029: ;
	hb_xvmSetLine( 871 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SPEED", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 872 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 873 );
	hb_xvmPushSymbol( symbols + 182 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 183 );
	hb_xvmPushFuncSymbol( symbols + 174 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65534 );
#else
	hb_xvmPushLong( 65534L );
#endif
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00032;
lab00030: ;
	hb_xvmSetLine( 875 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00032;
lab00031: ;
	hb_xvmSetLine( 879 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
lab00032: ;
	hb_xvmSetLine( 882 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00033;
	hb_xvmSetLine( 883 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "DoCommand: Command Error (", 26 );
	hb_xvmPushFuncSymbol( symbols + 143 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00033: ;
	hb_xvmSetLine( 886 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_RESTORESETTINGS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 893 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 894 );
	hb_xvmPushFuncSymbol( symbols + 161 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 895 );
	hb_xvmPushFuncSymbol( symbols + 168 );
	hb_xvmPushFuncSymbol( symbols + 164 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 896 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 897 );
	hb_xvmPushSymbol( symbols + 184 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 143 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 896 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 900 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "RestoreSettings: Invalid File Name (", 36 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 903 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_SAVESETTINGS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 908 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 912 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 913 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 914 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "SaveSettings: Invalid File Name", 31 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 916 );
	hb_xvmPushSymbol( symbols + 185 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 919 );
	hb_xvmPushSymbol( symbols + 135 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 920 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "# BREAKPOINTS #", 15 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 921 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	goto lab00007;
lab00004: ;
	hb_xvmSetLine( 922 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 923 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "BP ", 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 925 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "BP ", 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00006: ;
	hb_xvmSetLine( 921 );
	if( hb_xvmLocalIncPush( 5 ) ) break;
lab00007: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 928 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 930 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 931 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "# TRACEPOINTS #", 15 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 932 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	goto lab00010;
lab00008: ;
	hb_xvmSetLine( 933 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "tp", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 934 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 142 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00009: ;
	hb_xvmSetLine( 932 );
	if( hb_xvmLocalIncPush( 5 ) ) break;
lab00010: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 937 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 939 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "# WATCHPOINTS #", 15 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 940 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	goto lab00013;
lab00011: ;
	hb_xvmSetLine( 941 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "wp", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 942 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 142 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00012: ;
	hb_xvmSetLine( 940 );
	if( hb_xvmLocalIncPush( 5 ) ) break;
lab00013: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 945 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 947 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "# OTHERS #", 10 );
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 948 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "CodeBlockTrace ", 15 );
	hb_xvmPushSymbol( symbols + 187 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushStringConst( "YES", 3 );
	goto lab00015;
lab00014: ;
	hb_xvmPushStringConst( "NO", 2 );
lab00015: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 949 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "AnimateBreakPoint ", 18 );
	hb_xvmPushSymbol( symbols + 111 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushStringConst( "YES", 3 );
	goto lab00017;
lab00016: ;
	hb_xvmPushStringConst( "NO", 2 );
lab00017: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 950 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "AnimateTracePoint ", 18 );
	hb_xvmPushSymbol( symbols + 110 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmPushStringConst( "YES", 3 );
	goto lab00019;
lab00018: ;
	hb_xvmPushStringConst( "NO", 2 );
lab00019: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 951 );
	hb_xvmPushSymbol( symbols + 117 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00020;
	hb_xvmSetLine( 952 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "Speed ", 6 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushSymbol( symbols + 117 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 186 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00020: ;
	hb_xvmSetLine( 955 );
	hb_xvmPushFuncSymbol( symbols + 188 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 957 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_VARGETINFO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 962 );
	hb_xvmPushSymbol( symbols + 189 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 963 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 965 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "G", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( "Global", 6 );
	hb_xvmPopLocal( 3 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 966 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushStringConst( "Local", 5 );
	hb_xvmPopLocal( 3 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 967 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "S", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushStringConst( "Static", 6 );
	hb_xvmPopLocal( 3 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 968 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 3 );
lab00004: ;
	hb_xvmSetLine( 971 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_VARGETNAME )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 975 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_VARGETVALTYPE )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 980 );
	hb_xvmPushSymbol( symbols + 189 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 982 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_VARGETVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 987 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 989 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "G", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 190 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 990 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 191 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 991 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "S", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 192 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 992 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_VARSETVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 1001 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1002 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "G", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1003 );
	hb_xvmPushFuncSymbol( symbols + 193 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 1004 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1005 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1006 );
	hb_xvmPushFuncSymbol( symbols + 194 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 1007 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "S", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1008 );
	hb_xvmPushFuncSymbol( symbols + 195 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 1011 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 1012 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmMacroPop( 43 ) ) break;
lab00004: ;
	hb_xvmSetLine( 1015 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETPROCSTACK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 0 );
	hb_xvmSetLine( 1021 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1022 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	goto lab00010;
lab00001: ;
	hb_xvmSetLine( 1023 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushStringConst( "", 0 );
	goto lab00003;
lab00002: ;
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00003: ;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 1024 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushStringConst( "", 0 );
	goto lab00005;
lab00004: ;
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
lab00005: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1025 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushStringConst( "", 0 );
	goto lab00007;
lab00006: ;
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
lab00007: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1026 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushStringConst( "", 0 );
	goto lab00009;
lab00008: ;
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00009: ;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1027 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1022 );
	if( hb_xvmLocalIncPush( 5 ) ) break;
lab00010: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1030 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETAREAS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 0 );
	hb_xvmSetLine( 1035 );
	hb_xvmPushInteger( 512 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 1 );
	hb_xvmLocalSetInt( 4, 0L );
	hb_xvmSetLine( 1036 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1039 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 1040 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 196 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1041 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
	if( hb_xvmArrayPop() ) break;
lab00002: ;
	hb_xvmSetLine( 1039 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00003: ;
	if( hb_xvmGreaterThenIntIs( 512L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 1045 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1046 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00015;
lab00004: ;
	hb_xvmSetLine( 1047 );
	hb_xvmPushFuncSymbol( symbols + 197 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1048 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 12 );
	hb_xvmArrayDim( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1049 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushStringConst( "*", 1 );
	goto lab00006;
lab00005: ;
	hb_xvmPushStringConst( "", 0 );
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 198 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 1050 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 1051 );
	hb_xvmPushFuncSymbol( symbols + 199 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 1052 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushFuncSymbol( symbols + 200 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 1053 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushFuncSymbol( symbols + 201 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 1054 );
	hb_xvmPushFuncSymbol( symbols + 202 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushStringConst( "Yes", 3 );
	goto lab00008;
lab00007: ;
	hb_xvmPushStringConst( "No", 2 );
lab00008: ;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1055 );
	hb_xvmPushFuncSymbol( symbols + 203 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushStringConst( "Yes", 3 );
	goto lab00010;
lab00009: ;
	hb_xvmPushStringConst( "No", 2 );
lab00010: ;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1056 );
	hb_xvmPushFuncSymbol( symbols + 204 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushStringConst( "Yes", 3 );
	goto lab00012;
lab00011: ;
	hb_xvmPushStringConst( "No", 2 );
lab00012: ;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1057 );
	hb_xvmPushFuncSymbol( symbols + 205 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushStringConst( "Yes", 3 );
	goto lab00014;
lab00013: ;
	hb_xvmPushStringConst( "No", 2 );
lab00014: ;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 1058 );
	hb_xvmPushFuncSymbol( symbols + 206 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 1059 );
	hb_xvmPushFuncSymbol( symbols + 207 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 11L ) ) break;
	hb_xvmSetLine( 1060 );
	hb_xvmPushFuncSymbol( symbols + 208 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 12L ) ) break;
	hb_xvmSetLine( 1046 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00015: ;
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1062 );
	hb_xvmPushFuncSymbol( symbols + 197 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1064 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETREC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 1 );
	hb_xvmSetLine( 1070 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1071 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1072 );
	hb_xvmPushFuncSymbol( symbols + 198 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 1074 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 209 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 1075 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 1077 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 210 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1078 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1079 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 1080 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 211 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1081 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 72L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 1082 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 72 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
lab00005: ;
	hb_xvmSetLine( 1084 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 212 );
	hb_xvmPushFuncSymbol( symbols + 213 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1079 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00006: ;
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1087 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETARRAYINFO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 2 );
	hb_xvmSetLine( 1093 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1096 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1097 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "GetArrayInfo: Invalid data type ( ValType: ", 43 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " )", 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00005;
lab00001: ;
	hb_xvmSetLine( 1099 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 1100 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1101 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1102 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 72L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 1103 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 72 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
lab00003: ;
	hb_xvmSetLine( 1105 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( " [ ", 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ]", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1099 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00005: ;
	hb_xvmSetLine( 1109 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETHASHINFO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 2 );
	hb_xvmSetLine( 1114 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1117 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "H", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1118 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "GetHashInfo: Invalid data type ( ValType: ", 42 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " )", 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00005;
lab00001: ;
	hb_xvmSetLine( 1120 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 1121 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1122 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushFuncSymbol( symbols + 214 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1123 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 72L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 1124 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 72 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
lab00003: ;
	hb_xvmSetLine( 1126 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( " [ ", 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushFuncSymbol( symbols + 215 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ]", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1120 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00005: ;
	hb_xvmSetLine( 1130 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETOBJECTINFO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 3 );
	hb_xvmSetLine( 1137 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 1139 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1140 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "O", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1141 );
	hb_xvmPushSymbol( symbols + 131 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "GetObjectInfo: Invalid data type ( ValType: ", 44 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " )", 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00007;
lab00001: ;
	hb_xvmSetLine( 1143 );
	hb_xvmPushFuncSymbol( symbols + 216 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1144 );
	hb_xvmPushFuncSymbol( symbols + 217 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1145 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 1146 );
	hb_xvmPushFuncSymbol( symbols + 218 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1147 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1148 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1149 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 72L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 1150 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushInteger( 72 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
lab00003: ;
	hb_xvmSetLine( 1152 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ":", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1153 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1145 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1155 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 1156 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ":", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "Method", 6 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1157 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1155 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00007: ;
	hb_xvmSetLine( 1161 );
	hb_xvmPushLocal( 10 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( __DBGVALTOSTR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 1167 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1170 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( "NIL", 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 1171 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushStringConst( "{|| ... }", 9 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 1172 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 1173 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1174 );
	hb_xvmPushFuncSymbol( symbols + 183 );
	hb_xvmPushInteger( 8 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1175 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 1176 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushStringConst( "\"", 1 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\"", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushStringConst( "", 0 );
	goto lab00005;
lab00004: ;
	hb_xvmPushStringConst( ", ", 2 );
lab00005: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 1175 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00006: ;
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1178 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 1179 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushStringConst( ", ...", 5 );
	if( hb_xvmPlusEqPop() ) break;
lab00007: ;
	hb_xvmSetLine( 1181 );
	hb_xvmPushStringConst( "Array(", 6 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "): { ", 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " }", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 1182 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CM", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushStringConst( "\"", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\"", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmSetLine( 1183 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushStringConst( ".T.", 3 );
	goto lab00011;
lab00010: ;
	hb_xvmPushStringConst( ".F.", 3 );
lab00011: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00012: ;
	hb_xvmSetLine( 1184 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushFuncSymbol( symbols + 219 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
	break;
lab00013: ;
	hb_xvmSetLine( 1185 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "T", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushFuncSymbol( symbols + 220 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
	break;
lab00014: ;
	hb_xvmSetLine( 1186 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushFuncSymbol( symbols + 213 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
	break;
lab00015: ;
	hb_xvmSetLine( 1187 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "O", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushStringConst( "Class ", 6 );
	hb_xvmPushSymbol( symbols + 221 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " object", 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00016: ;
	hb_xvmSetLine( 1188 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "H", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmPushStringConst( "Hash(", 5 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00017: ;
	hb_xvmSetLine( 1189 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "P", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmPushStringConst( "Pointer", 7 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00018: ;
	hb_xvmSetLine( 1192 );
	hb_xvmPushStringConst( "U", 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( __DBGOBJGETVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 1200 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 1201 );
	hb_xvmSeqBegin();
	for( ;; ) {
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 222, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmSeqBlock() ) break;
	hb_xvmSetLine( 1202 );
	hb_xvmPushFuncSymbol( symbols + 223 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1203 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 3 );
	hb_stackPop();
	if( hb_xvmSeqEndTest() ) break;
	goto lab00002;
	}
	hb_xvmSetLine( 1204 );
	if( hb_xvmSeqRecover() ) break;
	hb_stackPop();
	hb_xvmSetLine( 1205 );
	hb_xvmSeqBegin();
	for( ;; ) {
	hb_xvmPushFuncSymbol( symbols + 145 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSeqBlock() ) break;
	hb_xvmSetLine( 1207 );
	hb_xvmPushFuncSymbol( symbols + 223 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1208 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 3 );
	hb_stackPop();
	if( hb_xvmSeqEndTest() ) break;
	goto lab00002;
	}
	hb_xvmSetLine( 1209 );
	if( hb_xvmSeqRecover() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1210 );
	hb_xvmPushSymbol( symbols + 148 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1211 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 3 );
lab00002: ;
	hb_xvmSetLine( 1215 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( __DBGPATHTOARRAY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 1241 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1242 );
	hb_xvmPushFuncSymbol( symbols + 224 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1243 );
	hb_xvmPushFuncSymbol( symbols + 225 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1245 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00001: ;
	hb_xvmSetLine( 1246 );
	hb_xvmPushFuncSymbol( symbols + 169 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 1247 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1248 );
	hb_xvmPushFuncSymbol( symbols + 170 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmInc() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00001;
lab00002: ;
	hb_xvmSetLine( 1250 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1252 );
	hb_xvmPushFuncSymbol( symbols + 151 );
	hb_xvmPushLocal( 2 );
	{
		static const HB_BYTE codeblock[ 38 ] = {
			2, 0, 2, 0, 2, 0, 4, 0, 176, 226, 0, 95, 1, 122, 12, 2, 
			95, 254, 24, 28, 17, 176, 227, 0, 95, 1, 12, 1, 165, 95, 255, 95, 
			2, 2, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 1255 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( __DBGTEXTTOARRAY )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1259 );
	hb_xvmPushFuncSymbol( symbols + 228 );
	hb_xvmPushFuncSymbol( symbols + 165 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_DEBUGGER )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 1263 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1264 );
	hb_xvmPushSymbol( symbols + 2 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopStatic( 1 );
lab00001: ;
	hb_xvmSetLine( 1267 );
	hb_xvmPushStatic( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETBREAKPOINTS )
{
   HB_BOOL fValue;
   do {
	hb_xvmSFrame( symbols + 235 );
	hb_xvmSetLine( 1277 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmArrayGen( 0 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 1279 );
	hb_xvmPushFuncSymbol( symbols + 230 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETWATCH )
{
   do {
	hb_xvmSetLine( 1283 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMGDEBUGGER_GETVARS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 12, 6 );
	hb_xvmSetLine( 1301 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1302 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1303 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1304 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1305 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1306 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1308 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 1310 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1311 );
	hb_xvmPushFuncSymbol( symbols + 231 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1312 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1313 );
	hb_xvmPushFuncSymbol( symbols + 231 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocalByRef( 13 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 1314 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "Public", 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1312 );
	if( hb_xvmLocalAddInt( 8, -1 ) ) break;
	hb_xvmPushLocal( 8 );
lab00002: ;
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
lab00003: ;
	hb_xvmSetLine( 1318 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 1324 );
	hb_xvmPushFuncSymbol( symbols + 231 );
	hb_xvmPushInteger( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1325 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 1326 );
	hb_xvmPushFuncSymbol( symbols + 231 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1327 );
	hb_xvmHashGen( 0 );
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 1328 );
	hb_xvmPushFuncSymbol( symbols + 232 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1329 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	goto lab00010;
lab00004: ;
	hb_xvmSetLine( 1330 );
	hb_xvmPushFuncSymbol( symbols + 231 );
	hb_xvmPushInteger( 6 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocalByRef( 13 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 1331 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 1332 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushStringConst( "Private LOCAL", 13 );
	goto lab00006;
lab00005: ;
	hb_xvmPushStringConst( "Private GLOBAL", 14 );
lab00006: ;
	hb_xvmPushLocal( 9 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	goto lab00008;
lab00007: ;
	hb_xvmPushInteger( 0 );
lab00008: ;
	hb_xvmArrayGen( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1333 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
lab00009: ;
	hb_xvmSetLine( 1335 );
	if( hb_xvmLocalDec( 9 ) ) break;
	hb_xvmSetLine( 1329 );
	if( hb_xvmLocalAddInt( 8, -1 ) ) break;
	hb_xvmPushLocal( 8 );
lab00010: ;
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
lab00011: ;
	hb_xvmSetLine( 1340 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 1342 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 1343 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 1345 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 1346 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 1347 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	goto lab00019;
lab00012: ;
	hb_xvmSetLine( 1348 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 1349 );
	hb_xvmPushFuncSymbol( symbols + 233 );
	hb_xvmPushSymbol( symbols + 234 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
lab00013: ;
	hb_xvmSetLine( 1353 );
	hb_xvmPushSymbol( symbols + 234 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 1354 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 9 );
	goto lab00015;
lab00014: ;
	hb_xvmSetLine( 1355 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1356 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1354 );
	if( hb_xvmLocalIncPush( 9 ) ) break;
lab00015: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 1358 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmSetLine( 1359 );
	hb_xvmPushSymbol( symbols + 234 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 1360 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 9 );
	goto lab00017;
lab00016: ;
	hb_xvmSetLine( 1361 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1362 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1360 );
	if( hb_xvmLocalIncPush( 9 ) ) break;
lab00017: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
lab00018: ;
	hb_xvmSetLine( 1347 );
	if( hb_xvmLocalIncPush( 8 ) ) break;
lab00019: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 234 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
lab00020: ;
	hb_xvmSetLine( 1368 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 1369 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 1370 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushSymbol( symbols + 234 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 18 ] = {
			1, 0, 1, 0, 13, 0, 176, 233, 0, 95, 1, 122, 1, 95, 255, 12, 
			2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1371 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00023;
	hb_xvmSetLine( 1372 );
	hb_xvmPushSymbol( symbols + 234 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 1373 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 9 );
	goto lab00022;
lab00021: ;
	hb_xvmSetLine( 1374 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1375 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1373 );
	if( hb_xvmLocalIncPush( 9 ) ) break;
lab00022: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
lab00023: ;
	hb_xvmSetLine( 1378 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 1379 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	goto lab00025;
lab00024: ;
	hb_xvmSetLine( 1380 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1381 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1379 );
	if( hb_xvmLocalIncPush( 8 ) ) break;
lab00025: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
lab00026: ;
	hb_xvmSetLine( 1385 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 1386 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 1387 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	goto lab00030;
lab00027: ;
	hb_xvmSetLine( 1388 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 1390 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushLocal( 15 );
	{
		static const HB_BYTE codeblock[ 34 ] = {
			1, 0, 1, 0, 13, 0, 95, 1, 122, 1, 95, 255, 8, 21, 28, 19, 
			73, 176, 171, 0, 95, 1, 92, 3, 1, 122, 12, 2, 106, 2, 83, 0, 
			8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1391 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00028;
	hb_xvmSetLine( 1392 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPop() ) break;
	goto lab00029;
lab00028: ;
	hb_xvmSetLine( 1394 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00029: ;
	hb_xvmSetLine( 1387 );
	if( hb_xvmLocalIncPush( 8 ) ) break;
lab00030: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
lab00031: ;
	hb_xvmSetLine( 1410 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 1411 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 10 );
	goto lab00037;
lab00032: ;
	hb_xvmSetLine( 1412 );
	hb_xvmPushFuncSymbol( symbols + 171 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 1413 );
	hb_xvmPushSymbol( symbols + 189 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 1415 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushStringConst( "G", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmSetLine( 1416 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "Global", 6 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00036;
lab00033: ;
	hb_xvmSetLine( 1417 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmSetLine( 1418 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "Local", 5 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00036;
lab00034: ;
	hb_xvmSetLine( 1419 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushStringConst( "S", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmSetLine( 1420 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "Static", 6 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00036;
lab00035: ;
	hb_xvmSetLine( 1422 );
	hb_xvmPushFuncSymbol( symbols + 158 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 152 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00036: ;
	hb_xvmSetLine( 1411 );
	if( hb_xvmLocalIncPush( 10 ) ) break;
lab00037: ;
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmSetLine( 1426 );
	hb_xvmCopyLocals( 15, 1 );
	hb_xvmSetLine( 1428 );
	hb_xvmPushLocal( 14 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 235, 3 );
	hb_xvmSFrame( symbols + 235 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopStatic( 3 );
	{
		static const HB_BYTE statics[ 2 ] = {
			1, 0 };
		hb_xvmThreadStatics( 1, statics );
	}
	/* *** END PROC *** */
   } while( 0 );
}

#line 1433 "dbgHB.prg"

#include "hbapi.h"

typedef struct
{
   char * szModule;
   int    nLine;
   char * szFunction;
} HB_BREAKPOINT;

typedef struct
{
   int      nIndex;
   PHB_ITEM xValue;
} HB_TRACEPOINT;

typedef struct
{
   char * szName;
   char   cType;
   union
   {
      int      num;
      PHB_ITEM ptr;
   } frame;
   int nIndex;
} HB_VARINFO;

typedef struct
{
   char *       szExpr;
   PHB_ITEM     pBlock;
   int          nVars;
   char **      aVars;
   HB_VARINFO * aScopes;
} HB_WATCHPOINT;

typedef struct
{
   char *       szModule;
   char *       szFunction;
   int          nLine;
   int          nProcLevel;
   int          nLocals;
   HB_VARINFO * aLocals;
   int          nStatics;
   HB_VARINFO * aStatics;
} HB_CALLSTACKINFO;

typedef struct
{
   HB_BOOL bQuit;
   HB_BOOL bGo;
   HB_BOOL bInside;
   int     nBreakPoints;
   HB_BREAKPOINT * aBreak;
   int nTracePoints;
   HB_TRACEPOINT * aTrace;
   int nWatchPoints;
   HB_WATCHPOINT * aWatch;
   HB_BOOL         bTraceOver;
   int     nTraceLevel;
   HB_BOOL bNextRoutine;
   HB_BOOL bCodeBlock;
   HB_BOOL bToCursor;
   int     nToCursorLine;
   char *  szToCursorModule;
   int     nProcLevel;
   int     nCallStackLen;
   HB_CALLSTACKINFO * aCallStack;
   HB_BOOL bCBTrace;
   HB_BOOL ( * pFunInvoke )( void );
   HB_BOOL bInitGlobals;
   HB_BOOL bInitStatics;
   HB_BOOL bInitLines;
} HB_DEBUGINFO;


//        __dbgResetRunFlags( pInfo )
HB_FUNC ( __DBGRESETRUNFLAGS )
{
   HB_DEBUGINFO * info = ( HB_DEBUGINFO * ) hb_parptr( 1 );

   if( info )
   {
      if( info->bToCursor )
         hb_xfree( info->szToCursorModule );

      info->bGo          = HB_FALSE;
      info->bTraceOver   = HB_FALSE;
      info->bNextRoutine = HB_FALSE;
      info->bToCursor    = HB_FALSE;
   }
}

