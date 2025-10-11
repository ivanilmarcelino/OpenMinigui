/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_richeditex.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINERICHEDITBOXEX );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( HB_UAT );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ULEFT );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( INITRICHEDITBOXEX );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( ADDSPLITBOXITEM );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( SETWINDOWTEXT );
HB_FUNC_EXTERN( ISWINDOWHANDLE );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( RICHEDITBOX_SETRTFTEXTMODE );
HB_FUNC_EXTERN( RICHEDITBOX_SETAUTOURLDETECT );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( _SETGETGLOBAL );
HB_FUNC_EXTERN( _DEFINECONTROLCONTEXTMENU );
HB_FUNC_EXTERN( _DEFINEMENUITEM );
HB_FUNC( RICHEDITBOX_MNUEDIT_CLICK );
HB_FUNC_EXTERN( _DEFINESEPARATOR );
HB_FUNC_EXTERN( _ENDMENU );
HB_FUNC_EXTERN( RICHEDITBOX_CHANGEUNDO );
HB_FUNC_EXTERN( RICHEDITBOX_SELCUT );
HB_FUNC_EXTERN( RICHEDITBOX_SELCOPY );
HB_FUNC_EXTERN( RICHEDITBOX_SELPASTE );
HB_FUNC_EXTERN( RICHEDITBOX_SELCLEAR );
HB_FUNC( RICHEDITBOX_SELECTALL );
HB_FUNC( RICHEDITBOX_SETCARETPOS );
HB_FUNC_EXTERN( RICHEDITBOX_SETSELRANGE );
HB_FUNC( RICHEDITBOX_GETCARETPOS );
HB_FUNC_EXTERN( RICHEDITBOX_GETSELRANGE );
HB_FUNC( RICHEDITBOX_UNSELECTALL );
HB_FUNC( RICHEDITBOX_REPLACETEXT );
HB_FUNC_EXTERN( RICHEDITBOX_FINDTEXT );
HB_FUNC_EXTERN( RICHEDITBOX_SETTEXT );
HB_FUNC( RICHEDITBOX_REPLACEALLTEXT );
HB_FUNC_EXTERN( DOEVENTS );
HB_FUNC( RICHEDITBOX_ADDTEXTANDSELECT );
HB_FUNC( RICHEDITBOX_RTFPRINT );
HB_FUNC_EXTERN( RICHEDITBOX_GETTEXTLENGTH );
HB_FUNC_EXTERN( _HMG_PRINTER_GETPAGEWIDTH );
HB_FUNC_EXTERN( _HMG_PRINTER_GETPAGEHEIGHT );
HB_FUNC_EXTERN( _HMG_PRINTER_SETJOBNAME );
HB_FUNC_EXTERN( _HMG_PRINTER_STARTDOC );
HB_FUNC_EXTERN( _HMG_PRINTER_STARTPAGE_PREVIEW );
HB_FUNC_EXTERN( GETTEMPFOLDER );
HB_FUNC_EXTERN( STRZERO );
HB_FUNC_EXTERN( _HMG_PRINTER_STARTPAGE );
HB_FUNC_EXTERN( RICHEDITBOX_FORMATRANGE );
HB_FUNC_EXTERN( _HMG_PRINTER_ENDPAGE_PREVIEW );
HB_FUNC_EXTERN( _HMG_PRINTER_ENDPAGE );
HB_FUNC_EXTERN( _HMG_PRINTER_SHOWPREVIEW );
HB_FUNC_EXTERN( _HMG_PRINTER_ENDDOC );
HB_FUNC( RICHEDITBOX_LOADFILE );
HB_FUNC_EXTERN( RICHEDITBOX_RTFLOADRESOURCEFILE );
HB_FUNC_EXTERN( RICHEDITBOX_STREAMIN );
HB_FUNC( RICHEDITBOX_SAVEFILE );
HB_FUNC_EXTERN( RICHEDITBOX_STREAMOUT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_RICHEDITEX )
{ "_DEFINERICHEDITBOXEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINERICHEDITBOXEX )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "HB_UAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UAT )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ULEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ULEFT )}, NULL },
{ "SELECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SELECT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "INITRICHEDITBOXEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITRICHEDITBOXEX )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "ADDSPLITBOXITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDSPLITBOXITEM )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "SETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTEXT )}, NULL },
{ "ISWINDOWHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISWINDOWHANDLE )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "RICHEDITBOX_SETRTFTEXTMODE", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_SETRTFTEXTMODE )}, NULL },
{ "RICHEDITBOX_SETAUTOURLDETECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_SETAUTOURLDETECT )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "_SETGETGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETGLOBAL )}, NULL },
{ "_DEFINECONTROLCONTEXTMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINECONTROLCONTEXTMENU )}, NULL },
{ "_DEFINEMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEMENUITEM )}, NULL },
{ "RICHEDITBOX_MNUEDIT_CLICK", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RICHEDITBOX_MNUEDIT_CLICK )}, NULL },
{ "_DEFINESEPARATOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINESEPARATOR )}, NULL },
{ "_ENDMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDMENU )}, NULL },
{ "RICHEDITBOX_CHANGEUNDO", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_CHANGEUNDO )}, NULL },
{ "RICHEDITBOX_SELCUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_SELCUT )}, NULL },
{ "RICHEDITBOX_SELCOPY", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_SELCOPY )}, NULL },
{ "RICHEDITBOX_SELPASTE", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_SELPASTE )}, NULL },
{ "RICHEDITBOX_SELCLEAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_SELCLEAR )}, NULL },
{ "RICHEDITBOX_SELECTALL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RICHEDITBOX_SELECTALL )}, NULL },
{ "RICHEDITBOX_SETCARETPOS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RICHEDITBOX_SETCARETPOS )}, NULL },
{ "RICHEDITBOX_SETSELRANGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_SETSELRANGE )}, NULL },
{ "RICHEDITBOX_GETCARETPOS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RICHEDITBOX_GETCARETPOS )}, NULL },
{ "RICHEDITBOX_GETSELRANGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_GETSELRANGE )}, NULL },
{ "RICHEDITBOX_UNSELECTALL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RICHEDITBOX_UNSELECTALL )}, NULL },
{ "RICHEDITBOX_REPLACETEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RICHEDITBOX_REPLACETEXT )}, NULL },
{ "RICHEDITBOX_FINDTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_FINDTEXT )}, NULL },
{ "RICHEDITBOX_SETTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_SETTEXT )}, NULL },
{ "RICHEDITBOX_REPLACEALLTEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RICHEDITBOX_REPLACEALLTEXT )}, NULL },
{ "DOEVENTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOEVENTS )}, NULL },
{ "RICHEDITBOX_ADDTEXTANDSELECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RICHEDITBOX_ADDTEXTANDSELECT )}, NULL },
{ "RICHEDITBOX_RTFPRINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RICHEDITBOX_RTFPRINT )}, NULL },
{ "RICHEDITBOX_GETTEXTLENGTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_GETTEXTLENGTH )}, NULL },
{ "_HMG_PRINTER_GETPAGEWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_GETPAGEWIDTH )}, NULL },
{ "_HMG_MINIPRINT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_HMG_PRINTER_GETPAGEHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_GETPAGEHEIGHT )}, NULL },
{ "_HMG_PRINTER_SETJOBNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_SETJOBNAME )}, NULL },
{ "_HMG_PRINTER_STARTDOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_STARTDOC )}, NULL },
{ "_HMG_PRINTER_STARTPAGE_PREVIEW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_STARTPAGE_PREVIEW )}, NULL },
{ "GETTEMPFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEMPFOLDER )}, NULL },
{ "STRZERO", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRZERO )}, NULL },
{ "_HMG_PRINTER_STARTPAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_STARTPAGE )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "RICHEDITBOX_FORMATRANGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_FORMATRANGE )}, NULL },
{ "_HMG_PRINTER_ENDPAGE_PREVIEW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_ENDPAGE_PREVIEW )}, NULL },
{ "_HMG_PRINTER_ENDPAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_ENDPAGE )}, NULL },
{ "_HMG_PRINTER_SHOWPREVIEW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_SHOWPREVIEW )}, NULL },
{ "_HMG_PRINTER_ENDDOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_ENDDOC )}, NULL },
{ "RICHEDITBOX_LOADFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RICHEDITBOX_LOADFILE )}, NULL },
{ "RICHEDITBOX_RTFLOADRESOURCEFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_RTFLOADRESOURCEFILE )}, NULL },
{ "RICHEDITBOX_STREAMIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_STREAMIN )}, NULL },
{ "RICHEDITBOX_SAVEFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RICHEDITBOX_SAVEFILE )}, NULL },
{ "RICHEDITBOX_STREAMOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RICHEDITBOX_STREAMOUT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_RICHEDITEX, "h_richeditex.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_RICHEDITEX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_RICHEDITEX )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINERICHEDITBOXEX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 30 );
	hb_xvmSetLine( 96 );
	hb_xvmLocalSetInt( 34, 0L );
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 240 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 26 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 27 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 111 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 112 );
	hb_xvmLocalSetInt( 11, -1L );
lab00001: ;
	hb_xvmSetLine( 115 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " : You must specify a fully qualified field name.", 49 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 119 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 121 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 7 );
lab00003: ;
	hb_xvmSetLine( 126 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 127 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 128 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 129 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	hb_xvmPopLocal( 8 );
lab00004: ;
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 132 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	hb_xvmPopLocal( 9 );
lab00005: ;
	hb_xvmSetLine( 136 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 137 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 138 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 139 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 140 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00006: ;
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 148 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00008: ;
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 153 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Already defined.", 17 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
lab00009: ;
	hb_xvmSetLine( 156 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 33 );
	hb_xvmSetLine( 158 );
	hb_xvmCopyLocals( 2, 32 );
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 162 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
lab00010: ;
	hb_xvmSetLine( 164 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	hb_xvmPushStringConst( "TOOLBAR", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 165 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 16 );
lab00011: ;
	hb_xvmSetLine( 168 );
	hb_xvmPushStringConst( "RICHEDIT", 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 22L ) ) break;
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 172 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00017;
	hb_xvmSetLine( 174 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 14 ) ) break;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 37 );
	goto lab00013;
lab00012: ;
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 37 );
lab00013: ;
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 232L ) ) break;
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 182 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00017;
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00017;
lab00014: ;
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 14 ) ) break;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 194 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 37 );
	goto lab00016;
lab00015: ;
	hb_xvmSetLine( 197 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 37 );
lab00016: ;
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00017;
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
lab00017: ;
	hb_xvmSetLine( 207 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmDo( 2 ) ) break;
lab00018: ;
	hb_xvmSetLine( 211 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00019: ;
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 216 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 35 );
	hb_xvmSetLine( 221 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushStringConst( "RICHEDIT", 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 227 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 228 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 229 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 230 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 232 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 233 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 234 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 235 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 236 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 237 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 238 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 239 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 240 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 241 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 242 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 243 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 244 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 245 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 246 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 247 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 248 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00020;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00021;
lab00020: ;
	hb_xvmPushInteger( -1 );
lab00021: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 249 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00023;
lab00022: ;
	hb_xvmPushInteger( -1 );
lab00023: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 250 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 251 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 252 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 253 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 254 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 255 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 258 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00025;
lab00024: ;
	hb_xvmPushLogical( HB_TRUE );
lab00025: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 260 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 261 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 262 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 263 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 264 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 265 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 267 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 268 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmDo( 2 ) ) break;
lab00026: ;
	hb_xvmSetLine( 271 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 35 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 1091 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00027: ;
	hb_xvmSetLine( 275 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_aRichEditMenu", 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 276 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_aRichEditMenu", 18 );
	hb_xvmPushStringConst( "&Undo", 5 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "Cu&t", 4 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "&Copy", 5 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "&Paste", 6 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "&Delete", 7 );
	if( hb_xvmMacroText() ) break;
	hb_xvmPushStringConst( "Select &All", 11 );
	if( hb_xvmMacroText() ) break;
	hb_xvmArrayGen( 6 );
	if( hb_xvmDo( 2 ) ) break;
lab00028: ;
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "_HMG_aRichEditMenu", 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 39 );
	hb_xvmSetLine( 281 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 282 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	{
		static const HB_BYTE codeblock[ 13 ] = {
			176, 33, 0, 106, 5, 85, 78, 68, 79, 0, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "mnuEditUndo", 11 );
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
	hb_xvmSetLine( 283 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	{
		static const HB_BYTE codeblock[ 12 ] = {
			176, 33, 0, 106, 4, 67, 85, 84, 0, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "mnuEditCut", 10 );
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
	hb_xvmSetLine( 285 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	{
		static const HB_BYTE codeblock[ 13 ] = {
			176, 33, 0, 106, 5, 67, 79, 80, 89, 0, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "mnuEditCopy", 11 );
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
	hb_xvmSetLine( 286 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	{
		static const HB_BYTE codeblock[ 14 ] = {
			176, 33, 0, 106, 6, 80, 65, 83, 84, 69, 0, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "mnuEditPaste", 12 );
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
	hb_xvmSetLine( 287 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	{
		static const HB_BYTE codeblock[ 12 ] = {
			176, 33, 0, 106, 4, 68, 69, 76, 0, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "mnuEditDelete", 13 );
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
	hb_xvmSetLine( 288 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	{
		static const HB_BYTE codeblock[ 15 ] = {
			176, 33, 0, 106, 7, 83, 69, 76, 65, 76, 76, 0, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushStringConst( "mnuEditSelAll", 13 );
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
	hb_xvmSetLine( 290 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 292 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RICHEDITBOX_MNUEDIT_CLICK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 299 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 87L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 302 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "UNDO", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 303 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00006;
lab00001: ;
	hb_xvmSetLine( 304 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "CUT", 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 305 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 306 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "COPY", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 307 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 308 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "PASTE", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 309 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 310 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "DEL", 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 311 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 312 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SELALL", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 313 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
lab00006: ;
	hb_xvmSetLine( 316 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RICHEDITBOX_SETCARETPOS )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 325 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 2 );
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 327 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 329 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RICHEDITBOX_GETCARETPOS )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 334 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 336 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RICHEDITBOX_SELECTALL )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 341 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( -1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 343 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 345 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RICHEDITBOX_UNSELECTALL )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 350 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 352 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 354 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RICHEDITBOX_REPLACETEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 6 );
	hb_xvmSetLine( 359 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 362 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 363 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 364 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 365 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmNotEqualIntIs( -1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 366 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 367 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 368 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 8 );
lab00001: ;
	hb_xvmSetLine( 371 );
	hb_xvmPushLocal( 8 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RICHEDITBOX_REPLACEALLTEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 6 );
	hb_xvmSetLine( 376 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 7 );
lab00001: ;
	hb_xvmSetLine( 378 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmNotEqualIntIs( -1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 379 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 380 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00001;
lab00002: ;
	hb_xvmSetLine( 383 );
	hb_xvmPushLocal( 7 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RICHEDITBOX_ADDTEXTANDSELECT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 390 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 391 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 393 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 394 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 395 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 397 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 399 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmLessEqualThenIntIs( -1L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 400 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( -1 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 402 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 404 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 406 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 407 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 408 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 411 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RICHEDITBOX_RTFPRINT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 7 );
	hb_xvmSetLine( 418 );
	hb_xvmLocalSetInt( 10, 0L );
	hb_xvmSetLine( 419 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 421 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( -1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 422 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmLocalSetInt( 3, 20L );
lab00002: ;
	hb_xvmSetLine( 423 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmLocalSetInt( 4, 20L );
lab00003: ;
	hb_xvmSetLine( 424 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmLocalSetInt( 5, 20L );
lab00004: ;
	hb_xvmSetLine( 425 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmLocalSetInt( 6, 20L );
lab00005: ;
	hb_xvmSetLine( 426 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	{
		static const HB_BYTE codeblock[ 2 ] = {
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPopLocal( 7 );
lab00006: ;
	hb_xvmSetLine( 428 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 429 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 431 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 432 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 435 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushDouble( * ( double * ) "\x17\x8B\xC5" "b\xB1" "XL@", 255, 255 );
	if( hb_xvmMultEqPop() ) break;
	hb_xvmSetLine( 436 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushDouble( * ( double * ) "\x17\x8B\xC5" "b\xB1" "XL@", 255, 255 );
	if( hb_xvmMultEqPop() ) break;
	hb_xvmSetLine( 437 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushDouble( * ( double * ) "\x17\x8B\xC5" "b\xB1" "XL@", 255, 255 );
	if( hb_xvmMultEqPop() ) break;
	hb_xvmSetLine( 438 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushDouble( * ( double * ) "\x17\x8B\xC5" "b\xB1" "XL@", 255, 255 );
	if( hb_xvmMultEqPop() ) break;
	hb_xvmSetLine( 440 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmEqualIntIs( -1L, &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00007: ;
	hb_xvmSetLine( 441 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00008: ;
	hb_xvmSetLine( 444 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPop( 21L ) ) break;
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPop( 18L ) ) break;
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPop( 20L ) ) break;
	goto lab00010;
lab00009: ;
	hb_xvmPushFuncSymbol( symbols + 59 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPop( 26L ) ) break;
lab00010: ;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPop( 27L ) ) break;
lab00011: ;
	hb_xvmSetLine( 446 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 448 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 61 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "\\", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_hmg_print_preview_", 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 62 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	hb_xvmPushInteger( 18 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEq() ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".Emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPop( 19L ) ) break;
	goto lab00013;
lab00012: ;
	hb_xvmPushFuncSymbol( symbols + 63 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00013: ;
	hb_xvmSetLine( 450 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 451 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 452 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 453 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 455 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00011;
lab00014: ;
	hb_xvmPushFuncSymbol( symbols + 67 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00011;
lab00015: ;
	hb_xvmSetLine( 459 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushFuncSymbol( symbols + 68 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00017;
lab00016: ;
	hb_xvmPushFuncSymbol( symbols + 69 );
	if( hb_xvmPushMemvar( symbols + 56 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00017: ;
	hb_xvmSetLine( 461 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RICHEDITBOX_LOADFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 468 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 469 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 471 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 473 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 474 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 5 );
lab00001: ;
	hb_xvmSetLine( 477 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RICHEDITBOX_SAVEFILE )
{
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 482 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 483 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 485 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 487 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

