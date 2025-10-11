/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_tab.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _BEGINTAB );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _SETGETGLOBAL );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_STATIC( _DEFINETAB );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( LEN );
HB_FUNC( INITDIALOGTAB );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITTABCONTROL );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( CREATESOLIDBRUSH );
HB_FUNC_EXTERN( SETWINDOWBRUSH );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( ADDDIALOGPAGES );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( BMPSIZE );
HB_FUNC_EXTERN( MIN );
HB_FUNC_EXTERN( _GETDUMMYIMAGE );
HB_FUNC_EXTERN( ADDTABBITMAP );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( HB_UAT );
HB_FUNC_EXTERN( _DEFINELETTERORDIGITHOTKEY );
HB_FUNC_EXTERN( HIDEWINDOW );
HB_FUNC( UPDATETAB );
HB_FUNC_EXTERN( TABCTRL_GETCURSEL );
HB_FUNC_STATIC( _ISCONTROLVISIBLEFROMHANDLE );
HB_FUNC_EXTERN( CSHOWCONTROL );
HB_FUNC_STATIC( _ISWINDOWVISIBLEFROMHANDLE );
HB_FUNC_EXTERN( _SETTYPE );
HB_FUNC_STATIC( LHANDLESMATCH );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC( _BEGINTABPAGE );
HB_FUNC_EXTERN( AFILL );
HB_FUNC( _ENDTABPAGE );
HB_FUNC( _ENDTAB );
HB_FUNC_EXTERN( _DELGLOBAL );
HB_FUNC( _ADDTABPAGE );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC_EXTERN( TABCTRL_INSERTITEM );
HB_FUNC_EXTERN( HB_AINS );
HB_FUNC_EXTERN( ASIZE );
HB_FUNC_EXTERN( IMAGELIST_DESTROY );
HB_FUNC( _ADDTABCONTROL );
HB_FUNC_EXTERN( BRINGWINDOWTOTOP );
HB_FUNC_EXTERN( _SETCONTROLROW );
HB_FUNC_EXTERN( _SETCONTROLCOL );
HB_FUNC( _DELETETABPAGE );
HB_FUNC_EXTERN( _GETVALUE );
HB_FUNC_EXTERN( ASC );
HB_FUNC_EXTERN( HB_USUBSTR );
HB_FUNC_EXTERN( _RELEASEHOTKEY );
HB_FUNC_EXTERN( TABCTRL_DELETEITEM );
HB_FUNC_EXTERN( MAX );
HB_FUNC_EXTERN( TABCTRL_SETCURSEL );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_TAB )
{ "_BEGINTAB", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINTAB )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_SETGETGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETGLOBAL )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "_DEFINETAB", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINETAB )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "INITDIALOGTAB", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGTAB )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITTABCONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITTABCONTROL )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "CREATESOLIDBRUSH", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATESOLIDBRUSH )}, NULL },
{ "SETWINDOWBRUSH", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWBRUSH )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "ADDDIALOGPAGES", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDDIALOGPAGES )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "BMPSIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BMPSIZE )}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "__ENUMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_GETDUMMYIMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETDUMMYIMAGE )}, NULL },
{ "ADDTABBITMAP", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDTABBITMAP )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "HB_UAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UAT )}, NULL },
{ "_DEFINELETTERORDIGITHOTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINELETTERORDIGITHOTKEY )}, NULL },
{ "HIDEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( HIDEWINDOW )}, NULL },
{ "UPDATETAB", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UPDATETAB )}, NULL },
{ "TABCTRL_GETCURSEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( TABCTRL_GETCURSEL )}, NULL },
{ "_ISCONTROLVISIBLEFROMHANDLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ISCONTROLVISIBLEFROMHANDLE )}, NULL },
{ "CSHOWCONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( CSHOWCONTROL )}, NULL },
{ "_ISWINDOWVISIBLEFROMHANDLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ISWINDOWVISIBLEFROMHANDLE )}, NULL },
{ "_SETTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETTYPE )}, NULL },
{ "LHANDLESMATCH", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( LHANDLESMATCH )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "_BEGINTABPAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINTABPAGE )}, NULL },
{ "AFILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFILL )}, NULL },
{ "_ENDTABPAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDTABPAGE )}, NULL },
{ "_ENDTAB", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDTAB )}, NULL },
{ "_DELGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DELGLOBAL )}, NULL },
{ "_ADDTABPAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ADDTABPAGE )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "TABCTRL_INSERTITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( TABCTRL_INSERTITEM )}, NULL },
{ "HB_AINS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_AINS )}, NULL },
{ "ASIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASIZE )}, NULL },
{ "IMAGELIST_DESTROY", {HB_FS_PUBLIC}, {HB_FUNCNAME( IMAGELIST_DESTROY )}, NULL },
{ "_ADDTABCONTROL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ADDTABCONTROL )}, NULL },
{ "BRINGWINDOWTOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( BRINGWINDOWTOTOP )}, NULL },
{ "_SETCONTROLROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETCONTROLROW )}, NULL },
{ "_SETCONTROLCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETCONTROLCOL )}, NULL },
{ "_DELETETABPAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DELETETABPAGE )}, NULL },
{ "_GETVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETVALUE )}, NULL },
{ "ASC", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASC )}, NULL },
{ "HB_USUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_USUBSTR )}, NULL },
{ "_RELEASEHOTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( _RELEASEHOTKEY )}, NULL },
{ "TABCTRL_DELETEITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( TABCTRL_DELETEITEM )}, NULL },
{ "MAX", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAX )}, NULL },
{ "TABCTRL_SETCURSEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( TABCTRL_SETCURSEL )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_TAB, "h_tab.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_TAB
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_TAB )
   #include "hbiniseg.h"
#endif

HB_FUNC( _BEGINTAB )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 26 );
	hb_xvmSetLine( 59 );
	hb_xvmPushInteger( 16 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 61 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 62 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 63 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 64 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 65 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 66 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 16 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 67 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 17 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 68 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 26 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 71 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "DEFINE TAB Structures can't be nested.", 38 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 75 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00002: ;
	hb_xvmSetLine( 76 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00004;
lab00003: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00004: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 81 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 82 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 84 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00006: ;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "_HMG_lDialogInMemory", 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "_HMG_bOnInit", 12 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "_HMG_ActiveTabImage_NoTransparent", 33 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 91 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 92 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
lab00007: ;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 6 );
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
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00008: ;
	hb_xvmSetLine( 99 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmPushLocal( 7 );
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
lab00009: ;
	hb_xvmSetLine( 100 );
	hb_xvmLocalSetInt( 7, 1L );
lab00010: ;
	hb_xvmSetLine( 103 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	hb_xvmPushInteger( 37 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 107 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 41L ) ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 42L ) ) break;
	hb_xvmSetLine( 110 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 43L ) ) break;
	hb_xvmSetLine( 111 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 44L ) ) break;
	hb_xvmSetLine( 112 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 249L ) ) break;
	hb_xvmSetLine( 113 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 45L ) ) break;
	hb_xvmSetLine( 114 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 46L ) ) break;
	hb_xvmSetLine( 115 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 47L ) ) break;
	hb_xvmSetLine( 116 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 48L ) ) break;
	hb_xvmSetLine( 117 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 49L ) ) break;
	hb_xvmSetLine( 118 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 50L ) ) break;
	hb_xvmSetLine( 119 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 51L ) ) break;
	hb_xvmSetLine( 120 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 52L ) ) break;
	hb_xvmSetLine( 121 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 53L ) ) break;
	hb_xvmSetLine( 122 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 54L ) ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 55L ) ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 56L ) ) break;
	hb_xvmSetLine( 125 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 417L ) ) break;
	hb_xvmSetLine( 126 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 70L ) ) break;
	hb_xvmSetLine( 128 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 57L ) ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 58L ) ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 59L ) ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 60L ) ) break;
	hb_xvmSetLine( 132 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 85L ) ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 61L ) ) break;
	hb_xvmSetLine( 134 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 208L ) ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 245L ) ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 246L ) ) break;
	hb_xvmSetLine( 138 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 247L ) ) break;
	hb_xvmSetLine( 139 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 248L ) ) break;
	hb_xvmSetLine( 141 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 27 );
	{
		static const HB_BYTE codeblock[ 71 ] = {
			2, 0, 3, 0, 27, 0, 1, 0, 2, 0, 106, 16, 123, 124, 124, 32, 
			95, 83, 101, 116, 86, 97, 108, 117, 101, 40, 34, 0, 95, 254, 72, 106, 
			4, 34, 44, 34, 0, 72, 95, 253, 72, 106, 3, 34, 44, 0, 72, 176, 
			9, 0, 95, 2, 12, 1, 72, 106, 4, 41, 32, 125, 0, 72, 40, 43, 
			165, 95, 255, 95, 2, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 62L ) ) break;
	hb_xvmSetLine( 145 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _DEFINETAB )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 12, 30 );
	hb_xvmSetLine( 151 );
	hb_xvmLocalSetInt( 34, 0L );
	hb_xvmSetLine( 156 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 39 );
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "_HMG_lDialogInMemory", 20 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 40 );
	hb_xvmSetLine( 158 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 41 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 42 );
	hb_xvmSetLine( 161 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 42 );
	hb_xvmSetLine( 164 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 165 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 166 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 16 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 168 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 17 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 172 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushLocal( 40 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 2 );
lab00002: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 40 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Already defined.", 17 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 180 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 33 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLocalByRef( 23 );
	hb_xvmPushLocalByRef( 24 );
	if( hb_xvmDo( 7 ) ) break;
lab00005: ;
	hb_xvmSetLine( 184 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocalByRef( 35 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00006: ;
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 186 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 39 );
	hb_xvmSetLine( 187 );
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 189 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
lab00008: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 191 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 192 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 17 );
lab00009: ;
	hb_xvmSetLine( 195 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 35 );
	hb_xvmSetLine( 196 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 198 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 200 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 202 );
	hb_xvmLocalSetInt( 37, 1342177280L );
	hb_xvmSetLine( 204 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 205 );
	if( hb_xvmLocalAddInt( 37, 256 ) ) break;
lab00010: ;
	hb_xvmSetLine( 207 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 208 );
	if( hb_xvmLocalAddInt( 37, 8 ) ) break;
lab00011: ;
	hb_xvmSetLine( 210 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 211 );
	if( hb_xvmLocalAddInt( 37, 64 ) ) break;
lab00012: ;
	hb_xvmSetLine( 213 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 214 );
	if( hb_xvmLocalAddInt( 37, 128 ) ) break;
lab00013: ;
	hb_xvmSetLine( 216 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 217 );
	if( hb_xvmLocalAddInt( 37, 512 ) ) break;
lab00014: ;
	hb_xvmSetLine( 219 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	hb_xvmSetLine( 220 );
	hb_xvmPushLocalByRef( 37 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00015: ;
	hb_xvmSetLine( 223 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 70L ) ) break;
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 225 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 227 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 19, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 70L ) ) break;
	hb_xvmPushLocal( 36 );
	hb_xvmPushStringConst( "SysTabControl32", 15 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 49L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 48L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 50L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 51L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 55L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 53L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 54L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 245L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 246L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 247L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 248L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00018;
lab00016: ;
	hb_xvmSetLine( 235 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 237 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 239 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 240 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00018;
lab00017: ;
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 250 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 18 ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 252 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 253 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 254 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 2 ) ) break;
lab00018: ;
	hb_xvmSetLine( 259 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00020;
	hb_xvmSetLine( 261 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00019;
	hb_xvmSetLine( 262 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00020;
lab00019: ;
	hb_xvmSetLine( 264 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 265 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 11 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 266 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 33 );
lab00020: ;
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 277 );
	hb_xvmPushStringConst( "TAB", 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 278 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 280 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 282 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 286 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 288 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 292 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 293 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 294 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 295 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 296 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 297 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 298 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 299 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 300 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 301 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 302 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 303 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 304 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 305 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 306 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 307 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 308 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 309 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 310 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 311 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 312 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 313 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 314 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 315 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 30 );
	hb_xvmArrayGen( 8 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 316 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 318 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 319 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmDo( 3 ) ) break;
lab00021: ;
	hb_xvmSetLine( 322 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 323 );
	hb_xvmPushSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 325 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 42 );
	hb_xvmSetLine( 326 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 41 );
lab00022: ;
	hb_xvmSetLine( 330 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushLocal( 41 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 332 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGTAB )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 3 );
	hb_xvmSetLine( 343 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 344 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 346 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 348 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 43L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 349 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 43L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 352 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 43L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 354 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00002: ;
	hb_xvmSetLine( 358 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 359 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 360 );
	hb_xvmPushInteger( 256 );
	hb_xvmPushInteger( 256 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 362 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
lab00003: ;
	hb_xvmSetLine( 364 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 365 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 366 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 367 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 368 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 369 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 6 );
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 371 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 372 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPop() ) break;
lab00005: ;
	hb_xvmSetLine( 376 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
lab00006: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 379 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	goto lab00008;
lab00007: ;
	hb_xvmPushNil();
lab00008: ;
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	goto lab00010;
lab00009: ;
	hb_xvmPushNil();
lab00010: ;
	if( hb_xvmFunction( 5 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00011: ;
	hb_xvmSetLine( 382 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
lab00012: ;
	hb_xvmSetLine( 384 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 386 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushStringConst( "&", 1 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 11 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 387 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00013: ;
	hb_xvmSetLine( 390 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
lab00014: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 393 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 43L ) ) break;
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
lab00015: ;
	hb_xvmSetLine( 395 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 397 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
lab00016: ;
	hb_xvmSetLine( 399 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_xvmSetLine( 401 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00020;
lab00017: ;
	hb_xvmSetLine( 405 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
lab00018: ;
	hb_xvmSetLine( 406 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 407 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
lab00019: ;
	hb_xvmEnumEnd();
lab00020: ;
	hb_xvmSetLine( 411 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
lab00021: ;
	hb_xvmEnumEnd();
lab00022: ;
	hb_xvmSetLine( 415 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
lab00023: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 418 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00024;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 419 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00024: ;
	hb_xvmSetLine( 422 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( UPDATETAB )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 431 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00001: ;
	hb_xvmSetLine( 433 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
lab00002: ;
	hb_xvmSetLine( 435 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 437 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 441 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00004: ;
	hb_xvmSetLine( 442 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 443 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
lab00005: ;
	hb_xvmEnumEnd();
lab00006: ;
	hb_xvmSetLine( 447 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
lab00007: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 449 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00008: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 452 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 454 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 456 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
lab00009: ;
	hb_xvmSetLine( 458 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 460 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 462 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00014;
lab00010: ;
	hb_xvmSetLine( 464 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 466 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00014;
lab00011: ;
	hb_xvmSetLine( 472 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 474 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
lab00012: ;
	hb_xvmSetLine( 475 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 476 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
lab00013: ;
	hb_xvmEnumEnd();
lab00014: ;
	hb_xvmSetLine( 482 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
lab00015: ;
	hb_xvmEnumEnd();
lab00016: ;
	hb_xvmSetLine( 486 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _ISWINDOWVISIBLEFROMHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 492 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushStringConst( "Logical", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 495 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00001: ;
	hb_xvmSetLine( 497 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 498 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 108L ) ) break;
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmNot() ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 499 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 502 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00003: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 504 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _ISCONTROLVISIBLEFROMHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 513 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00001: ;
	hb_xvmSetLine( 514 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 516 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 517 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 519 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00003: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 521 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( LHANDLESMATCH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 526 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 527 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 528 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 529 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 532 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINTABPAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 538 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 539 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 541 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	hb_xvmPushInteger( 42 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 542 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 44L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 543 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 249L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 545 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 547 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 55L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 549 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayDim( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 55L ) ) break;
	hb_xvmSetLine( 550 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 55L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 551 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 55L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	if( hb_xvmArrayPop() ) break;
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 555 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 55L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 559 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 55L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 561 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 55L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 565 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayDim( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 55L ) ) break;
	hb_xvmSetLine( 566 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 55L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
lab00004: ;
	hb_xvmSetLine( 570 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDTABPAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 576 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "_HMG_lDialogInMemory", 20 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 577 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 21L ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 579 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 43L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 580 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 45L ) ) break;
lab00002: ;
	hb_xvmSetLine( 583 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDTAB )
{
   do {
	hb_xvmSetLine( 598 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 46L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 47L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 49L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 48L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 50L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 51L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 44L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 43L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 52L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 53L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 54L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 55L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 56L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 57L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 58L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 59L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 60L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 85L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 61L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 62L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 245L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 246L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 247L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 248L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 249L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 208L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 417L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 70L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "_HMG_bOnInit", 12 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "_HMG_ActiveTabImage_NoTransparent", 33 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 30 ) ) break;
	hb_xvmSetLine( 600 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushStringConst( "_HMG_bOnInit", 12 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushStringConst( "_HMG_ActiveTabImage_NoTransparent", 33 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 601 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 41L ) ) break;
	hb_xvmSetLine( 602 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	hb_xvmPushInteger( 37 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmDecEqPop() ) break;
	hb_xvmSetLine( 604 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ADDTABPAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 6 );
	hb_xvmSetLine( 610 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 613 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 614 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 615 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 617 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 619 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 621 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 623 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmArrayGen( 0 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 624 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 625 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 627 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 628 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 630 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 7 );
	{
		static const HB_BYTE codeblock[ 71 ] = {
			2, 0, 3, 0, 7, 0, 1, 0, 2, 0, 106, 16, 123, 124, 124, 32, 
			95, 83, 101, 116, 86, 97, 108, 117, 101, 40, 34, 0, 95, 254, 72, 106, 
			4, 34, 44, 34, 0, 72, 95, 253, 72, 106, 3, 34, 44, 0, 72, 176, 
			9, 0, 95, 2, 12, 1, 72, 106, 4, 41, 32, 125, 0, 72, 40, 43, 
			165, 95, 255, 95, 2, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 632 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 11 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00001: ;
	hb_xvmSetLine( 633 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 634 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushStringConst( "&", 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 10 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 635 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 11 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	hb_xvmSetLine( 637 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00003: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 639 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
lab00004: ;
	hb_xvmSetLine( 640 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 641 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 642 );
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 644 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
lab00006: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 646 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 648 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 649 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 650 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00007: ;
	hb_xvmSetLine( 652 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 5 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPop() ) break;
lab00008: ;
	hb_xvmSetLine( 655 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 656 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 657 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 658 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	goto lab00010;
lab00009: ;
	hb_xvmSetLine( 660 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
lab00010: ;
	hb_xvmSetLine( 663 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
lab00011: ;
	hb_xvmSetLine( 667 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ADDTABCONTROL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 6 );
	hb_xvmSetLine( 674 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 676 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 678 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmMult() ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 680 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 683 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "SPLITTER", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 684 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 685 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 686 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 687 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 688 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 689 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 693 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "BROWSE", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 694 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 696 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 699 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "SLIDER", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 701 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 702 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 704 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "FRAME,CHECKBOX,RADIOGROUP,LABEL", 31 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 706 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 707 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
lab00005: ;
	hb_xvmSetLine( 711 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 712 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 714 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 715 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 717 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 720 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "SPBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 721 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00006: ;
	hb_xvmSetLine( 726 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DELETETABPAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 3 );
	hb_xvmSetLine( 734 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 737 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 739 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00036;
	hb_xvmSetLine( 742 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 744 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
lab00001: ;
	hb_xvmSetLine( 745 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 746 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
lab00002: ;
	hb_xvmSetLine( 747 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 748 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 750 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00004: ;
	hb_xvmSetLine( 751 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 752 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
lab00005: ;
	hb_xvmEnumEnd();
lab00006: ;
	hb_xvmSetLine( 754 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
lab00007: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 755 );
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 757 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00009: ;
	hb_xvmEnumEnd();
lab00010: ;
	hb_xvmSetLine( 762 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 764 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
lab00011: ;
	hb_xvmSetLine( 765 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 766 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
lab00012: ;
	hb_xvmSetLine( 768 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
lab00013: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 770 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 773 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 775 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
lab00014: ;
	hb_xvmSetLine( 776 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 777 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
lab00015: ;
	hb_xvmSetLine( 779 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
lab00016: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 781 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 784 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
lab00017: ;
	hb_xvmSetLine( 785 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 786 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushStringConst( "&", 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 9 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00018;
	hb_xvmSetLine( 787 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmInc() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 788 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmGreaterEqualThenIntIs( 48L, &fValue ) ) break;
	if( !fValue )
		goto lab00018;
	hb_xvmPushLocal( 10 );
	if( hb_xvmLessEqualThenIntIs( 90L, &fValue ) ) break;
	if( !fValue )
		goto lab00018;
	hb_xvmSetLine( 789 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 3 ) ) break;
lab00018: ;
	hb_xvmSetLine( 792 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
lab00019: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 795 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 797 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
lab00020: ;
	hb_xvmSetLine( 798 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 799 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
lab00021: ;
	hb_xvmSetLine( 801 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00020;
lab00022: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 803 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 806 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 808 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
lab00023: ;
	hb_xvmSetLine( 809 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 810 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushStringConst( "&", 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 9 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00024;
	hb_xvmSetLine( 811 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00024: ;
	hb_xvmSetLine( 813 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00023;
lab00025: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 816 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 818 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
lab00026: ;
	hb_xvmSetLine( 819 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 820 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
lab00027: ;
	hb_xvmSetLine( 822 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00026;
lab00028: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 824 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 826 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 829 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
lab00029: ;
	hb_xvmSetLine( 830 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00030;
	hb_xvmSetLine( 831 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 832 );
	goto lab00031;
lab00030: ;
	hb_xvmSetLine( 834 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00029;
lab00031: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 836 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 839 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmSetLine( 840 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00032;
	hb_xvmSetLine( 841 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00032: ;
	hb_xvmSetLine( 843 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
lab00033: ;
	hb_xvmSetLine( 846 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmLocalDec( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00035;
lab00034: ;
	hb_xvmPushLocal( 3 );
lab00035: ;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 848 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 850 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 1 ) ) break;
lab00036: ;
	hb_xvmSetLine( 854 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

