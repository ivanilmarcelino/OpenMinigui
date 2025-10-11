/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_qhtm.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEQHTM );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( _GETID );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( CREATEQHTM );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( SETWINDOWTEXT );
HB_FUNC_EXTERN( QHTM_LOADFILE );
HB_FUNC_EXTERN( QHTM_LOADRES );
HB_FUNC_EXTERN( QHTM_FORMCALLBACK );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC( QHTM_LOADFROMVAL );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC( QHTM_LOADFROMFILE );
HB_FUNC( QHTM_LOADFROMRES );
HB_FUNC( QHTM_GETLINK );
HB_FUNC_EXTERN( QHTM_GETNOTIFY );
HB_FUNC_EXTERN( QHTM_SETRETURNVALUE );
HB_FUNC( QHTM_SCROLLPOS );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC( QHTM_SCROLLPERCENT );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( QHTM_GETSIZE );
HB_FUNC_EXTERN( MIN );
HB_FUNC_EXTERN( ROUND );
HB_FUNC( QHTM_ENABLEUPDATE );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_QHTM )
{ "_DEFINEQHTM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEQHTM )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_GETID", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETID )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "CREATEQHTM", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATEQHTM )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "SETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTEXT )}, NULL },
{ "QHTM_LOADFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( QHTM_LOADFILE )}, NULL },
{ "QHTM_LOADRES", {HB_FS_PUBLIC}, {HB_FUNCNAME( QHTM_LOADRES )}, NULL },
{ "QHTM_FORMCALLBACK", {HB_FS_PUBLIC}, {HB_FUNCNAME( QHTM_FORMCALLBACK )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "QHTM_LOADFROMVAL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( QHTM_LOADFROMVAL )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "QHTM_LOADFROMFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( QHTM_LOADFROMFILE )}, NULL },
{ "QHTM_LOADFROMRES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( QHTM_LOADFROMRES )}, NULL },
{ "QHTM_GETLINK", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( QHTM_GETLINK )}, NULL },
{ "QHTM_GETNOTIFY", {HB_FS_PUBLIC}, {HB_FUNCNAME( QHTM_GETNOTIFY )}, NULL },
{ "QHTM_SETRETURNVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( QHTM_SETRETURNVALUE )}, NULL },
{ "QHTM_SCROLLPOS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( QHTM_SCROLLPOS )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "QHTM_SCROLLPERCENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( QHTM_SCROLLPERCENT )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "QHTM_GETSIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( QHTM_GETSIZE )}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "ROUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROUND )}, NULL },
{ "QHTM_ENABLEUPDATE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( QHTM_ENABLEUPDATE )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_QHTM, "h_qhtm.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_QHTM
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_QHTM )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEQHTM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 14 );
	hb_xvmSetLine( 58 );
	hb_xvmLocalSetInt( 16, 0L );
	hb_xvmSetLine( 61 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 19 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 62 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLocalByRef( 23 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 65 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 67 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 70 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	hb_xvmPopLocal( 10 );
lab00002: ;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 74 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	hb_xvmPopLocal( 11 );
lab00003: ;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 7 );
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
lab00005: ;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 89 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 15 );
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8388608 );
#else
	hb_xvmPushLong( 8388608L );
#endif
	goto lab00007;
lab00006: ;
	hb_xvmPushInteger( 0 );
lab00007: ;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 95 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocalByRef( 11 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 19 );
lab00009: ;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00012;
lab00010: ;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00012;
lab00011: ;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 2 ) ) break;
lab00012: ;
	hb_xvmSetLine( 112 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 114 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 122 );
	hb_xvmPushStringConst( "QHTM", 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 125 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 126 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 128 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 132 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 134 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 135 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 138 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 139 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 140 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 141 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 142 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 144 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00014;
lab00013: ;
	hb_xvmPushInteger( -1 );
lab00014: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 145 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00016;
lab00015: ;
	hb_xvmPushInteger( -1 );
lab00016: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 146 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 148 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 149 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 150 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 151 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 152 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 153 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 159 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 160 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 161 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 163 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( QHTM_LOADFROMVAL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 175 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 179 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( QHTM_LOADFROMFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 191 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 195 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( QHTM_LOADFROMRES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 207 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 211 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( QHTM_GETLINK )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 221 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 223 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 225 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( QHTM_SCROLLPOS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 238 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 240 );
	goto lab00006;
lab00001: ;
	hb_xvmSetLine( 243 );
	hb_xvmLocalSetInt( 2, 0L );
	goto lab00007;
lab00002: ;
	hb_xvmSetLine( 247 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1030 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 2 );
lab00003: ;
	goto lab00007;
lab00004: ;
	hb_xvmSetLine( 253 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 254 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1031 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00007;
lab00005: ;
	hb_xvmSetLine( 256 );
	hb_xvmLocalSetInt( 2, 0L );
	goto lab00007;
lab00006: ;
	hb_xvmPushLocal( 3 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 0L )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 1L )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00004;
		}
		hb_stackPop();
	}
lab00007: ;
	hb_xvmSetLine( 261 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( QHTM_SCROLLPERCENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 2 );
	hb_xvmSetLine( 277 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 281 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 282 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 286 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 287 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmMinusEqPop() ) break;
lab00001: ;
	hb_xvmSetLine( 292 );
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 295 );
	hb_xvmLocalSetInt( 2, 0L );
	goto lab00007;
lab00003: ;
	hb_xvmSetLine( 299 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1030 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 300 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDivide() ) break;
	if( hb_xvmMultByInt( 100L ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00" "Y@", 10, 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00007;
lab00004: ;
	hb_xvmSetLine( 304 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 305 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMult() ) break;
	hb_xvmPushDouble( * ( double * ) "{\x14\xAE" "G\xE1" "z\x84\?", 10, 2 );
	if( hb_xvmMult() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 306 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1031 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00007;
lab00005: ;
	hb_xvmSetLine( 308 );
	hb_xvmLocalSetInt( 2, 0L );
	goto lab00007;
lab00006: ;
	hb_xvmPushLocal( 3 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 0L )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 1L )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00004;
		}
		hb_stackPop();
	}
lab00007: ;
	hb_xvmSetLine( 313 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( QHTM_ENABLEUPDATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 324 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmLessThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 325 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 328 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 330 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 11 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushInteger( 1 );
	goto lab00003;
lab00002: ;
	hb_xvmPushInteger( 0 );
lab00003: ;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 332 );
	/* *** END PROC *** */
   } while( 0 );
}

