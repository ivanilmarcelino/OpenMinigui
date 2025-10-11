/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_animate.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC_INIT( _INITANIMATERES );
HB_FUNC_EXTERN( INSTALLMETHODHANDLER );
HB_FUNC_EXTERN( INSTALLPROPERTYHANDLER );
HB_FUNC( _DEFINEANIMATERES );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC( INITANIMATERES );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC( SETANIMATERESFILE );
HB_FUNC_EXTERN( GETCONTROLTYPE );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC( GETANIMATERESFILE );
HB_FUNC( SETANIMATERESID );
HB_FUNC( GETANIMATERESID );
HB_FUNC( RELEASEANIMATERES );
HB_FUNC( UNLOADANIMATELIB );
HB_FUNC_EXTERN( _GETCONTROLOBJECT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_ANIMATE )
{ "_INITANIMATERES$", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( _INITANIMATERES )}, NULL },
{ "INSTALLMETHODHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLMETHODHANDLER )}, NULL },
{ "INSTALLPROPERTYHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLPROPERTYHANDLER )}, NULL },
{ "_DEFINEANIMATERES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEANIMATERES )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITANIMATERES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITANIMATERES )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETANIMATERESFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETANIMATERESFILE )}, NULL },
{ "GETCONTROLTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLTYPE )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "GETANIMATERESFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETANIMATERESFILE )}, NULL },
{ "SETANIMATERESID", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETANIMATERESID )}, NULL },
{ "GETANIMATERESID", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETANIMATERESID )}, NULL },
{ "RELEASEANIMATERES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RELEASEANIMATERES )}, NULL },
{ "UNLOADANIMATELIB", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UNLOADANIMATELIB )}, NULL },
{ "_GETCONTROLOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLOBJECT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_ANIMATE, "h_animate.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_ANIMATE
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_ANIMATE )
   #include "hbiniseg.h"
#endif

HB_FUNC_INIT( _INITANIMATERES )
{
   do {
	hb_xvmSetLine( 58 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "Release", 7 );
	hb_xvmPushStringConst( "ReleaseAnimateRes", 17 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 59 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "File", 4 );
	hb_xvmPushStringConst( "SetAnimateResFile", 17 );
	hb_xvmPushStringConst( "GetAnimateResFile", 17 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 60 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "ResId", 5 );
	hb_xvmPushStringConst( "SetAnimateResId", 15 );
	hb_xvmPushStringConst( "GetAnimateResId", 15 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 62 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINEANIMATERES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 11 );
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 200 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 50 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 77 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 78 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 81 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 82 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 84 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00002: ;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00004: ;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 96 );
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
	hb_xvmSetLine( 99 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 15 );
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 106 );
	hb_xvmCopyLocals( 2, 14 );
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 110 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 9 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 112 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 116 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00007: ;
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushStringConst( "ANIMATERES", 10 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 121 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 125 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 126 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 128 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 132 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 134 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 135 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 139 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 140 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 141 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 142 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00009;
lab00008: ;
	hb_xvmPushInteger( -1 );
lab00009: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00011;
lab00010: ;
	hb_xvmPushInteger( -1 );
lab00011: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 148 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 150 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 153 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00013;
lab00012: ;
	hb_xvmPushLogical( HB_TRUE );
lab00013: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 161 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 162 );
	hb_xvmPushSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00014: ;
	hb_xvmSetLine( 165 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETANIMATERESFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 171 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "ANIMATERES", 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "FILE", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 173 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	hb_xvmSetLine( 175 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPop() ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 179 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 183 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETANIMATERESFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 188 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 190 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "ANIMATERES", 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 192 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	hb_xvmSetLine( 194 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 198 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 202 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETANIMATERESID )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "ANIMATERES", 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "RESID", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 210 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	hb_xvmSetLine( 212 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPop() ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 216 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 220 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETANIMATERESID )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 225 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 227 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "ANIMATERES", 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 229 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	hb_xvmSetLine( 231 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 235 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 239 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RELEASEANIMATERES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 245 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "ANIMATERES", 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 247 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 249 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 253 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 257 );
	/* *** END PROC *** */
   } while( 0 );
}

#line 264 "h_animate.prg"

#include <mgdefs.h>
#include <mmsystem.h>
#include <commctrl.h>

// If compiled in UNICODE mode, declare a function to convert ANSI strings to wide-character strings
#ifdef UNICODE
   LPWSTR AnsiToWide( LPCSTR );
#endif

// Harbour function to initialize and display an animated control (e.g., a loading animation)
HB_FUNC( INITANIMATERES )
{
   HWND AnimationCtrl;        // Handle for the animation control
   HINSTANCE avi;             // Handle to the loaded library for the AVI resource
#ifndef UNICODE
   LPCSTR lpszDllName = hb_parc( 7 ); // Get the DLL name (resource file) from the 7th Harbour function parameter
#else
   LPWSTR lpszDllName = AnsiToWide( ( char * ) hb_parc( 7 ) ); // Convert to wide-character if in UNICODE mode
#endif

   // Define window style for the animation control
   DWORD Style = WS_CHILD | WS_VISIBLE | ACS_TRANSPARENT | ACS_CENTER | ACS_AUTOPLAY;

   // Initialize the common controls for the animation class
   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof( INITCOMMONCONTROLSEX );
   i.dwICC  = ICC_ANIMATE_CLASS; // Specify that we are using the animation control class
   InitCommonControlsEx( &i ); // Load the necessary common controls

   // Check if the 9th parameter indicates the animation should initially be hidden
   if( ! hb_parl( 9 ) )
   {
      Style |= WS_VISIBLE;    // Add visibility to the style if not hidden
   }

   // Load the specified DLL containing the AVI resource
   avi = LoadLibrary( lpszDllName );

   // Create the animation control window with the specified parameters
   AnimationCtrl = CreateWindowEx
                   (
      0,                       // Extended window style
      ANIMATE_CLASS,           // Predefined class name for animation controls
      NULL,                    // No window name
      Style,                   // Style for the animation control
      hb_parni( 3 ),           // Left position
      hb_parni( 4 ),           // Top position
      hb_parni( 5 ),           // Width
      hb_parni( 6 ),           // Height
      hmg_par_raw_HWND( 1 ),   // Parent window handle
      hmg_par_raw_HMENU( 2 ),  // Handle to menu or child window ID
      avi,                     // Instance handle of the loaded DLL
      NULL                     // Additional parameters (not used here)
                   );

   // Open and play the specified AVI resource in the animation control
   Animate_OpenEx( ( HWND ) AnimationCtrl, avi, MAKEINTRESOURCE( hb_parni( 8 ) ) );

   // Store the library handle in the second return parameter
   HB_STORNL( ( LONG_PTR ) avi, 2 );

   // Return the animation control handle to the caller
   hmg_ret_raw_HANDLE( AnimationCtrl );
}

// Harbour function to unload the library associated with an animation control
HB_FUNC( UNLOADANIMATELIB )
{
   // Retrieve the handle of the library to unload from the function's first parameter
   HINSTANCE hLib = hmg_par_raw_HINSTANCE( 1 );

   // If the library handle is valid, free the loaded library
   if( hLib )
   {
      FreeLibrary( hLib );
   }
}

