/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_scrsaver.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _BEGINSCRSAVER );
HB_FUNC_EXTERN( GETDESKTOPWIDTH );
HB_FUNC_EXTERN( GETDESKTOPHEIGHT );
HB_FUNC_EXTERN( __MVPUBLIC );
HB_FUNC_EXTERN( _DEFINEWINDOW );
HB_FUNC_EXTERN( SHOWCURSOR );
HB_FUNC_EXTERN( SYSTEMPARAMETERSINFO );
HB_FUNC( _RELEASESCRSAVER );
HB_FUNC( _LVALIDSCRSAVER );
HB_FUNC_EXTERN( DOMETHOD );
HB_FUNC_EXTERN( GETCURSORPOS );
HB_FUNC_EXTERN( SETCURSORPOS );
HB_FUNC_EXTERN( _DEFINETIMER );
HB_FUNC_EXTERN( _ENDWINDOW );
HB_FUNC( _ACTIVATESCRSAVER );
HB_FUNC_EXTERN( LOWER );
HB_FUNC_EXTERN( _ACTIVATEWINDOW );
HB_FUNC_EXTERN( MSGINFO );
HB_FUNC_EXTERN( CHANGEPASSWORD );
HB_FUNC_EXTERN( GETACTIVEWINDOW );
HB_FUNC_EXTERN( HB_PROGNAME );
HB_FUNC_EXTERN( GETSYSTEMFOLDER );
HB_FUNC_EXTERN( HB_PS );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( CFILENOEXT );
HB_FUNC_EXTERN( FILE );
HB_FUNC_EXTERN( FERASE );
HB_FUNC_EXTERN( __COPYFILE );
HB_FUNC_EXTERN( _EXECUTE );
HB_FUNC_EXTERN( _BEGININI );
HB_FUNC_EXTERN( GETWINDOWSFOLDER );
HB_FUNC_EXTERN( _SETINI );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( _GETSHORTPATHNAME );
HB_FUNC_EXTERN( _ENDINI );
HB_FUNC_EXTERN( CFILENOPATH );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( MSGSTOP );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( TREG32 );
HB_FUNC_EXTERN( VERIFYPASSWORD );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_SCRSAVER )
{ "_BEGINSCRSAVER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINSCRSAVER )}, NULL },
{ "GETDESKTOPWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDESKTOPWIDTH )}, NULL },
{ "GETDESKTOPHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDESKTOPHEIGHT )}, NULL },
{ "_HMG_SCRSAVERDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPUBLIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPUBLIC )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_DEFINEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEWINDOW )}, NULL },
{ "SHOWCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SHOWCURSOR )}, NULL },
{ "SYSTEMPARAMETERSINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( SYSTEMPARAMETERSINFO )}, NULL },
{ "_RELEASESCRSAVER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _RELEASESCRSAVER )}, NULL },
{ "_LVALIDSCRSAVER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _LVALIDSCRSAVER )}, NULL },
{ "DOMETHOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOMETHOD )}, NULL },
{ "GETCURSORPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCURSORPOS )}, NULL },
{ "SETCURSORPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCURSORPOS )}, NULL },
{ "_DEFINETIMER", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINETIMER )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ENDWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDWINDOW )}, NULL },
{ "_ACTIVATESCRSAVER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ACTIVATESCRSAVER )}, NULL },
{ "LOWER", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOWER )}, NULL },
{ "_ACTIVATEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ACTIVATEWINDOW )}, NULL },
{ "MSGINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGINFO )}, NULL },
{ "CHANGEPASSWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHANGEPASSWORD )}, NULL },
{ "GETACTIVEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETACTIVEWINDOW )}, NULL },
{ "HB_PROGNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PROGNAME )}, NULL },
{ "GETSYSTEMFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSTEMFOLDER )}, NULL },
{ "HB_PS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PS )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "CFILENOEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( CFILENOEXT )}, NULL },
{ "FILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FILE )}, NULL },
{ "FERASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERASE )}, NULL },
{ "__COPYFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __COPYFILE )}, NULL },
{ "_EXECUTE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _EXECUTE )}, NULL },
{ "_BEGININI", {HB_FS_PUBLIC}, {HB_FUNCNAME( _BEGININI )}, NULL },
{ "GETWINDOWSFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWSFOLDER )}, NULL },
{ "_SETINI", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETINI )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "_GETSHORTPATHNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETSHORTPATHNAME )}, NULL },
{ "_ENDINI", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDINI )}, NULL },
{ "CFILENOPATH", {HB_FS_PUBLIC}, {HB_FUNCNAME( CFILENOPATH )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "MSGSTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGSTOP )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TREG32", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREG32 )}, NULL },
{ "GET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CLOSE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VERIFYPASSWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( VERIFYPASSWORD )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_SCRSAVER, "h_scrsaver.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_SCRSAVER
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_SCRSAVER )
   #include "hbiniseg.h"
#endif

HB_FUNC( _BEGINSCRSAVER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 7 );
	hb_xvmSetLine( 60 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 8 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 62 );
	hb_xvmPushInteger( 5 );
	hb_xvmArrayDim( 1 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushSymbol( symbols + 3 );
	if( hb_xvmDo( 1 ) ) break;
	if( hb_xvmPopMemvar( symbols + 3 ) ) break;
	hb_xvmSetLine( 64 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 65 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 66 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 67 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 69 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmLocalSetInt( 6, 1L );
lab00001: ;
	hb_xvmSetLine( 71 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 184L ) ) break;
	hb_xvmSetLine( 73 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	{
		static const HB_BYTE codeblock[ 25 ] = {
			0, 0, 1, 0, 11, 0, 176, 7, 0, 9, 20, 1, 176, 8, 0, 92, 
			97, 122, 96, 255, 255, 121, 12, 4, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 22 ] = {
			0, 0, 3, 0, 4, 0, 1, 0, 5, 0, 176, 9, 0, 95, 255, 95, 
			254, 95, 253, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 34 ] = {
			0, 0, 1, 0, 1, 0, 176, 10, 0, 12, 0, 28, 21, 176, 11, 0, 
			95, 255, 106, 8, 82, 101, 108, 101, 97, 115, 101, 0, 12, 2, 25, 3, 
			100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 75 ] = {
			0, 0, 4, 0, 8, 0, 10, 0, 9, 0, 1, 0, 176, 12, 0, 12, 
			0, 80, 255, 95, 255, 122, 1, 95, 254, 92, 2, 18, 69, 28, 44, 95, 
			255, 92, 2, 1, 95, 253, 92, 2, 18, 69, 28, 31, 176, 10, 0, 12, 
			0, 28, 21, 176, 11, 0, 95, 252, 106, 8, 82, 101, 108, 101, 97, 115, 
			101, 0, 12, 2, 25, 6, 100, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 24 ] = {
			0, 0, 2, 0, 9, 0, 10, 0, 176, 13, 0, 95, 255, 92, 2, 18, 
			95, 254, 92, 2, 18, 12, 2, 6 };
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
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	{
		static const HB_BYTE codeblock[ 25 ] = {
			0, 0, 1, 0, 11, 0, 176, 7, 0, 9, 20, 1, 176, 8, 0, 92, 
			97, 122, 96, 255, 255, 121, 12, 4, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 22 ] = {
			0, 0, 3, 0, 4, 0, 1, 0, 5, 0, 176, 9, 0, 95, 255, 95, 
			254, 95, 253, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 34 ] = {
			0, 0, 1, 0, 1, 0, 176, 10, 0, 12, 0, 28, 21, 176, 11, 0, 
			95, 255, 106, 8, 82, 101, 108, 101, 97, 115, 101, 0, 12, 2, 25, 3, 
			100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 75 ] = {
			0, 0, 4, 0, 8, 0, 10, 0, 9, 0, 1, 0, 176, 12, 0, 12, 
			0, 80, 255, 95, 255, 122, 1, 95, 254, 92, 2, 18, 69, 28, 44, 95, 
			255, 92, 2, 1, 95, 253, 92, 2, 18, 69, 28, 31, 176, 10, 0, 12, 
			0, 28, 21, 176, 11, 0, 95, 252, 106, 8, 82, 101, 108, 101, 97, 115, 
			101, 0, 12, 2, 25, 6, 100, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 24 ] = {
			0, 0, 2, 0, 9, 0, 10, 0, 176, 13, 0, 95, 255, 92, 2, 18, 
			95, 254, 92, 2, 18, 12, 2, 6 };
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
lab00003: ;
	hb_xvmSetLine( 103 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushStringConst( "Timer_SSaver", 12 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 6 );
	if( hb_xvmMultByInt( 1000L ) ) break;
	{
		static const HB_BYTE codeblock[ 14 ] = {
			0, 0, 1, 0, 5, 0, 48, 15, 0, 95, 255, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	if( hb_xvmDo( 6 ) ) break;
lab00004: ;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 111 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 113 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 117 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ACTIVATESCRSAVER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 124 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( "-i", 2 );
	goto lab00002;
lab00001: ;
	hb_xvmPushStringConst( "-s", 2 );
lab00002: ;
	hb_xvmPopLocal( 2 );
lab00003: ;
	hb_xvmSetLine( 126 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "/s", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "-s", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00004: ;
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00018;
lab00005: ;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "/c", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "-c", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00006: ;
	hb_xvmSetLine( 135 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 136 );
	hb_xvmPushSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00018;
lab00007: ;
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushStringConst( "This screen saver has no options that you configure.", 52 );
	hb_xvmPushStringConst( "Information", 11 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00018;
lab00008: ;
	hb_xvmSetLine( 141 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "/a", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "-a", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
lab00009: ;
	hb_xvmSetLine( 143 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00018;
lab00010: ;
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "/i", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "-i", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
lab00011: ;
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 150 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	goto lab00013;
lab00012: ;
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ".SCR", 4 );
	if( hb_xvmPlus() ) break;
lab00013: ;
	hb_xvmLocalAdd( 4 );
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 153 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
lab00014: ;
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 160 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 271L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 164 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "Rundll32.exe", 12 );
	hb_xvmPushStringConst( "desk.cpl,InstallScreenSaver ", 28 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".SCR", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushNil();
	hb_xvmPushInteger( 5 );
	if( hb_xvmDo( 6 ) ) break;
	goto lab00016;
lab00015: ;
	hb_xvmSetLine( 168 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "system.ini", 10 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushStringConst( "boot", 4 );
	hb_xvmPushStringConst( "SCRNSAVE.EXE", 12 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmDo( 0 ) ) break;
lab00016: ;
	hb_xvmSetLine( 174 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( " installation successfully.", 27 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Information", 11 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 176 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 274 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 61760 );
#else
	hb_xvmPushLong( 61760L );
#endif
	if( hb_xvmDo( 3 ) ) break;
	goto lab00018;
lab00017: ;
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( " installation no successfully.", 30 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Error", 5 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00018: ;
	hb_xvmSetLine( 188 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _RELEASESCRSAVER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 193 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 195 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 196 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 199 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Timer_SSaver", 12 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushInteger( 97 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 207 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _LVALIDSCRSAVER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 0 );
	hb_xvmSetLine( 213 );
	hb_xvmLocalSetInt( 2, 1L );
	hb_xvmPushStringConst( "ScreenSave", 10 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 180L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( "rIsSecure", 9 );
	goto lab00002;
lab00001: ;
	hb_xvmPushStringConst( "UsePassword", 11 );
lab00002: ;
	hb_xvmLocalAdd( 4 );
	hb_xvmSetLine( 215 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLongLong( HB_LL( 2147483649 ) );
	hb_xvmPushStringConst( "Control Panel\\Desktop", 21 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 217 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 219 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 221 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 222 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 224 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 3 );
lab00004: ;
	hb_xvmSetLine( 227 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

