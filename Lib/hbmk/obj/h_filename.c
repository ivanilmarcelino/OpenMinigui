/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_filename.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( CFILEPATH );
HB_FUNC_EXTERN( HB_FNAMESPLIT );
HB_FUNC_EXTERN( HB_STRSHRINK );
HB_FUNC( CFILENOPATH );
HB_FUNC( CFILENOEXT );
HB_FUNC( _GETCOMPACTPATH );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( GETCOMPACTPATH );
HB_FUNC( _GETSHORTPATHNAME );
HB_FUNC_EXTERN( GETSHORTPATHNAME );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_FILENAME )
{ "CFILEPATH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CFILEPATH )}, NULL },
{ "HB_FNAMESPLIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FNAMESPLIT )}, NULL },
{ "HB_STRSHRINK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_STRSHRINK )}, NULL },
{ "CFILENOPATH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CFILENOPATH )}, NULL },
{ "CFILENOEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CFILENOEXT )}, NULL },
{ "_GETCOMPACTPATH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETCOMPACTPATH )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "GETCOMPACTPATH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCOMPACTPATH )}, NULL },
{ "_GETSHORTPATHNAME", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETSHORTPATHNAME )}, NULL },
{ "GETSHORTPATHNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSHORTPATHNAME )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_FILENAME, "h_filename.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_FILENAME
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_FILENAME )
   #include "hbiniseg.h"
#endif

HB_FUNC( CFILEPATH )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CFILENOPATH )
{
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 104 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CFILENOEXT )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETCOMPACTPATH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 64 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETSHORTPATHNAME )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushLocal( 2 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

