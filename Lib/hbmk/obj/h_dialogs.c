/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_dialogs.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( GETCOLOR );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( ASIZE );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_EXTERN( CHOOSECOLOR );
HB_FUNC_EXTERN( GETRED );
HB_FUNC_EXTERN( GETGREEN );
HB_FUNC_EXTERN( GETBLUE );
HB_FUNC( GETFOLDER );
HB_FUNC_EXTERN( C_BROWSEFORFOLDER );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC( BROWSEFORFOLDER );
HB_FUNC( GETFILE );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( WIN_GETOPENFILENAME );
HB_FUNC_EXTERN( HB_BITAND );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( HB_ATOKENS );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( AADD );
HB_FUNC( PUTFILE );
HB_FUNC_EXTERN( WIN_GETSAVEFILENAME );
HB_FUNC( GETFONT );
HB_FUNC_EXTERN( _SETTYPE );
HB_FUNC_EXTERN( CHOOSEFONT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_DIALOGS )
{ "GETCOLOR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETCOLOR )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "ASIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASIZE )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "GETSYSCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSCOLOR )}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "CHOOSECOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHOOSECOLOR )}, NULL },
{ "GETRED", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETRED )}, NULL },
{ "GETGREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETGREEN )}, NULL },
{ "GETBLUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBLUE )}, NULL },
{ "GETFOLDER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETFOLDER )}, NULL },
{ "C_BROWSEFORFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( C_BROWSEFORFOLDER )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "BROWSEFORFOLDER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BROWSEFORFOLDER )}, NULL },
{ "GETFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETFILE )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "WIN_GETOPENFILENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( WIN_GETOPENFILENAME )}, NULL },
{ "HB_BITAND", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BITAND )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "HB_ATOKENS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ATOKENS )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "PUTFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PUTFILE )}, NULL },
{ "WIN_GETSAVEFILENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( WIN_GETSAVEFILENAME )}, NULL },
{ "GETFONT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETFONT )}, NULL },
{ "_SETTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETTYPE )}, NULL },
{ "CHOOSEFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHOOSEFONT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_DIALOGS, "h_dialogs.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_DIALOGS
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_DIALOGS )
   #include "hbiniseg.h"
#endif

HB_FUNC( GETCOLOR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 3 );
	hb_xvmSetLine( 68 );
	hb_xvmPushInteger( 3 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 73 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 6 );
lab00001: ;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 16 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 7 );
	goto lab00005;
lab00002: ;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 81 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
lab00004: ;
	hb_xvmSetLine( 79 );
	if( hb_xvmLocalIncPush( 7 ) ) break;
lab00005: ;
	if( hb_xvmGreaterThenIntIs( 16L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
lab00006: ;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 90 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 92 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 97 );
	hb_xvmLocalSetInt( 3, 261L );
lab00008: ;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmNotEqualIntIs( -1L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 4 );
lab00009: ;
	hb_xvmSetLine( 106 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETFOLDER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 5 );
	hb_xvmSetLine( 124 );
	hb_xvmLocalSetInt( 6, 112L );
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 512 );
lab00002: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 5 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BROWSEFORFOLDER )
{
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 144 );
	hb_xvmLocalSetInt( 5, 112L );
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 5 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 6 );
	hb_xvmSetLine( 170 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 171 );
	hb_xvmLocalSetInt( 12, 524288L );
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 174 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 177 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 178 );
	if( hb_xvmLocalAddInt( 12, 512 ) ) break;
lab00001: ;
	hb_xvmSetLine( 181 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 182 );
	if( hb_xvmLocalAddInt( 12, 8 ) ) break;
lab00002: ;
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	{
		static const HB_BYTE codeblock[ 31 ] = {
			1, 0, 1, 0, 11, 0, 96, 255, 255, 95, 1, 122, 1, 106, 2, 0, 
			0, 72, 95, 1, 92, 2, 1, 72, 106, 2, 0, 0, 72, 139, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 188 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushStringConst( "\x00", 1 );
	if( hb_xvmPlusEqPop() ) break;
lab00003: ;
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmFunction( 8 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushInteger( 512 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 196 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 197 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( "\x00", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 199 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmArrayGen( 1 );
	hb_xvmPopLocal( 9 );
	goto lab00008;
lab00004: ;
	hb_xvmSetLine( 201 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 10 );
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "\\", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 201 );
	if( hb_xvmLocalIncPush( 10 ) ) break;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 207 );
	hb_xvmCopyLocals( 7, 9 );
lab00008: ;
	hb_xvmSetLine( 210 );
	hb_xvmPushLocal( 9 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PUTFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 7 );
	hb_xvmSetLine( 230 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 9 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 231 );
	hb_xvmLocalSetInt( 11, 524288L );
	hb_xvmSetLine( 233 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 234 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 235 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 238 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 239 );
	if( hb_xvmLocalAddInt( 11, 8 ) ) break;
lab00001: ;
	hb_xvmSetLine( 242 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 243 );
	if( hb_xvmLocalAddInt( 11, 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 247 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	{
		static const HB_BYTE codeblock[ 31 ] = {
			1, 0, 1, 0, 9, 0, 96, 255, 255, 95, 1, 122, 1, 106, 2, 0, 
			0, 72, 95, 1, 92, 2, 1, 72, 106, 2, 0, 0, 72, 139, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 249 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushStringConst( "\x00", 1 );
	if( hb_xvmPlusEqPop() ) break;
lab00003: ;
	hb_xvmSetLine( 253 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 8 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 255 );
	hb_xvmPushLocal( 8 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETFONT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 8 );
	hb_xvmSetLine( 445 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushStringConst( "Numeric", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 448 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 449 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 10 );
lab00001: ;
	hb_xvmSetLine( 460 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 8 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 463 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 464 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 3 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 466 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 467 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
lab00003: ;
	hb_xvmSetLine( 470 );
	hb_xvmPushLocal( 9 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

