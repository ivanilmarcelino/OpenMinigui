/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_crypt.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC_STATIC( _ENCRYPT );
HB_FUNC_EXTERN( CHARXOR );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_STATIC( _DECRYPT );
HB_FUNC( FI_CODE );
HB_FUNC_EXTERN( FILE );
HB_FUNC_EXTERN( MSGSTOP );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( PADR );
HB_FUNC_EXTERN( FOPEN );
HB_FUNC_EXTERN( FERROR );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( FREAD );
HB_FUNC_EXTERN( FCLOSE );
HB_FUNC_EXTERN( FSEEK );
HB_FUNC_EXTERN( FCREATE );
HB_FUNC_EXTERN( FWRITE );
HB_FUNC_EXTERN( FERASE );
HB_FUNC( FI_DECODE );
HB_FUNC( DB_ENCRYPT );
HB_FUNC_EXTERN( AT );
HB_FUNC_STATIC( CFILENAME );
HB_FUNC( DB_UNENCRYPT );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( LEFT );
HB_FUNC( DB_CODE );
HB_FUNC_EXTERN( RECNO );
HB_FUNC_EXTERN( ALIAS );
HB_FUNC_EXTERN( __DBCOPYSTRUCT );
HB_FUNC_EXTERN( FCOUNT );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( FIELDPUT );
HB_FUNC_EXTERN( FIELDGET );
HB_FUNC_EXTERN( AFILL );
HB_FUNC_EXTERN( FIELDPOS );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( RLOCK );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( DBCLOSEAREA );
HB_FUNC_EXTERN( DBGOTO );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_CRYPT )
{ "_ENCRYPT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENCRYPT )}, NULL },
{ "CHARXOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHARXOR )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "_DECRYPT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DECRYPT )}, NULL },
{ "FI_CODE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( FI_CODE )}, NULL },
{ "FILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FILE )}, NULL },
{ "MSGSTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGSTOP )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "PADR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PADR )}, NULL },
{ "FOPEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOPEN )}, NULL },
{ "FERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERROR )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "FREAD", {HB_FS_PUBLIC}, {HB_FUNCNAME( FREAD )}, NULL },
{ "FCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCLOSE )}, NULL },
{ "FSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( FSEEK )}, NULL },
{ "FCREATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCREATE )}, NULL },
{ "FWRITE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FWRITE )}, NULL },
{ "FERASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERASE )}, NULL },
{ "FI_DECODE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( FI_DECODE )}, NULL },
{ "DB_ENCRYPT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DB_ENCRYPT )}, NULL },
{ "AT", {HB_FS_PUBLIC}, {HB_FUNCNAME( AT )}, NULL },
{ "CFILENAME", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( CFILENAME )}, NULL },
{ "DB_UNENCRYPT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DB_UNENCRYPT )}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "DB_CODE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DB_CODE )}, NULL },
{ "RECNO", {HB_FS_PUBLIC}, {HB_FUNCNAME( RECNO )}, NULL },
{ "ALIAS", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALIAS )}, NULL },
{ "__DBCOPYSTRUCT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBCOPYSTRUCT )}, NULL },
{ "FCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCOUNT )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "FIELDPUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDPUT )}, NULL },
{ "FIELDGET", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDGET )}, NULL },
{ "AFILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFILL )}, NULL },
{ "FIELDPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDPOS )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "RLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( RLOCK )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "DBCLOSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEAREA )}, NULL },
{ "DBGOTO", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTO )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_CRYPT, "h_crypt.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_CRYPT
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_CRYPT )
   #include "hbiniseg.h"
#endif

HB_FUNC_STATIC( _ENCRYPT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 62 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<ORIGINAL>", 10 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 64 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 66 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 70 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _DECRYPT )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "<ORIGINAL>", 10 );
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( FI_CODE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 4 );
	hb_xvmSetLine( 84 );
	hb_xvmLocalSetInt( 8, 1L );
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "No such file", 12 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 90 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushStringConst( "New and old filenames must not be the same", 42 );
	hb_xvmPushStringConst( "Attention", 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 97 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 101 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 103 );
	hb_xvmPushStringConst( "<PRIMARY>", 9 );
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 107 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 109 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 4 );
lab00005: ;
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 10L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00007;
lab00006: ;
	hb_xvmSetLine( 119 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
lab00007: ;
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 125 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot proceed", 30 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00008: ;
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushInteger( 30 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 132 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 30 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 134 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "ENCRYPTED FILE (C) ODESSA 2002", 30 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File already encrypted", 22 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 138 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmSetLine( 142 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot proceed", 30 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 148 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 149 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00010: ;
	hb_xvmSetLine( 153 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "ENCRYPTED FILE (C) ODESSA 2002", 30 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushInteger( 512 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
lab00011: ;
	hb_xvmSetLine( 158 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 512 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 162 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmNotEqualIntIs( 512L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 164 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 6 );
lab00012: ;
	hb_xvmSetLine( 168 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00011;
lab00013: ;
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 174 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 176 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00014: ;
	hb_xvmSetLine( 182 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( FI_DECODE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 4 );
	hb_xvmSetLine( 188 );
	hb_xvmLocalSetInt( 8, 1L );
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "No such file", 12 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 194 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushStringConst( "New and old filenames must not be the same", 42 );
	hb_xvmPushStringConst( "Attention", 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 201 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 205 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 207 );
	hb_xvmPushStringConst( "<PRIMARY>", 9 );
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 211 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 213 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 4 );
lab00005: ;
	hb_xvmSetLine( 217 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 10L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 219 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00007;
lab00006: ;
	hb_xvmSetLine( 223 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
lab00007: ;
	hb_xvmSetLine( 227 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot proceed", 30 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00008: ;
	hb_xvmSetLine( 235 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushInteger( 30 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 236 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 30 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 238 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "ENCRYPTED FILE (C) ODESSA 2002", 30 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 240 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File is not encrypted", 21 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 242 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmSetLine( 246 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 247 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 249 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 251 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushStringConst( "You have entered the wrong password", 35 );
	hb_xvmPushStringConst( "Attention", 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 252 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 253 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00010: ;
	hb_xvmSetLine( 257 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 259 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 261 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot proceed", 30 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 262 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 263 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00011: ;
	hb_xvmSetLine( 267 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushInteger( 512 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
lab00012: ;
	hb_xvmSetLine( 269 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmSetLine( 271 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 512 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 273 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmNotEqualIntIs( 512L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 275 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 6 );
lab00013: ;
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 280 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00012;
lab00014: ;
	hb_xvmSetLine( 284 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 289 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00015: ;
	hb_xvmSetLine( 293 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DB_ENCRYPT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 299 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 301 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 303 );
	hb_xvmPushStringConst( "<PRIMARY>", 9 );
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 307 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 309 );
	hb_xvmPushStringConst( "TEMP.DBF", 8 );
	hb_xvmPopLocal( 1 );
lab00002: ;
	hb_xvmSetLine( 313 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 10L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 315 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 319 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 323 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 325 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( ".DBF", 4 );
	if( hb_xvmPlusEqPop() ) break;
lab00005: ;
	hb_xvmSetLine( 329 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 331 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 333 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 335 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot encrypt file", 35 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 336 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 340 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 28 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 342 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 344 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot encrypt file", 35 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 345 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 346 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00007: ;
	hb_xvmSetLine( 350 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 352 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot encrypt file", 35 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 353 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 354 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 358 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ENC", 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 360 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "This database already encrypted!", 32 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 361 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 362 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmSetLine( 366 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 368 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 370 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 371 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot encrypt file", 35 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 372 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00010: ;
	hb_xvmSetLine( 376 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 378 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 379 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot encrypt file", 35 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 380 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00011: ;
	hb_xvmSetLine( 384 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 385 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 387 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 389 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 390 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot encrypt file", 35 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 391 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00012: ;
	hb_xvmSetLine( 395 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 397 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 398 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot encrypt file", 35 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 399 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00013: ;
	hb_xvmSetLine( 403 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 12 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 405 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmSetLine( 407 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 408 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot encrypt file", 35 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 409 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00014: ;
	hb_xvmSetLine( 413 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 415 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 417 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 418 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot encrypt file", 35 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 419 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00015: ;
	hb_xvmSetLine( 423 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 28 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 425 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "ENC", 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 427 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 428 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot encrypt file", 35 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 429 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00016: ;
	hb_xvmSetLine( 433 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00018;
lab00017: ;
	hb_xvmSetLine( 437 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "No such file", 12 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00018: ;
	hb_xvmSetLine( 441 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DB_UNENCRYPT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 2 );
	hb_xvmSetLine( 447 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 449 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 451 );
	hb_xvmPushStringConst( "<PRIMARY>", 9 );
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 455 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 457 );
	hb_xvmPushStringConst( "TEMP.DBF", 8 );
	hb_xvmPopLocal( 1 );
lab00002: ;
	hb_xvmSetLine( 461 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 10L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 463 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 467 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 471 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 473 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( ".DBF", 4 );
	if( hb_xvmPlusEqPop() ) break;
lab00005: ;
	hb_xvmSetLine( 477 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 479 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 481 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 483 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot unencrypt file", 37 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 484 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 488 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 28 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 490 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 492 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot unencrypt file", 37 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 493 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 494 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00007: ;
	hb_xvmSetLine( 498 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 500 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot unencrypt file", 37 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 501 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 502 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 506 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "ENC", 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 508 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "This database is not encrypted!", 31 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 509 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 510 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmSetLine( 514 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 12 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 516 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 518 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 519 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot unencrypt file", 37 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 520 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00010: ;
	hb_xvmSetLine( 524 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 526 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 528 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 529 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot unencrypt file", 37 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 530 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00011: ;
	hb_xvmSetLine( 534 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 536 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 537 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushStringConst( "You have entered the wrong password", 35 );
	hb_xvmPushStringConst( "Attention", 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 538 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00012: ;
	hb_xvmSetLine( 542 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 543 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 545 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 547 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 548 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot unencrypt file", 37 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 549 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00013: ;
	hb_xvmSetLine( 553 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmSetLine( 555 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 556 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot unencrypt file", 37 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 557 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00014: ;
	hb_xvmSetLine( 561 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 562 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 564 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
	hb_xvmSetLine( 566 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 567 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot unencrypt file", 37 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 568 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00015: ;
	hb_xvmSetLine( 572 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 574 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 575 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot unencrypt file", 37 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 576 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00016: ;
	hb_xvmSetLine( 580 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 12 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 582 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushStringConst( "\x00", 1 );
	hb_xvmPushInteger( 20 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualIntIs( 20L, &fValue ) ) break;
	if( !fValue )
		goto lab00017;
	hb_xvmSetLine( 584 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 585 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "File I/O error, cannot unencrypt file", 37 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 586 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00017: ;
	hb_xvmSetLine( 590 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00019;
lab00018: ;
	hb_xvmSetLine( 594 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "No such file", 12 );
	hb_xvmPushStringConst( "Stop!", 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00019: ;
	hb_xvmSetLine( 598 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( CFILENAME )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 604 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 605 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 607 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 2 );
lab00002: ;
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DB_CODE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 10, 6 );
	hb_xvmSetLine( 613 );
	hb_xvmPushStringConst( "__temp__.dbf", 12 );
	hb_xvmPopLocal( 7 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 614 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 616 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( ".DBF", 4 );
	if( hb_xvmPlus() ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 617 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ".DBF", 4 );
	if( hb_xvmPlus() ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 1 );
lab00004: ;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 618 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushStringConst( ".T.", 3 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 6 );
lab00006: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 619 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushStringConst( ".T.", 3 );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 5 );
lab00008: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 620 );
	hb_xvmCopyLocals( 2, 13 );
	hb_xvmSetLine( 622 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 624 );
	hb_xvmPushStringConst( "<PRIMARY>", 9 );
	hb_xvmPopLocal( 4 );
lab00009: ;
	hb_xvmSetLine( 628 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 7 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 629 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 630 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 632 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 633 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 635 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 1 ) ) break;
lab00010: ;
	hb_xvmSetLine( 636 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00021;
	hb_xvmPushLocal( 6 );
	if( hb_xvmMacroPush( 43 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 637 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmMacroPush( 43 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 638 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00010;
lab00011: ;
	hb_xvmSetLine( 642 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 643 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 645 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 14 );
	goto lab00013;
lab00012: ;
	hb_xvmSetLine( 646 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 645 );
	if( hb_xvmLocalIncPush( 14 ) ) break;
lab00013: ;
	hb_xvmPushLocal( 12 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 649 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 650 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 652 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 653 );
	hb_xvmCopyLocals( 10, 9 );
lab00014: ;
	hb_xvmSetLine( 654 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmPushFuncSymbol( symbols + 36 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmSetLine( 655 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmMacroPush( 43 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	hb_xvmSetLine( 656 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00014;
lab00015: ;
	hb_xvmSetLine( 660 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 14 );
	goto lab00017;
lab00016: ;
	hb_xvmSetLine( 661 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 660 );
	if( hb_xvmLocalIncPush( 14 ) ) break;
lab00017: ;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 664 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 665 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00014;
lab00018: ;
	hb_xvmSetLine( 668 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 669 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 14 );
	goto lab00020;
lab00019: ;
	hb_xvmSetLine( 670 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 669 );
	if( hb_xvmLocalIncPush( 14 ) ) break;
lab00020: ;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 673 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00010;
lab00021: ;
	hb_xvmSetLine( 676 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 677 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	if( hb_xvmDo( 0 ) ) break;
lab00022: ;
	hb_xvmSetLine( 678 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_xvmSetLine( 679 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 680 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 681 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 682 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 683 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 14 );
	goto lab00024;
lab00023: ;
	hb_xvmSetLine( 684 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 683 );
	if( hb_xvmLocalIncPush( 14 ) ) break;
lab00024: ;
	hb_xvmPushLocal( 12 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 686 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 687 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 688 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00022;
lab00025: ;
	hb_xvmSetLine( 690 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 691 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 692 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 693 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 695 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

