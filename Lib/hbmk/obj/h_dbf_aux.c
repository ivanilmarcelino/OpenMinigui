/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_dbf_aux.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( HMG_DBFTOARRAY );
HB_FUNC_EXTERN( RECNO );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( DBSTRUCT );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( HB_MACROBLOCK );
HB_FUNC_EXTERN( DBEVAL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( DBGOTO );
HB_FUNC( HMG_ARRAYTODBF );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HB_ATOKENS );
HB_FUNC_EXTERN( FCOUNT );
HB_FUNC_EXTERN( MIN );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( FIELDPOS );
HB_FUNC_EXTERN( FIELDTYPE );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( NETAPPEND );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_STATIC( CONVERTTYPE );
HB_FUNC_EXTERN( FIELDPUT );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( HB_VALTOSTR );
HB_FUNC_EXTERN( STOD );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( HB_TTOS );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( HB_STOT );
HB_FUNC_EXTERN( DTOS );
HB_FUNC_EXTERN( HB_CTOT );
HB_FUNC( HMG_DBFTOEXCEL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( __BREAKBLOCK );
HB_FUNC_EXTERN( CREATEOBJECT );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC( HMG_DBFSTRUCT );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( HB_FNAMEEXTSETDEF );
HB_FUNC_EXTERN( FOPEN );
HB_FUNC_EXTERN( FREAD );
HB_FUNC_EXTERN( BEFORATNUM );
HB_FUNC_EXTERN( BIN2I );
HB_FUNC_EXTERN( ASC );
HB_FUNC_EXTERN( FCLOSE );
HB_FUNC( HMG_RECTOHASH );
HB_FUNC_EXTERN( HSETCASEMATCH );
HB_FUNC_EXTERN( HSET );
HB_FUNC( HMG_HASHTOREC );
HB_FUNC_EXTERN( DBINFO );
HB_FUNC_EXTERN( DBRECORDINFO );
HB_FUNC_EXTERN( DBRLOCK );
HB_FUNC_EXTERN( HB_HEVAL );
HB_FUNC_EXTERN( DBRUNLOCK );
HB_FUNC( DBFCOPYREC );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( FIELDNAME );
HB_FUNC_EXTERN( FIELDGET );
HB_FUNC( DBFMODSTRU );
HB_FUNC_EXTERN( FERROR );
HB_FUNC_EXTERN( STUFF );
HB_FUNC_EXTERN( PADR );
HB_FUNC_EXTERN( FSEEK );
HB_FUNC_EXTERN( FWRITE );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_DBF_AUX )
{ "HMG_DBFTOARRAY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_DBFTOARRAY )}, NULL },
{ "RECNO", {HB_FS_PUBLIC}, {HB_FUNCNAME( RECNO )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "DBSTRUCT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSTRUCT )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "HB_MACROBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_MACROBLOCK )}, NULL },
{ "DBEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEVAL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DBGOTO", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTO )}, NULL },
{ "HMG_ARRAYTODBF", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_ARRAYTODBF )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HB_ATOKENS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ATOKENS )}, NULL },
{ "FCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCOUNT )}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "FIELDPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDPOS )}, NULL },
{ "FIELDTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDTYPE )}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "NETAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( NETAPPEND )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "CONVERTTYPE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( CONVERTTYPE )}, NULL },
{ "FIELDPUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDPUT )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "HB_VALTOSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VALTOSTR )}, NULL },
{ "STOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( STOD )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "HB_TTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_TTOS )}, NULL },
{ "CTOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CTOD )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "HB_STOT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_STOT )}, NULL },
{ "DTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOS )}, NULL },
{ "HB_CTOT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_CTOT )}, NULL },
{ "HMG_DBFTOEXCEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_DBFTOEXCEL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "__BREAKBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __BREAKBLOCK )}, NULL },
{ "CREATEOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATEOBJECT )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "ADD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "WORKBOOKS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACTIVESHEET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "RANGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "COLUMNS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_SCREENUPDATING", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_VALUE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ROWS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BOLD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "FONT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "AUTOFIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_VISIBLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HMG_DBFSTRUCT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_DBFSTRUCT )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "HB_FNAMEEXTSETDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FNAMEEXTSETDEF )}, NULL },
{ "FOPEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOPEN )}, NULL },
{ "FREAD", {HB_FS_PUBLIC}, {HB_FUNCNAME( FREAD )}, NULL },
{ "BEFORATNUM", {HB_FS_PUBLIC}, {HB_FUNCNAME( BEFORATNUM )}, NULL },
{ "BIN2I", {HB_FS_PUBLIC}, {HB_FUNCNAME( BIN2I )}, NULL },
{ "ASC", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASC )}, NULL },
{ "FCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCLOSE )}, NULL },
{ "HMG_RECTOHASH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_RECTOHASH )}, NULL },
{ "HSETCASEMATCH", {HB_FS_PUBLIC}, {HB_FUNCNAME( HSETCASEMATCH )}, NULL },
{ "HSET", {HB_FS_PUBLIC}, {HB_FUNCNAME( HSET )}, NULL },
{ "HMG_HASHTOREC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_HASHTOREC )}, NULL },
{ "DBINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBINFO )}, NULL },
{ "DBRECORDINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBRECORDINFO )}, NULL },
{ "DBRLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBRLOCK )}, NULL },
{ "HB_HEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_HEVAL )}, NULL },
{ "DBRUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBRUNLOCK )}, NULL },
{ "DBFCOPYREC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DBFCOPYREC )}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "FIELDNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDNAME )}, NULL },
{ "FIELDGET", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDGET )}, NULL },
{ "DBFMODSTRU", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DBFMODSTRU )}, NULL },
{ "FERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERROR )}, NULL },
{ "STUFF", {HB_FS_PUBLIC}, {HB_FUNCNAME( STUFF )}, NULL },
{ "PADR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PADR )}, NULL },
{ "FSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( FSEEK )}, NULL },
{ "FWRITE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FWRITE )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_DBF_AUX, "h_dbf_aux.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_DBF_AUX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_DBF_AUX )
   #include "hbiniseg.h"
#endif

HB_FUNC( HMG_DBFTOARRAY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 6 );
	hb_xvmSetLine( 43 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 44 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 47 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 48 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 49 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmFunction( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 20 ] = {
			1, 0, 1, 0, 1, 0, 96, 255, 255, 106, 2, 44, 0, 95, 1, 122, 
			1, 72, 139, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 50 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 54 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "{", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "}", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 59 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	{
		static const HB_BYTE codeblock[ 23 ] = {
			0, 0, 2, 0, 7, 0, 9, 0, 176, 8, 0, 95, 255, 48, 9, 0, 
			95, 254, 112, 0, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 61 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 63 );
	hb_xvmPushLocal( 7 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_ARRAYTODBF )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 13, 3 );
	hb_xvmSetLine( 103 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 106 );
	hb_xvmCopyLocals( 2, 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 112 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
lab00004: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 113 );
	hb_xvmPushLocal( 9 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 114 );
	hb_xvmPushLocal( 9 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 116 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 11 );
	goto lab00010;
lab00005: ;
	hb_xvmSetLine( 117 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushLocal( 11 );
	goto lab00007;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00007: ;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 118 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00009: ;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 116 );
	if( hb_xvmLocalIncPush( 11 ) ) break;
lab00010: ;
	hb_xvmPushLocal( 9 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 121 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 123 );
	hb_xvmPushSymbol( symbols + 9 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00011: ;
	hb_xvmSetLine( 126 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 12 );
	goto lab00019;
lab00012: ;
	hb_xvmSetLine( 127 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 129 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 130 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 16 );
	goto lab00020;
lab00013: ;
	hb_xvmSetLine( 134 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 11 );
	goto lab00017;
lab00014: ;
	hb_xvmSetLine( 135 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 136 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 140 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 142 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushStringConst( "+@", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 143 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
lab00015: ;
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 148 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
lab00016: ;
	hb_xvmSetLine( 134 );
	if( hb_xvmLocalIncPush( 11 ) ) break;
lab00017: ;
	hb_xvmPushLocal( 9 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 159 );
	hb_xvmPushSymbol( symbols + 9 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00018: ;
	hb_xvmSetLine( 126 );
	if( hb_xvmLocalIncPush( 12 ) ) break;
lab00019: ;
	hb_xvmPushLocal( 10 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
lab00020: ;
	hb_xvmSetLine( 163 );
	hb_xvmPushLocal( 16 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( CONVERTTYPE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 194 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 196 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 199 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CM", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00013;
lab00001: ;
	hb_xvmSetLine( 202 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 204 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "T", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00013;
lab00002: ;
	hb_xvmSetLine( 206 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 207 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00013;
lab00003: ;
	hb_xvmSetLine( 209 );
	hb_xvmPushDate( 0L );
	hb_xvmPopLocal( 1 );
	goto lab00013;
lab00004: ;
	hb_xvmSetLine( 212 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 214 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "LN", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmPopLocal( 1 );
	goto lab00013;
lab00005: ;
	hb_xvmSetLine( 216 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 217 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "Y,YES,T,.T.,TRUE", 16 );
	if( hb_xvmInstring() ) break;
	hb_xvmPopLocal( 1 );
	goto lab00013;
lab00006: ;
	hb_xvmSetLine( 219 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 1 );
	goto lab00013;
lab00007: ;
	hb_xvmSetLine( 222 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 224 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 225 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00013;
lab00008: ;
	hb_xvmSetLine( 227 );
	hb_xvmLocalSetInt( 1, 0L );
	goto lab00013;
lab00009: ;
	hb_xvmSetLine( 230 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "T", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 232 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 233 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "000000.000", 10 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00013;
lab00010: ;
	hb_xvmSetLine( 234 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 235 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00013;
lab00011: ;
	hb_xvmSetLine( 237 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00013;
lab00012: ;
	hb_xvmSetLine( 241 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 1 );
lab00013: ;
	hb_xvmSetLine( 246 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_DBFTOEXCEL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 7 );
	hb_xvmSetLine( 278 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 282 );
	hb_xvmLocalSetInt( 15, 1L );
	hb_xvmSetLine( 284 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 285 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 286 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmFunction( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 20 ] = {
			1, 0, 1, 0, 1, 0, 96, 255, 255, 106, 2, 44, 0, 95, 1, 122, 
			1, 72, 139, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 290 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 292 );
	hb_xvmSeqBegin();
	for( ;; ) {
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSeqBlock() ) break;
	hb_xvmSetLine( 293 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushStringConst( "Excel.Application", 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_stackPop();
	if( hb_xvmSeqEndTest() ) break;
	goto lab00002;
	}
	hb_xvmSetLine( 294 );
	if( hb_xvmSeqRecover() ) break;
	hb_stackPop();
	hb_xvmSetLine( 295 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushStringConst( "Excel not installed", 19 );
	hb_xvmPushStringConst( "Warning", 7 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 296 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 299 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushSymbol( symbols + 42 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 300 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 302 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 303 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 305 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 307 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 308 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 310 );
	hb_xvmPushStringConst( "{||{", 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "}}", 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 312 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 313 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	if( hb_xvmDo( 0 ) ) break;
lab00003: ;
	hb_xvmSetLine( 316 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	{
		static const HB_BYTE codeblock[ 37 ] = {
			0, 0, 3, 0, 13, 0, 15, 0, 9, 0, 48, 47, 0, 48, 48, 0, 
			95, 255, 96, 254, 255, 172, 112, 1, 48, 9, 0, 95, 253, 112, 0, 112, 
			1, 73, 95, 254, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 317 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 319 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 321 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 322 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 324 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_DBFSTRUCT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 359 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 361 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushInteger( 32 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 363 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushInteger( 109 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 364 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ".dbf", 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 367 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 64 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	if( hb_xvmGreaterEqualThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 369 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 32 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 32L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
lab00002: ;
	hb_xvmSetLine( 371 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 32 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 32L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 373 );
	hb_xvmPushInteger( 4 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 375 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushStringConst( "\x00", 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 376 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 12 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 378 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 380 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 17 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 381 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 385 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 17 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 386 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 18 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
lab00004: ;
	hb_xvmSetLine( 390 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00002;
lab00005: ;
	hb_xvmSetLine( 396 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00007;
lab00006: ;
	hb_xvmSetLine( 400 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 2 );
lab00007: ;
	hb_xvmSetLine( 404 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_RECTOHASH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 431 );
	hb_xvmHashGen( 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 434 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 436 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 437 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 438 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmFunction( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 20 ] = {
			1, 0, 1, 0, 1, 0, 96, 255, 255, 106, 2, 44, 0, 95, 1, 122, 
			1, 72, 139, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 439 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 442 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 2 );
lab00003: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 444 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 447 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "{", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "}", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 452 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 4 );
	{
		static const HB_BYTE codeblock[ 23 ] = {
			2, 0, 2, 0, 5, 0, 3, 0, 176, 66, 0, 95, 254, 95, 255, 95, 
			2, 1, 95, 1, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 454 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_HASHTOREC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 2 );
	hb_xvmSetLine( 481 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushInteger( 36 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 482 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 483 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 484 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 489 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushInteger( 20 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00001: ;
	hb_xvmSetLine( 491 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 492 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushLocal( 1 );
	{
		static const HB_BYTE codeblock[ 19 ] = {
			2, 0, 0, 0, 176, 24, 0, 176, 18, 0, 95, 1, 12, 1, 95, 2, 
			12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 494 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 495 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushLocal( 1 );
	{
		static const HB_BYTE codeblock[ 24 ] = {
			3, 0, 1, 0, 7, 0, 176, 24, 0, 176, 18, 0, 95, 255, 95, 3, 
			1, 12, 1, 95, 2, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 498 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 499 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 502 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 5 );
lab00005: ;
	hb_xvmSetLine( 506 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DBFCOPYREC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 2 );
	hb_xvmSetLine( 533 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 539 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 541 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	if( hb_xvmDo( 0 ) ) break;
	if( hb_xvmPopAlias() ) break;
lab00001: ;
	hb_xvmSetLine( 545 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 547 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 550 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 552 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	if( hb_xvmPopAlias() ) break;
lab00003: ;
	hb_xvmSetLine( 545 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00004: ;
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 558 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DBFMODSTRU )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 2 );
	hb_xvmSetLine( 590 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushInteger( 32 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 596 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMultByInt( 32L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 597 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 598 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 18 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 599 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 601 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 603 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 32 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqualIntIs( 32L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 605 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00005;
lab00001: ;
	hb_xvmSetLine( 607 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 609 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00005;
lab00002: ;
	hb_xvmSetLine( 613 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 615 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 32 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDec() ) break;
	if( hb_xvmMult() ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPushInteger( 10 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 616 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 12 );
	hb_xvmPushInteger( 32 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDec() ) break;
	if( hb_xvmMult() ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 613 );
	if( hb_xvmLocalIncPush( 8 ) ) break;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 620 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 32 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 622 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 624 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 7 );
lab00005: ;
	hb_xvmSetLine( 630 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 1 ) ) break;
lab00006: ;
	hb_xvmSetLine( 634 );
	hb_xvmPushLocal( 7 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

