/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_ini.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _SETGETLOGFILE );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( PROCNAME );
HB_FUNC_EXTERN( _ADDNEWGLOBAL );
HB_FUNC_EXTERN( _SETGETGLOBAL );
HB_FUNC( _LOGFILE );
HB_FUNC_EXTERN( HB_APARAMS );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( GETSTARTUPFOLDER );
HB_FUNC_EXTERN( HB_PS );
HB_FUNC_EXTERN( ISERRORLOGACTIVE );
HB_FUNC_EXTERN( HB_ISCHAR );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( FILE );
HB_FUNC_EXTERN( FOPEN );
HB_FUNC_EXTERN( FCREATE );
HB_FUNC_EXTERN( FSEEK );
HB_FUNC_EXTERN( FWRITE );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC_EXTERN( HB_ISNIL );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( TRIM );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( HB_DTOC );
HB_FUNC_EXTERN( HB_TSTOSTR );
HB_FUNC_EXTERN( FCLOSE );
HB_FUNC( _BEGININI );
HB_FUNC_EXTERN( AT );
HB_FUNC_EXTERN( HB_DIRBASE );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( HMG_CREATEFILE_UTF16LE_BOM );
HB_FUNC_EXTERN( MSGINFO );
HB_FUNC_EXTERN( FERROR );
HB_FUNC_EXTERN( HB_VFOPEN );
HB_FUNC_EXTERN( HB_VFEXISTS );
HB_FUNC_EXTERN( HB_VFCLOSE );
HB_FUNC( _GETINI );
HB_FUNC_EXTERN( _SETTYPE );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( GETPRIVATEPROFILESTRING );
HB_FUNC( XCHAR );
HB_FUNC( XVALUE );
HB_FUNC( _SETINI );
HB_FUNC_EXTERN( WRITEPRIVATEPROFILESTRING );
HB_FUNC( _DELINIENTRY );
HB_FUNC_EXTERN( DELINIENTRY );
HB_FUNC( _DELINISECTION );
HB_FUNC_EXTERN( DELINISECTION );
HB_FUNC( _ENDINI );
HB_FUNC( GETBEGINCOMMENT );
HB_FUNC_EXTERN( HB_ATOKENS );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( MEMOREAD );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( HB_ULEFT );
HB_FUNC( GETENDCOMMENT );
HB_FUNC( SETBEGINCOMMENT );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( ASIZE );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( AINS );
HB_FUNC_EXTERN( HB_MEMOWRIT );
HB_FUNC( SETENDCOMMENT );
HB_FUNC_EXTERN( HB_ULEN );
HB_FUNC_EXTERN( INT );
HB_FUNC_EXTERN( LTRIM );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( DTOS );
HB_FUNC( ATOC );
HB_FUNC_EXTERN( STOD );
HB_FUNC_EXTERN( VAL );
HB_FUNC( CTOA );
HB_FUNC_EXTERN( HB_USUBSTR );
HB_FUNC( _GETSECTIONNAMES );
HB_FUNC_EXTERN( _GETPRIVATEPROFILESECTIONNAMES );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( MSGSTOP );
HB_FUNC( _GETSECTION );
HB_FUNC_EXTERN( _GETPRIVATEPROFILESECTION );
HB_FUNC_EXTERN( LEFT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_INI )
{ "_SETGETLOGFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETGETLOGFILE )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "PROCNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCNAME )}, NULL },
{ "_ADDNEWGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ADDNEWGLOBAL )}, NULL },
{ "_SETGETGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETGLOBAL )}, NULL },
{ "_LOGFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _LOGFILE )}, NULL },
{ "HB_APARAMS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_APARAMS )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "GETSTARTUPFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSTARTUPFOLDER )}, NULL },
{ "HB_PS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PS )}, NULL },
{ "ISERRORLOGACTIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISERRORLOGACTIVE )}, NULL },
{ "HB_ISCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISCHAR )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "FILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FILE )}, NULL },
{ "FOPEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOPEN )}, NULL },
{ "FCREATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCREATE )}, NULL },
{ "FSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( FSEEK )}, NULL },
{ "FWRITE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FWRITE )}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "HB_ISNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNIL )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "TRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( TRIM )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "HB_DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DTOC )}, NULL },
{ "HB_TSTOSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_TSTOSTR )}, NULL },
{ "FCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCLOSE )}, NULL },
{ "_BEGININI", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGININI )}, NULL },
{ "AT", {HB_FS_PUBLIC}, {HB_FUNCNAME( AT )}, NULL },
{ "HB_DIRBASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DIRBASE )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "HMG_CREATEFILE_UTF16LE_BOM", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_CREATEFILE_UTF16LE_BOM )}, NULL },
{ "MSGINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGINFO )}, NULL },
{ "FERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERROR )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "HB_VFOPEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VFOPEN )}, NULL },
{ "HB_VFEXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VFEXISTS )}, NULL },
{ "HB_VFCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VFCLOSE )}, NULL },
{ "_GETINI", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETINI )}, NULL },
{ "_SETTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETTYPE )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "GETPRIVATEPROFILESTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPRIVATEPROFILESTRING )}, NULL },
{ "XCHAR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( XCHAR )}, NULL },
{ "XVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( XVALUE )}, NULL },
{ "_SETINI", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETINI )}, NULL },
{ "WRITEPRIVATEPROFILESTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( WRITEPRIVATEPROFILESTRING )}, NULL },
{ "_DELINIENTRY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DELINIENTRY )}, NULL },
{ "DELINIENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELINIENTRY )}, NULL },
{ "_DELINISECTION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DELINISECTION )}, NULL },
{ "DELINISECTION", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELINISECTION )}, NULL },
{ "_ENDINI", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDINI )}, NULL },
{ "GETBEGINCOMMENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETBEGINCOMMENT )}, NULL },
{ "HB_ATOKENS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ATOKENS )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "MEMOREAD", {HB_FS_PUBLIC}, {HB_FUNCNAME( MEMOREAD )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "HB_ULEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ULEFT )}, NULL },
{ "GETENDCOMMENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETENDCOMMENT )}, NULL },
{ "SETBEGINCOMMENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETBEGINCOMMENT )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "ASIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASIZE )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "AINS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AINS )}, NULL },
{ "HB_MEMOWRIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_MEMOWRIT )}, NULL },
{ "SETENDCOMMENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETENDCOMMENT )}, NULL },
{ "HB_ULEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ULEN )}, NULL },
{ "INT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INT )}, NULL },
{ "LTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( LTRIM )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "DTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOS )}, NULL },
{ "ATOC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ATOC )}, NULL },
{ "CLASSNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "STOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( STOD )}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "CTOA", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CTOA )}, NULL },
{ "HB_USUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_USUBSTR )}, NULL },
{ "_GETSECTIONNAMES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETSECTIONNAMES )}, NULL },
{ "_GETPRIVATEPROFILESECTIONNAMES", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETPRIVATEPROFILESECTIONNAMES )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "MSGSTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGSTOP )}, NULL },
{ "_GETSECTION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETSECTION )}, NULL },
{ "_GETPRIVATEPROFILESECTION", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETPRIVATEPROFILESECTION )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_INI, "h_ini.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_INI
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_INI )
   #include "hbiniseg.h"
#endif

HB_FUNC( _SETGETLOGFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 64 );
	hb_xvmPushStringConst( "_HMG_", 5 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmLocalAdd( 2 );
	hb_xvmSetLine( 65 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 67 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 68 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 69 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 72 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _LOGFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 7, 1 );
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_MsgLog.txt", 11 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 89 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 91 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 93 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
lab00002: ;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 98 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 99 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	goto lab00005;
lab00003: ;
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 101 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 102 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 104 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
lab00005: ;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmLocalAdd( 5 );
lab00006: ;
	hb_xvmSetLine( 110 );
	hb_xvmCopyLocals( 5, 8 );
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 113 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
lab00008: ;
	hb_xvmSetLine( 115 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 117 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 1 );
lab00009: ;
	hb_xvmSetLine( 121 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00033;
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00011;
lab00010: ;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
lab00011: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 123 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( -1L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 124 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00012: ;
	hb_xvmSetLine( 126 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00031;
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 132 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 3 ) ) break;
lab00013: ;
	hb_xvmSetLine( 134 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00032;
lab00014: ;
	hb_xvmSetLine( 136 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00030;
lab00015: ;
	hb_xvmSetLine( 137 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 139 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushStringConst( "''", 2 );
	goto lab00017;
lab00016: ;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
lab00017: ;
	hb_xvmPopLocal( 4 );
	goto lab00029;
lab00018: ;
	hb_xvmSetLine( 140 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00029;
lab00019: ;
	hb_xvmSetLine( 141 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushStringConst( ".T.", 3 );
	goto lab00021;
lab00020: ;
	hb_xvmPushStringConst( ".F.", 3 );
lab00021: ;
	hb_xvmPopLocal( 4 );
	goto lab00029;
lab00022: ;
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "DD.MM.YYYY", 10 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00029;
lab00023: ;
	hb_xvmSetLine( 147 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmPushStringConst( "ARRAY[", 6 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "]", 1 );
	hb_xvmLocalAdd( 4 );
	goto lab00029;
lab00024: ;
	hb_xvmSetLine( 148 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "H", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmPushStringConst( "HASH[", 5 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "]", 1 );
	hb_xvmLocalAdd( 4 );
	goto lab00029;
lab00025: ;
	hb_xvmSetLine( 149 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmPushStringConst( "'B'", 3 );
	hb_xvmPopLocal( 4 );
	goto lab00029;
lab00026: ;
	hb_xvmSetLine( 150 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "T", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00029;
lab00027: ;
	hb_xvmSetLine( 151 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmPushStringConst( "NIL", 3 );
	hb_xvmPopLocal( 4 );
	goto lab00029;
lab00028: ;
	hb_xvmSetLine( 152 );
	hb_xvmPushStringConst( "'", 1 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "'", 1 );
	hb_xvmLocalAdd( 4 );
lab00029: ;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "\x09", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 136 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00030: ;
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	goto lab00032;
lab00031: ;
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 3 ) ) break;
lab00032: ;
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
lab00033: ;
	hb_xvmSetLine( 163 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGININI )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "\\", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 171 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 1 );
lab00001: ;
	hb_xvmSetLine( 174 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushInteger( 114 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "UTF8", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 64 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
lab00003: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 177 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( -1L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushStringConst( "Error opening a file INI. DOS ERROR: ", 37 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 179 );
	hb_xvmRetInt( -1L );
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00009;
lab00005: ;
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushInteger( 64 );
	goto lab00007;
lab00006: ;
	hb_xvmPushInteger( 258 );
lab00007: ;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 193 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushStringConst( "Error opening a file INI. DOS ERROR: ", 37 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 196 );
	hb_xvmRetInt( -1L );
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 198 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
lab00009: ;
	hb_xvmSetLine( 207 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETINI )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushStringConst( "String", 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 214 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 216 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 217 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 5 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 219 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 220 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
lab00002: ;
	hb_xvmSetLine( 224 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 226 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETINI )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushStringConst( "Logical", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 233 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 234 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 4 );
lab00001: ;
	hb_xvmSetLine( 237 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DELINIENTRY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushStringConst( "Logical", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 244 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 245 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 248 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DELINISECTION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 253 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushStringConst( "Logical", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 255 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 259 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDINI )
{
   do {
	hb_xvmSetLine( 264 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 266 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETBEGINCOMMENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 0 );
	hb_xvmSetLine( 271 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 4 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 273 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 274 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 276 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 277 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 278 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 280 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "#;", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 281 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 282 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 4 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 283 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 284 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 4 );
lab00003: ;
	hb_xvmSetLine( 277 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00004: ;
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
lab00005: ;
	hb_xvmSetLine( 293 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETENDCOMMENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 0 );
	hb_xvmSetLine( 298 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 4 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 300 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 301 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 303 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 304 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 305 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 306 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 307 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "#;", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 308 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 309 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 4 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 310 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 311 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 4 );
lab00003: ;
	hb_xvmSetLine( 304 );
	if( hb_xvmLocalAddInt( 3, -1 ) ) break;
	hb_xvmPushLocal( 3 );
lab00004: ;
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
lab00005: ;
	hb_xvmSetLine( 320 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETBEGINCOMMENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 1 );
	hb_xvmSetLine( 325 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 5 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 327 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 329 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 330 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 332 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 333 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 334 );
	if( hb_xvmLocalDec( 3 ) ) break;
lab00001: ;
	hb_xvmSetLine( 336 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 337 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00012;
lab00002: ;
	hb_xvmSetLine( 338 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 339 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 340 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "#;", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 341 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 342 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	goto lab00005;
lab00003: ;
	hb_xvmSetLine( 344 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "#;", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 345 );
	hb_xvmPushStringConst( "#", 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 1 );
lab00004: ;
	hb_xvmSetLine( 347 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
lab00005: ;
	hb_xvmSetLine( 349 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00011;
lab00006: ;
	hb_xvmSetLine( 350 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 351 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00011;
lab00007: ;
	hb_xvmSetLine( 352 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 353 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 354 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00011;
lab00008: ;
	hb_xvmSetLine( 356 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 357 );
	if( hb_xvmLocalInc( 3 ) ) break;
	hb_xvmSetLine( 358 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 359 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "#;", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 360 );
	hb_xvmPushStringConst( "#", 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 1 );
lab00009: ;
	hb_xvmSetLine( 362 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 363 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 366 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00011: ;
	hb_xvmSetLine( 368 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 337 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00012: ;
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 370 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
lab00013: ;
	hb_xvmSetLine( 374 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETENDCOMMENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 1 );
	hb_xvmSetLine( 379 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 5 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 381 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 382 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 384 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 385 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 386 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 387 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 388 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 389 );
	if( hb_xvmLocalDec( 3 ) ) break;
lab00001: ;
	hb_xvmSetLine( 391 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 392 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00010;
lab00002: ;
	hb_xvmSetLine( 393 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 394 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 395 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 397 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "#;", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 398 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 402 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "#;", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 403 );
	hb_xvmPushStringConst( "#", 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 1 );
lab00003: ;
	hb_xvmSetLine( 405 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 6 );
lab00004: ;
	hb_xvmSetLine( 407 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00009;
lab00005: ;
	hb_xvmSetLine( 408 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 410 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00009;
lab00006: ;
	hb_xvmSetLine( 413 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "#;", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 414 );
	hb_xvmPushStringConst( "#", 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 1 );
lab00007: ;
	hb_xvmSetLine( 416 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 6 );
	hb_xvmSetLine( 418 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmLocalAdd( 6 );
	hb_xvmSetLine( 419 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 423 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmLocalAdd( 6 );
lab00009: ;
	hb_xvmSetLine( 392 );
	if( hb_xvmLocalAddInt( 4, -1 ) ) break;
	hb_xvmPushLocal( 4 );
lab00010: ;
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 426 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 427 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
lab00011: ;
	hb_xvmSetLine( 429 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmPushMemvar( symbols + 36 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
lab00012: ;
	hb_xvmSetLine( 433 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( XCHAR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 438 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 439 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 442 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CM", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmCopyLocals( 1, 3 );
	goto lab00012;
lab00001: ;
	hb_xvmSetLine( 443 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushInteger( 0 );
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 4 );
lab00003: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 20 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00012;
lab00004: ;
	hb_xvmSetLine( 444 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00012;
lab00005: ;
	hb_xvmSetLine( 445 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushStringConst( "T", 1 );
	goto lab00007;
lab00006: ;
	hb_xvmPushStringConst( "F", 1 );
lab00007: ;
	hb_xvmPopLocal( 3 );
	goto lab00012;
lab00008: ;
	hb_xvmSetLine( 446 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00012;
lab00009: ;
	hb_xvmSetLine( 447 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "UE", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushStringConst( "NIL", 3 );
	hb_xvmPopLocal( 3 );
	goto lab00012;
lab00010: ;
	hb_xvmSetLine( 448 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushStringConst( "{|| ... }", 9 );
	hb_xvmPopLocal( 3 );
	goto lab00012;
lab00011: ;
	hb_xvmSetLine( 449 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "O", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushStringConst( "{", 1 );
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "}", 1 );
	hb_xvmLocalAdd( 3 );
lab00012: ;
	hb_xvmSetLine( 452 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( XVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 460 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CM", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmCopyLocals( 1, 3 );
	goto lab00006;
lab00001: ;
	hb_xvmSetLine( 461 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 462 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 463 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "T", 1 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmPopLocal( 3 );
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 464 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 465 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
lab00006: ;
	hb_xvmSetLine( 468 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ATOC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 473 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 475 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 476 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 477 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 478 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 480 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00003: ;
	hb_xvmSetLine( 482 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00004: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 484 );
	hb_xvmPushStringConst( "A", 1 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CTOA )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 489 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 491 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 6 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 492 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 493 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 494 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 495 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmAddInt( 5L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 497 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 499 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00001;
lab00004: ;
	hb_xvmSetLine( 502 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETSECTIONNAMES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 510 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 512 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "\\", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 513 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 1 );
lab00001: ;
	hb_xvmSetLine( 516 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 517 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 518 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 519 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushLocal( 3 );
	{
		static const HB_BYTE codeblock[ 28 ] = {
			1, 0, 1, 0, 2, 0, 176, 15, 0, 95, 1, 12, 1, 28, 5, 100, 
			25, 11, 176, 64, 0, 95, 255, 95, 1, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 522 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	hb_xvmPushStringConst( "Can`t open ", 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Error", 5 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 525 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETSECTION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 2 );
	hb_xvmSetLine( 531 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 533 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "\\", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 534 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmLocalAdd( 2 );
lab00001: ;
	hb_xvmSetLine( 537 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 538 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 539 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 540 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 541 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "=", 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 542 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmInc() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 540 );
	if( hb_xvmLocalIncPush( 5 ) ) break;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 547 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	hb_xvmPushStringConst( "Can`t open ", 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Error", 5 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00006: ;
	hb_xvmSetLine( 550 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

