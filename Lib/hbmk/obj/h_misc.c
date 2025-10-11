/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_misc.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( COMPRESSFILES );
HB_FUNC_EXTERN( FILE );
HB_FUNC_EXTERN( FERASE );
HB_FUNC_EXTERN( HB_ZIPOPEN );
HB_FUNC_EXTERN( HB_FILEEXISTS );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( CFILENOPATH );
HB_FUNC_EXTERN( HB_ZIPSTOREFILE );
HB_FUNC_EXTERN( HB_ZIPCLOSE );
HB_FUNC( UNCOMPRESSFILES );
HB_FUNC_EXTERN( HB_UNZIPOPEN );
HB_FUNC_EXTERN( HB_UNZIPFILEFIRST );
HB_FUNC_EXTERN( HB_UNZIPFILEINFO );
HB_FUNC_EXTERN( CFILEPATH );
HB_FUNC_EXTERN( HB_DIREXISTS );
HB_FUNC_EXTERN( HB_DIRSEPADD );
HB_FUNC_EXTERN( HB_DIRBUILD );
HB_FUNC_EXTERN( HB_UNZIPEXTRACTCURRENTFILE );
HB_FUNC_EXTERN( HB_UNZIPFILENEXT );
HB_FUNC_EXTERN( HB_UNZIPCLOSE );
HB_FUNC( GETDATA );
HB_FUNC_EXTERN( ADIR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( __SETCENTURY );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( MEMOREAD );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( MEMOLINE );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( MLCOUNT );
HB_FUNC_EXTERN( AADD );
HB_FUNC( SENDDATA );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( YEAR );
HB_FUNC_EXTERN( MONTH );
HB_FUNC_EXTERN( DAY );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( MEMOWRIT );
HB_FUNC( HMG_RAEVAL );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( RASCAN );
HB_FUNC( HMG_AREVERSE );
HB_FUNC_EXTERN( ASIZE );
HB_FUNC( HMG_CLRTOHTML );
HB_FUNC_EXTERN( LOWER );
HB_FUNC_EXTERN( HB_NUMTOHEX );
HB_FUNC_EXTERN( RIGHT );
HB_FUNC( HMG_FILECOPY );
HB_FUNC_EXTERN( HB_VFCOPYFILEEX );
HB_FUNC( HMG_CREATELINK );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( _DETERMINEKEY );
HB_FUNC_EXTERN( C_CREATELINK );
HB_FUNC( UCHARTOVAL );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_STATIC( IFNIL );
HB_FUNC( NSTRTONUM );
HB_FUNC_STATIC( DCHARTODATE );
HB_FUNC_EXTERN( LTRIM );
HB_FUNC_EXTERN( TRIM );
HB_FUNC_EXTERN( BEFORATNUM );
HB_FUNC_EXTERN( CHARONLY );
HB_FUNC_EXTERN( RAT );
HB_FUNC_EXTERN( OCCURS );
HB_FUNC_EXTERN( CHARREPL );
HB_FUNC_EXTERN( CHARREM );
HB_FUNC_EXTERN( HB_APARAMS );
HB_FUNC_STATIC( DALPHATODATE );
HB_FUNC_EXTERN( ISDIGIT );
HB_FUNC_EXTERN( STOD );
HB_FUNC_EXTERN( CMONTH );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( STRZERO );
HB_FUNC_STATIC( PARSENUMSFROMDATESTR );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC( TCDOMAIL );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( HBOBJECT );
HB_FUNC_STATIC( TCDOMAIL_NEW );
HB_FUNC_STATIC( TCDOMAIL_ACTIVATE );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( __BREAKBLOCK );
HB_FUNC_EXTERN( CREATEOBJECT );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( MSGSTOP );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC_EXTERN( CVALTOCHAR );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_MISC )
{ "COMPRESSFILES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( COMPRESSFILES )}, NULL },
{ "FILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FILE )}, NULL },
{ "FERASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERASE )}, NULL },
{ "HB_ZIPOPEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ZIPOPEN )}, NULL },
{ "HB_FILEEXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FILEEXISTS )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__ENUMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CFILENOPATH", {HB_FS_PUBLIC}, {HB_FUNCNAME( CFILENOPATH )}, NULL },
{ "HB_ZIPSTOREFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ZIPSTOREFILE )}, NULL },
{ "HB_ZIPCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ZIPCLOSE )}, NULL },
{ "UNCOMPRESSFILES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UNCOMPRESSFILES )}, NULL },
{ "HB_UNZIPOPEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UNZIPOPEN )}, NULL },
{ "HB_UNZIPFILEFIRST", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UNZIPFILEFIRST )}, NULL },
{ "HB_UNZIPFILEINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UNZIPFILEINFO )}, NULL },
{ "CFILEPATH", {HB_FS_PUBLIC}, {HB_FUNCNAME( CFILEPATH )}, NULL },
{ "HB_DIREXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DIREXISTS )}, NULL },
{ "HB_DIRSEPADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DIRSEPADD )}, NULL },
{ "HB_DIRBUILD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DIRBUILD )}, NULL },
{ "HB_UNZIPEXTRACTCURRENTFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UNZIPEXTRACTCURRENTFILE )}, NULL },
{ "HB_UNZIPFILENEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UNZIPFILENEXT )}, NULL },
{ "HB_UNZIPCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UNZIPCLOSE )}, NULL },
{ "GETDATA", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETDATA )}, NULL },
{ "ADIR", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADIR )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "__SETCENTURY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETCENTURY )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "MEMOREAD", {HB_FS_PUBLIC}, {HB_FUNCNAME( MEMOREAD )}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "MEMOLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( MEMOLINE )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "CTOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CTOD )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "MLCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( MLCOUNT )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "SENDDATA", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SENDDATA )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "YEAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( YEAR )}, NULL },
{ "MONTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( MONTH )}, NULL },
{ "DAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( DAY )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "MEMOWRIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( MEMOWRIT )}, NULL },
{ "HMG_RAEVAL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_RAEVAL )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "RASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RASCAN )}, NULL },
{ "HMG_AREVERSE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_AREVERSE )}, NULL },
{ "ASIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASIZE )}, NULL },
{ "HMG_CLRTOHTML", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_CLRTOHTML )}, NULL },
{ "LOWER", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOWER )}, NULL },
{ "HB_NUMTOHEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NUMTOHEX )}, NULL },
{ "RIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RIGHT )}, NULL },
{ "HMG_FILECOPY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_FILECOPY )}, NULL },
{ "HB_VFCOPYFILEEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VFCOPYFILEEX )}, NULL },
{ "HMG_CREATELINK", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_CREATELINK )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "_DETERMINEKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DETERMINEKEY )}, NULL },
{ "C_CREATELINK", {HB_FS_PUBLIC}, {HB_FUNCNAME( C_CREATELINK )}, NULL },
{ "UCHARTOVAL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UCHARTOVAL )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "IFNIL", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( IFNIL )}, NULL },
{ "NSTRTONUM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( NSTRTONUM )}, NULL },
{ "DCHARTODATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( DCHARTODATE )}, NULL },
{ "LTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( LTRIM )}, NULL },
{ "TRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( TRIM )}, NULL },
{ "BEFORATNUM", {HB_FS_PUBLIC}, {HB_FUNCNAME( BEFORATNUM )}, NULL },
{ "CHARONLY", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHARONLY )}, NULL },
{ "RAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RAT )}, NULL },
{ "OCCURS", {HB_FS_PUBLIC}, {HB_FUNCNAME( OCCURS )}, NULL },
{ "CHARREPL", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHARREPL )}, NULL },
{ "CHARREM", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHARREM )}, NULL },
{ "HB_APARAMS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_APARAMS )}, NULL },
{ "DALPHATODATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( DALPHATODATE )}, NULL },
{ "ISDIGIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISDIGIT )}, NULL },
{ "STOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( STOD )}, NULL },
{ "CMONTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( CMONTH )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "STRZERO", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRZERO )}, NULL },
{ "PARSENUMSFROMDATESTR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( PARSENUMSFROMDATESTR )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "TCDOMAIL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCDOMAIL )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "HBOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBOBJECT )}, NULL },
{ "ADDMULTICLSDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCDOMAIL_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCDOMAIL_NEW )}, NULL },
{ "TCDOMAIL_ACTIVATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCDOMAIL_ACTIVATE )}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CTEXTBODY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CSUBJECT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CSERVER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NPORT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CUSER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CPASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LRECEIPT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NPRIORITY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_AORIGIN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ARECIPIENTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_AFILES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BEMAIL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__BREAKBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __BREAKBLOCK )}, NULL },
{ "CREATEOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATEOBJECT )}, NULL },
{ "AORIGIN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "_FROM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "ARECIPIENTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_TO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CCOPY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BCC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_SUBJECT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CSUBJECT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CTEXTBODY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HTMLBODY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_TEXTBODY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CHARSET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BODYPART", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AFILES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDATTACHMENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "FIELDS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CONFIGURATION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_VALUE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ITEM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CSERVER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NPORT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CUSER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CPASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NTIMEOUT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "UPDATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NPRIORITY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LRECEIPT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_DSNOPTIONS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SEND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LSUCCESS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MSGSTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGSTOP )}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "CVALTOCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CVALTOCHAR )}, NULL },
{ "GENCODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SUBCODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OSCODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SUBSYSTEM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DESCRIPTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_MISC, "h_misc.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_MISC
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_MISC )
   #include "hbiniseg.h"
#endif

HB_FUNC( COMPRESSFILES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 6 );
	hb_xvmSetLine( 62 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 4 );
lab00001: ;
	hb_xvmSetLine( 64 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 66 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 67 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 2 );
	goto lab00004;
lab00003: ;
	hb_xvmPushNil();
lab00004: ;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmSetLine( 76 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
lab00005: ;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 79 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00006: ;
	hb_xvmSetLine( 82 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLocal( 9 );
	goto lab00008;
lab00007: ;
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
lab00008: ;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 86 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
lab00009: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 1 ) ) break;
lab00010: ;
	hb_xvmSetLine( 92 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( UNCOMPRESSFILES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 11, 3 );
	hb_xvmSetLine( 97 );
	hb_xvmLocalSetInt( 4, 0L );
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
lab00001: ;
	hb_xvmSetLine( 104 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLocalByRef( 13 );
	if( hb_xvmDo( 11 ) ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 112 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 113 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 3 );
	goto lab00005;
lab00004: ;
	hb_xvmPushNil();
lab00005: ;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 118 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00001;
lab00006: ;
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 124 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETDATA )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 14, 0 );
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 25 ) ) break;
	if( hb_xvmArrayItemPush( 183L ) ) break;
	if( hb_xvmPushMemvar( symbols + 25 ) ) break;
	if( hb_xvmArrayItemPush( 181L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".*", 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 132 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 5 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 134 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( "yyyy.mm.dd", 10 );
	goto lab00002;
lab00001: ;
	hb_xvmPushStringConst( "yy.mm.dd", 8 );
lab00002: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 25 ) ) break;
	if( hb_xvmArrayItemPush( 183L ) ) break;
	if( hb_xvmPushMemvar( symbols + 25 ) ) break;
	if( hb_xvmArrayItemPush( 181L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".*", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 140 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 142 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmPushMemvar( symbols + 25 ) ) break;
	if( hb_xvmArrayItemPush( 183L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 11 );
	hb_xvmPushInteger( 99 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 11 );
	hb_xvmPushInteger( 99 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 150 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 12 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 153 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 14 );
	hb_xvmPushInteger( 99 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushInteger( 254 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 158 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	goto lab00021;
lab00003: ;
	hb_xvmSetLine( 160 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 161 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	goto lab00021;
lab00004: ;
	hb_xvmSetLine( 162 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 163 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	goto lab00021;
lab00005: ;
	hb_xvmSetLine( 164 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 165 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "T", 1 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmPopLocal( 5 );
	goto lab00021;
lab00006: ;
	hb_xvmSetLine( 169 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 171 );
	hb_xvmLocalSetInt( 2, 3L );
lab00007: ;
	hb_xvmSetLine( 173 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 12 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 14 );
	hb_xvmPushInteger( 99 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 178 );
	if( hb_xvmLocalInc( 2 ) ) break;
	hb_xvmSetLine( 180 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushInteger( 254 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 183 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00011;
lab00008: ;
	hb_xvmSetLine( 185 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00011;
lab00009: ;
	hb_xvmSetLine( 187 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 188 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 189 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 190 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "T", 1 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmPopLocal( 6 );
lab00011: ;
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 195 );
	if( hb_xvmLocalInc( 2 ) ) break;
	goto lab00007;
lab00012: ;
	hb_xvmSetLine( 199 );
	hb_xvmCopyLocals( 7, 5 );
	goto lab00021;
lab00013: ;
	hb_xvmSetLine( 202 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmPushLocal( 4 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 204 );
	hb_xvmLocalSetInt( 2, 3L );
	hb_xvmSetLine( 206 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayDim( 2 );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 208 );
	hb_xvmLocalSetInt( 8, 1L );
	hb_xvmSetLine( 209 );
	hb_xvmLocalSetInt( 9, 1L );
lab00014: ;
	hb_xvmSetLine( 211 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 213 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 12 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 214 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 14 );
	hb_xvmPushInteger( 99 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 216 );
	if( hb_xvmLocalInc( 2 ) ) break;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushInteger( 254 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 221 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 222 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00018;
lab00015: ;
	hb_xvmSetLine( 223 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 224 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00018;
lab00016: ;
	hb_xvmSetLine( 225 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 226 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00018;
lab00017: ;
	hb_xvmSetLine( 227 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 228 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "T", 1 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmPopLocal( 6 );
lab00018: ;
	hb_xvmSetLine( 231 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 233 );
	if( hb_xvmLocalInc( 9 ) ) break;
	hb_xvmSetLine( 234 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 235 );
	if( hb_xvmLocalInc( 8 ) ) break;
	hb_xvmSetLine( 236 );
	hb_xvmLocalSetInt( 9, 1L );
lab00019: ;
	hb_xvmSetLine( 239 );
	if( hb_xvmLocalInc( 2 ) ) break;
	goto lab00014;
lab00020: ;
	hb_xvmSetLine( 243 );
	hb_xvmCopyLocals( 7, 5 );
lab00021: ;
	hb_xvmSetLine( 247 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 25 ) ) break;
	if( hb_xvmArrayItemPush( 183L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00022: ;
	hb_xvmSetLine( 251 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 253 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SENDDATA )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 2 );
	hb_xvmSetLine( 261 );
	if( hb_xvmPushMemvar( symbols + 25 ) ) break;
	if( hb_xvmArrayItemPush( 183L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 25 ) ) break;
	if( hb_xvmArrayItemPush( 181L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 39 );
	if( hb_xvmPushMemvar( symbols + 25 ) ) break;
	hb_xvmPushInteger( 182 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEq() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 9 );
	hb_xvmSetLine( 263 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 265 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 267 );
	hb_xvmPushStringConst( "#DataRows=", 10 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	hb_xvmLocalAdd( 3 );
	hb_xvmSetLine( 268 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( "#DataCols=0\x0D\x0A", 13 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 270 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00009;
lab00001: ;
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 274 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 275 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 6 );
	hb_xvmSetLine( 276 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00008;
lab00002: ;
	hb_xvmSetLine( 277 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 278 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushStringConst( "T", 1 );
	goto lab00004;
lab00003: ;
	hb_xvmPushStringConst( "F", 1 );
lab00004: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00008;
lab00005: ;
	hb_xvmSetLine( 280 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 281 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 282 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00008;
lab00006: ;
	hb_xvmSetLine( 283 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 284 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 285 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 287 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushStringConst( "SendData: Type Not Supported.", 29 );
	if( hb_xvmDo( 1 ) ) break;
lab00008: ;
	hb_xvmSetLine( 290 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( "#DataBlock=", 11 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 270 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00009: ;
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 295 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00030;
lab00010: ;
	hb_xvmSetLine( 299 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 300 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 302 );
	hb_xvmPushStringConst( "#DataRows=", 10 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	hb_xvmLocalAdd( 3 );
	hb_xvmSetLine( 303 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( "#DataCols=", 10 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 305 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00021;
lab00011: ;
	hb_xvmSetLine( 307 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	goto lab00020;
lab00012: ;
	hb_xvmSetLine( 309 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 311 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 312 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 6 );
	hb_xvmSetLine( 313 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00019;
lab00013: ;
	hb_xvmSetLine( 314 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 315 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushStringConst( "T", 1 );
	goto lab00015;
lab00014: ;
	hb_xvmPushStringConst( "F", 1 );
lab00015: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 316 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00019;
lab00016: ;
	hb_xvmSetLine( 317 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 318 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 319 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00019;
lab00017: ;
	hb_xvmSetLine( 320 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 321 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 322 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00019;
lab00018: ;
	hb_xvmSetLine( 324 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushStringConst( "SendData: Type Not Supported.", 29 );
	if( hb_xvmDo( 1 ) ) break;
lab00019: ;
	hb_xvmSetLine( 327 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( "#DataBlock=", 11 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 328 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 307 );
	if( hb_xvmLocalIncPush( 5 ) ) break;
lab00020: ;
	hb_xvmPushLocal( 11 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 305 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00021: ;
	hb_xvmPushLocal( 10 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 333 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00030;
lab00022: ;
	hb_xvmSetLine( 339 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 341 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 342 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 6 );
	hb_xvmSetLine( 343 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00029;
lab00023: ;
	hb_xvmSetLine( 344 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 345 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmPushStringConst( "T", 1 );
	goto lab00025;
lab00024: ;
	hb_xvmPushStringConst( "F", 1 );
lab00025: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 346 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00029;
lab00026: ;
	hb_xvmSetLine( 347 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 348 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 349 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00029;
lab00027: ;
	hb_xvmSetLine( 350 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 351 );
	hb_xvmCopyLocals( 2, 6 );
	hb_xvmSetLine( 352 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	goto lab00029;
lab00028: ;
	hb_xvmSetLine( 354 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushStringConst( "SendData: Type Not Supported.", 29 );
	if( hb_xvmDo( 1 ) ) break;
lab00029: ;
	hb_xvmSetLine( 357 );
	hb_xvmPushStringConst( "#DataRows=0\x0D\x0A", 13 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 358 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( "#DataCols=0\x0D\x0A", 13 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 360 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( "#DataBlock=", 11 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 361 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 363 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00030: ;
	hb_xvmSetLine( 367 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_RAEVAL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 372 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 374 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 376 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	{
		static const HB_BYTE codeblock[ 26 ] = {
			1, 0, 2, 0, 2, 0, 5, 0, 48, 7, 0, 95, 255, 95, 1, 95, 
			254, 112, 2, 73, 96, 254, 255, 169, 9, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 380 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_AREVERSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 385 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 2 );
	hb_xvmLocalSetInt( 3, 1L );
	hb_xvmSetLine( 387 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 389 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 391 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	{
		static const HB_BYTE codeblock[ 20 ] = {
			1, 0, 2, 0, 2, 0, 3, 0, 95, 1, 165, 95, 255, 96, 254, 255, 
			158, 170, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 395 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_CLRTOHTML )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 403 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 6 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 405 );
	hb_xvmPushStringConst( "#", 1 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_FILECOPY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 411 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 457 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmLocalSetInt( 3, 65536L );
lab00001: ;
	hb_xvmSetLine( 459 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 4 );
	goto lab00003;
lab00002: ;
	hb_xvmPushNil();
lab00003: ;
	if( hb_xvmFunction( 5 ) ) break;
	if( hb_xvmEqualInt( 0L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 463 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_CREATELINK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 9 );
	hb_xvmSetLine( 468 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 470 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 471 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
lab00001: ;
	hb_xvmSetLine( 474 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 10 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( UCHARTOVAL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 480 );
	hb_xvmPushStringConst( "|.T.|T|TRUE|YES|SI|", 19 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 481 );
	hb_xvmPushStringConst( "|.F.|F|FALSE|NO|", 16 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 483 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	hb_xvmPushStringConst( "CDLMN", 5 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 485 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "M", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 486 );
	hb_xvmPushStringConst( "C", 1 );
	hb_xvmPopLocal( 2 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 491 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
lab00002: ;
	hb_xvmSetLine( 495 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "T", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 496 );
	hb_xvmPushStringConst( "D", 1 );
	hb_xvmPopLocal( 2 );
lab00003: ;
	hb_xvmSetLine( 499 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 501 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 505 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 507 );
	hb_xvmCopyLocals( 1, 3 );
	goto lab00014;
lab00004: ;
	hb_xvmSetLine( 509 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 511 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00014;
lab00005: ;
	hb_xvmSetLine( 513 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 515 );
	hb_xvmPushStringConst( "|", 1 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "|", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmInstring() ) break;
	hb_xvmPopLocal( 3 );
	goto lab00014;
lab00006: ;
	hb_xvmSetLine( 517 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 519 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00014;
lab00007: ;
	hb_xvmSetLine( 523 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 525 );
	hb_xvmPushStringConst( "N", 1 );
	hb_xvmPopLocal( 2 );
	goto lab00012;
lab00008: ;
	hb_xvmSetLine( 527 );
	hb_xvmPushStringConst( "|", 1 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "|", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 529 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 530 );
	hb_xvmPushStringConst( "L", 1 );
	hb_xvmPopLocal( 2 );
	goto lab00012;
lab00009: ;
	hb_xvmSetLine( 532 );
	hb_xvmPushStringConst( "|", 1 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "|", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 534 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 535 );
	hb_xvmPushStringConst( "L", 1 );
	hb_xvmPopLocal( 2 );
	goto lab00012;
lab00010: ;
	hb_xvmSetLine( 539 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 541 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 543 );
	hb_xvmCopyLocals( 1, 3 );
	hb_xvmSetLine( 544 );
	hb_xvmPushStringConst( "C", 1 );
	hb_xvmPopLocal( 2 );
	goto lab00012;
lab00011: ;
	hb_xvmSetLine( 548 );
	hb_xvmPushStringConst( "D", 1 );
	hb_xvmPopLocal( 2 );
lab00012: ;
	goto lab00014;
lab00013: ;
	hb_xvmSetLine( 558 );
	hb_xvmCopyLocals( 1, 3 );
	hb_xvmSetLine( 559 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
lab00014: ;
	hb_xvmSetLine( 563 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( NSTRTONUM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 3 );
	hb_xvmSetLine( 568 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 569 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 570 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 573 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 574 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "+", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 575 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 578 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 579 );
	hb_xvmPushStringConst( "-", 1 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 580 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00002: ;
	hb_xvmSetLine( 583 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "%", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 584 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 585 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00003: ;
	hb_xvmSetLine( 588 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 589 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 1 );
lab00004: ;
	hb_xvmSetLine( 592 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushStringConst( "0123456789,.", 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 595 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushStringConst( ",", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 596 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 597 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushLocal( 8 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 598 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 601 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushStringConst( ",", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 604 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 2 );
	goto lab00014;
lab00006: ;
	hb_xvmSetLine( 606 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 608 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 2 );
	goto lab00014;
lab00007: ;
	hb_xvmSetLine( 610 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 613 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 2 );
	goto lab00014;
lab00008: ;
	hb_xvmSetLine( 615 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 618 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 2 );
	goto lab00014;
lab00009: ;
	hb_xvmSetLine( 620 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmPushLocal( 8 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmAddInt( -3L ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 622 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 2 );
	goto lab00014;
lab00010: ;
	hb_xvmSetLine( 624 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmPushLocal( 7 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmAddInt( -3L ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 626 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 2 );
	goto lab00014;
lab00011: ;
	hb_xvmSetLine( 630 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00013;
lab00012: ;
	hb_xvmPushLocal( 2 );
lab00013: ;
	hb_xvmPopLocal( 2 );
lab00014: ;
	hb_xvmSetLine( 634 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 636 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushStringConst( ",", 1 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00016;
lab00015: ;
	hb_xvmSetLine( 640 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushStringConst( ",", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
lab00016: ;
	hb_xvmSetLine( 644 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 645 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushDouble( * ( double * ) "{\x14\xAE" "G\xE1" "z\x84\?", 10, 2 );
	if( hb_xvmMultEqPop() ) break;
lab00017: ;
	hb_xvmSetLine( 650 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( IFNIL )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 2, 0 );
	hb_xvmSetLine( 655 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 658 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 659 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 662 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00002: ;
	hb_xvmSetLine( 664 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 665 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 668 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
lab00004: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 670 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( DCHARTODATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 680 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 681 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 684 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterEqualThenIntIs( 8L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 685 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 689 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterEqualThenIntIs( 10L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 6 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 9 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 690 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 693 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 694 );
	hb_xvmPushStringConst( "0", 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 1 );
lab00004: ;
	hb_xvmSetLine( 697 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 698 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmLocalAdd( 1 );
lab00005: ;
	hb_xvmSetLine( 701 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 703 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 705 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 707 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 709 );
	hb_xvmPushStringConst( "dd/mm/yy", 8 );
	hb_xvmPushStringConst( "mm/dd/yy", 8 );
	hb_xvmPushStringConst( "yy/mm/dd", 8 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00006: ;
	hb_xvmSetLine( 711 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 713 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 714 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 715 );
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 720 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
lab00008: ;
	hb_xvmEnumEnd();
lab00009: ;
	hb_xvmSetLine( 724 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 726 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( DALPHATODATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 1 );
	hb_xvmSetLine( 731 );
	hb_xvmPushDate( 0L );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 733 );
	hb_xvmPushInteger( 12 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 736 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 738 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "01", 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 740 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 742 );
	hb_xvmCopyLocals( 4, 3 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 736 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00003: ;
	if( hb_xvmGreaterThenIntIs( 12L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
lab00004: ;
	hb_xvmSetLine( 749 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 751 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 753 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 755 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 759 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmLessThenIntIs( 100L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 761 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 762 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushInteger( 1900 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 764 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 765 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushInteger( 100 );
	if( hb_xvmPlusEqPop() ) break;
lab00006: ;
	hb_xvmSetLine( 772 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
lab00007: ;
	hb_xvmSetLine( 776 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( PARSENUMSFROMDATESTR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 781 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 782 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 785 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00001: ;
	hb_xvmSetLine( 787 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 789 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 793 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( ":", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 795 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 799 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 801 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 802 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 3 );
lab00004: ;
	hb_xvmSetLine( 808 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00005: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 810 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 812 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 816 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 818 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 822 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	hb_xvmPushLocal( 2 );
	{
		static const HB_BYTE codeblock[ 29 ] = {
			2, 0, 1, 0, 2, 0, 95, 1, 100, 8, 28, 5, 121, 25, 9, 176, 
			30, 0, 95, 1, 12, 1, 165, 95, 255, 95, 2, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 824 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmGreaterThenIntIs( 31L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 826 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 827 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 828 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00008: ;
	hb_xvmSetLine( 832 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TCDOMAIL )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 153 );
	hb_xvmSetLine( 50 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushStaticByRef( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSeqAlways();
	do {
	hb_xvmLocalSetInt( 1, 1L );
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "TCDOMail", 8 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 83 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 53 );
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 32L ) ) break;
	hb_xvmPushStringConst( "bEmail", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 56 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cSubject", 8 );
	hb_xvmPushStringConst( "cTextBody", 9 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 57 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cServer", 7 );
	hb_xvmPushStringConst( "nPort", 5 );
	hb_xvmPushStringConst( "cUser", 5 );
	hb_xvmPushStringConst( "cPass", 5 );
	hb_xvmArrayGen( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 58 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lReceipt", 8 );
	hb_xvmPushStringConst( "nPriority", 9 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 59 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aOrigin", 7 );
	hb_xvmPushStringConst( "aRecipients", 11 );
	hb_xvmPushStringConst( "aFiles", 6 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 62 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CHARACTER", 9 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "CCopy", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 63 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "NUMERIC", 7 );
	hb_xvmPushInteger( 30 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nTimeout", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 66 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lSuccess", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 71 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 91 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 8L ) ) break;
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 73 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Activate", 8 );
	hb_xvmPushSymbol( symbols + 92 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 75 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushStaticByRef( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushVParams();
	if( hb_xvmMacroSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCDOMAIL_NEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 11 );
	hb_xvmSetLine( 116 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( "", 0 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 6 );
lab00002: ;
	hb_xvmPopLocal( 6 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushStringConst( "", 0 );
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 5 );
lab00004: ;
	hb_xvmPopLocal( 5 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushStringConst( "", 0 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 1 );
lab00006: ;
	hb_xvmPopLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushInteger( 465 );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 2 );
lab00008: ;
	hb_xvmPopLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushStringConst( "", 0 );
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 3 );
lab00010: ;
	hb_xvmPopLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushStringConst( "", 0 );
	goto lab00012;
lab00011: ;
	hb_xvmPushLocal( 4 );
lab00012: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00014;
lab00013: ;
	hb_xvmPushLocal( 8 );
lab00014: ;
	hb_xvmPopLocal( 8 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushInteger( 1 );
	goto lab00016;
lab00015: ;
	hb_xvmPushLocal( 7 );
lab00016: ;
	hb_xvmPopLocal( 7 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmArrayGen( 0 );
	goto lab00018;
lab00017: ;
	hb_xvmPushLocal( 9 );
lab00018: ;
	hb_xvmPopLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmArrayGen( 0 );
	goto lab00020;
lab00019: ;
	hb_xvmPushLocal( 10 );
lab00020: ;
	hb_xvmPopLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmArrayGen( 0 );
	goto lab00022;
lab00021: ;
	hb_xvmPushLocal( 11 );
lab00022: ;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 119 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 120 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 121 );
	hb_xvmPushSymbol( symbols + 100 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 122 );
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 123 );
	hb_xvmPushSymbol( symbols + 102 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 124 );
	hb_xvmPushSymbol( symbols + 103 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 125 );
	hb_xvmPushSymbol( symbols + 104 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 126 );
	hb_xvmPushSymbol( symbols + 105 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 7 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 127 );
	hb_xvmPushSymbol( symbols + 106 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 128 );
	hb_xvmPushSymbol( symbols + 107 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 129 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 11 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 131 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCDOMAIL_ACTIVATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 0 );
	hb_xvmSetLine( 163 );
	hb_xvmPushStringConst( "http://schemas.microsoft.com/cdo/configuration/", 47 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 164 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 167 );
	hb_xvmPushSymbol( symbols + 109 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 168 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 109 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSelf();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 169 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 172 );
	hb_xvmSeqBegin();
	for( ;; ) {
	hb_xvmPushFuncSymbol( symbols + 110 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSeqBlock() ) break;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 111 );
	hb_xvmPushStringConst( "CDO.Message", 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 177 );
	hb_xvmPushLocal( 1 );
	hb_xvmWithObjectStart();
	hb_xvmSetLine( 180 );
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 181 );
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 184 );
	hb_xvmCopyLocals( 4, 5 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 186 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( " <", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmLocalAdd( 5 );
lab00003: ;
	hb_xvmSetLine( 189 );
	hb_xvmWithObjectMessage( symbols + 114 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushSymbol( symbols + 116 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 194 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	goto lab00011;
lab00004: ;
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 113 );
	hb_xvmPushSymbol( symbols + 116 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 196 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushSymbol( symbols + 116 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushStringConst( "", 0 );
	goto lab00006;
lab00005: ;
	hb_xvmPushStringConst( ";", 1 );
lab00006: ;
	hb_xvmLocalAdd( 8 );
	goto lab00010;
lab00007: ;
	hb_xvmSetLine( 198 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushSymbol( symbols + 116 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " <", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushSymbol( symbols + 116 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ">", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushStringConst( "", 0 );
	goto lab00009;
lab00008: ;
	hb_xvmPushStringConst( ";", 1 );
lab00009: ;
	hb_xvmLocalAdd( 8 );
lab00010: ;
	hb_xvmSetLine( 194 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
lab00011: ;
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 202 );
	hb_xvmWithObjectMessage( symbols + 117 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00012: ;
	hb_xvmSetLine( 206 );
	hb_xvmWithObjectMessage( symbols + 118 );
	hb_xvmPushSymbol( symbols + 119 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 207 );
	hb_xvmWithObjectMessage( symbols + 120 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 208 );
	hb_xvmWithObjectMessage( symbols + 121 );
	hb_xvmPushSymbol( symbols + 122 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 211 );
	hb_xvmPushStringConst( "<", 1 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 212 );
	hb_xvmWithObjectMessage( symbols + 124 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00014;
lab00013: ;
	hb_xvmSetLine( 214 );
	hb_xvmWithObjectMessage( symbols + 125 );
	hb_xvmPushSymbol( symbols + 123 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00014: ;
	hb_xvmSetLine( 217 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmWithObjectMessage( symbols + 127 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "utf-8", 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 220 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushSymbol( symbols + 128 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00017;
	hb_xvmSetLine( 221 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	goto lab00016;
lab00015: ;
	hb_xvmSetLine( 222 );
	hb_xvmWithObjectMessage( symbols + 129 );
	hb_xvmPushSymbol( symbols + 128 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 221 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
lab00016: ;
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
lab00017: ;
	hb_xvmSetLine( 227 );
	hb_xvmPushSymbol( symbols + 130 );
	hb_xvmWithObjectMessage( symbols + 131 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmWithObjectStart();
	hb_xvmSetLine( 229 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "smtpserver", 10 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 134 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 230 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "smtpserverport", 14 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 135 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 231 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "sendusing", 9 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 232 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "smtpauthenticate", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 233 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "smtpusessl", 10 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 135 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualInt( 465L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 234 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "sendusername", 12 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 136 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 235 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "sendpassword", 12 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 137 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 236 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "smtpconnectiontimeout", 21 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 138 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 238 );
	hb_xvmWithObjectMessage( symbols + 139 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmWithObjectEnd();
	hb_xvmSetLine( 243 );
	hb_xvmPushSymbol( symbols + 130 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmWithObjectStart();
	hb_xvmSetLine( 245 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushStringConst( "urn:schemas:httpmail:importance", 31 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 140 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 246 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushStringConst( "urn:schemas:mailheader:X-Priority", 33 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 140 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 247 );
	hb_xvmPushSymbol( symbols + 141 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 248 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushStringConst( "urn:schemas:mailheader:return-receipt-to", 40 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 249 );
	hb_xvmPushSymbol( symbols + 132 );
	hb_xvmWithObjectMessage( symbols + 133 );
	hb_xvmPushStringConst( "urn:schemas:mailheader:disposition-notification-to", 50 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00018: ;
	hb_xvmSetLine( 252 );
	hb_xvmWithObjectMessage( symbols + 139 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmWithObjectEnd();
	hb_xvmSetLine( 256 );
	hb_xvmWithObjectMessage( symbols + 142 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 258 );
	hb_xvmWithObjectMessage( symbols + 143 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 260 );
	hb_xvmPushSymbol( symbols + 144 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmWithObjectEnd();
	hb_xvmSetLine( 262 );
	hb_stackPop();
	if( hb_xvmSeqEndTest() ) break;
	goto lab00019;
	}
	hb_xvmSetLine( 264 );
	if( hb_xvmSeqRecover() ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 145 );
	hb_xvmPushStringConst( "The email was not sent.", 23 );
	hb_xvmPushFuncSymbol( symbols + 146 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Error:      ", 12 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 147 );
	hb_xvmPushSymbol( symbols + 148 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 146 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "SubCode:   ", 11 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 147 );
	hb_xvmPushSymbol( symbols + 149 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 146 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "OSCode:    ", 11 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 147 );
	hb_xvmPushSymbol( symbols + 150 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 146 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "SubSystem: ", 11 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 147 );
	hb_xvmPushSymbol( symbols + 151 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 146 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Description:      ", 18 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushSymbol( symbols + 152 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 274 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 1 );
lab00019: ;
	hb_xvmSetLine( 278 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 153, 1 );
	/* *** END PROC *** */
   } while( 0 );
}

