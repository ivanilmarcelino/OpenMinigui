/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "t7zip.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( T7ZIP );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( HBOBJECT );
HB_FUNC_STATIC( T7ZIP_CREATE );
HB_FUNC_EXTERN( HB_SEVENZIPOPENARCHIVE );
HB_FUNC_EXTERN( HB_SEVENZIP );
HB_FUNC_EXTERN( HB_ANSITOOEM );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_STATIC( T7ZIP_ERRORDESCRIPTION );
HB_FUNC_EXTERN( HB_SEVENZIPCLOSEARCHIVE );
HB_FUNC_EXTERN( HB_SEVENZIPGETARCFILESIZE );
HB_FUNC_EXTERN( HB_SEVENZIPGETARCORIGINALSIZE );
HB_FUNC_EXTERN( HB_SEVENZIPGETARCCOMPRESSEDSIZE );
HB_FUNC_EXTERN( HB_SEVENZIPGETARCRATIO );
HB_FUNC_EXTERN( HB_SEVENZIPGETORIGINALSIZE );
HB_FUNC_EXTERN( HB_SEVENZIPGETCOMPRESSEDSIZE );
HB_FUNC_EXTERN( HB_SEVENZIPGETRATIO );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( LTRIM );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_STATIC( HB_7ZIPCONVERTFILENAME );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_INIT( _7ZINIT );
HB_FUNC_EXTERN( INIT7ZIPDLL );
HB_FUNC_EXIT( _7ZEXIT );
HB_FUNC_EXTERN( EXIT7ZIPDLL );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_T7ZIP )
{ "T7ZIP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( T7ZIP )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "HBOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBOBJECT )}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDINLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "T7ZIP_CREATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( T7ZIP_CREATE )}, NULL },
{ "_HANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_SEVENZIPOPENARCHIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_SEVENZIPOPENARCHIVE )}, NULL },
{ "HWNDOWNER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CARCNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_SEVENZIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_SEVENZIP )}, NULL },
{ "HB_ANSITOOEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ANSITOOEM )}, NULL },
{ "_CBUFFER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NBUFFER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "LALWAYSOVERWRITE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LSHOWPROCESSDLG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "T7ZIP_ERRORDESCRIPTION", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( T7ZIP_ERRORDESCRIPTION )}, NULL },
{ "HB_SEVENZIPCLOSEARCHIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_SEVENZIPCLOSEARCHIVE )}, NULL },
{ "HANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_SEVENZIPGETARCFILESIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_SEVENZIPGETARCFILESIZE )}, NULL },
{ "HB_SEVENZIPGETARCORIGINALSIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_SEVENZIPGETARCORIGINALSIZE )}, NULL },
{ "HB_SEVENZIPGETARCCOMPRESSEDSIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_SEVENZIPGETARCCOMPRESSEDSIZE )}, NULL },
{ "HB_SEVENZIPGETARCRATIO", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_SEVENZIPGETARCRATIO )}, NULL },
{ "HB_SEVENZIPGETORIGINALSIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_SEVENZIPGETORIGINALSIZE )}, NULL },
{ "HB_SEVENZIPGETCOMPRESSEDSIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_SEVENZIPGETCOMPRESSEDSIZE )}, NULL },
{ "HB_SEVENZIPGETRATIO", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_SEVENZIPGETRATIO )}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NARCTYPE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CARCTYPE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CCOMMAND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CARCTYPE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NZIPCOMPRESSIONLEVEL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( LTRIM )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "NCOMPRESSIONMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "_CCOMPRESSIONMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CCOMPRESSIONMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CPASSWORD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "LRECURSIVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LSOLID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LMULTICPU", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AEXCLUDEFILES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AVOLUMES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_7ZIPCONVERTFILENAME", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HB_7ZIPCONVERTFILENAME )}, NULL },
{ "LCONVERTANSITOOEM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AFILES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CCOMMAND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "_7ZINIT$", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( _7ZINIT )}, NULL },
{ "INIT7ZIPDLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( INIT7ZIPDLL )}, NULL },
{ "_7ZEXIT$", {HB_FS_EXIT | HB_FS_LOCAL}, {HB_EXIT_FUNCNAME( _7ZEXIT )}, NULL },
{ "EXIT7ZIPDLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( EXIT7ZIPDLL )}, NULL },
{ "(_INITSTATICS00004)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_T7ZIP, "t7zip.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_T7ZIP
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_T7ZIP )
   #include "hbiniseg.h"
#endif

HB_FUNC( T7ZIP )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 64 );
	hb_xvmSetLine( 56 );
	hb_xvmPushStatic( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStaticByRef( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSeqAlways();
	do {
	hb_xvmLocalSetInt( 1, 1L );
	hb_xvmPushSymbol( symbols + 2 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "T7ZIP", 5 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 0 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 58 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "INTEGER", 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hWndOwner", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 59 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "INTEGER", 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nError", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 60 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "INTEGER", 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "handle", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 61 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lShowProcessDlg", 15 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 62 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lAlwaysOverWrite", 16 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 63 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cArcName", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 64 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "STRING", 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cBuffer", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 65 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "INTEGER", 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nBuffer", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 66 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "STRING", 6 );
	hb_xvmPushStringConst( "PPMd", 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cCompressionMethod", 18 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 72 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "INTEGER", 7 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nCompressionMethod", 18 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 73 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "INTEGER", 7 );
	hb_xvmPushInteger( 5 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nZipCompressionLevel", 20 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 75 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "STRING", 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cCommand", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 76 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "INTEGER", 7 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nArctype", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 77 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "STRING", 6 );
	hb_xvmPushStringConst( "7z", 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cArcType", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 79 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aFiles", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 82 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "STRING", 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cPassword", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 83 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lRecursive", 10 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 84 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aExcludeFiles", 13 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 86 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aVolumes", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 87 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lSolid", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 92 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lMultiCPU", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 94 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lConvertANSIToOEM", 17 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 97 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	{
		static const HB_BYTE codeblock[ 7 ] = {
			1, 0, 0, 0, 95, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 98 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Create", 6 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 101 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Open", 4 );
	{
		static const HB_BYTE codeblock[ 32 ] = {
			1, 0, 0, 0, 48, 9, 0, 95, 1, 176, 10, 0, 48, 11, 0, 95, 
			1, 112, 0, 48, 12, 0, 95, 1, 112, 0, 121, 12, 3, 112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 104 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "List", 4 );
	{
		static const HB_BYTE codeblock[ 61 ] = {
			1, 0, 0, 0, 48, 13, 0, 95, 1, 176, 14, 0, 48, 11, 0, 95, 
			1, 112, 0, 106, 4, 108, 32, 34, 0, 176, 15, 0, 48, 12, 0, 95, 
			1, 112, 0, 12, 1, 72, 106, 2, 34, 0, 72, 48, 16, 0, 95, 1, 
			147, 48, 17, 0, 95, 1, 112, 0, 12, 4, 112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 107 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Test", 4 );
	{
		static const HB_BYTE codeblock[ 61 ] = {
			1, 0, 0, 0, 48, 13, 0, 95, 1, 176, 14, 0, 48, 11, 0, 95, 
			1, 112, 0, 106, 4, 116, 32, 34, 0, 176, 15, 0, 48, 12, 0, 95, 
			1, 112, 0, 12, 1, 72, 106, 2, 34, 0, 72, 48, 16, 0, 95, 1, 
			147, 48, 17, 0, 95, 1, 112, 0, 12, 4, 112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 110 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Extract", 7 );
	{
		static const HB_BYTE codeblock[ 135 ] = {
			2, 0, 0, 0, 48, 13, 0, 95, 1, 176, 14, 0, 48, 11, 0, 95, 
			1, 112, 0, 176, 18, 0, 95, 2, 12, 1, 106, 2, 76, 0, 8, 28, 
			13, 95, 2, 28, 9, 106, 3, 120, 32, 0, 25, 7, 106, 3, 101, 32, 
			0, 48, 19, 0, 95, 1, 112, 0, 28, 10, 106, 4, 45, 121, 32, 0, 
			25, 5, 106, 1, 0, 72, 48, 20, 0, 95, 1, 112, 0, 28, 13, 106, 
			7, 45, 104, 105, 100, 101, 32, 0, 25, 5, 106, 1, 0, 72, 106, 2, 
			34, 0, 72, 176, 15, 0, 48, 12, 0, 95, 1, 112, 0, 12, 1, 72, 
			106, 2, 34, 0, 72, 48, 16, 0, 95, 1, 147, 48, 17, 0, 95, 1, 
			112, 0, 12, 4, 112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 112 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ErrorDescription", 16 );
	hb_xvmPushSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 114 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Close", 5 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 176, 22, 0, 48, 23, 0, 95, 1, 112, 0, 12, 1, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 115 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetArcFileSize", 14 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 176, 24, 0, 48, 23, 0, 95, 1, 112, 0, 12, 1, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 116 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetArcOriginalSize", 18 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 176, 25, 0, 48, 23, 0, 95, 1, 112, 0, 12, 1, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 117 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetArcCompressedSize", 20 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 176, 26, 0, 48, 23, 0, 95, 1, 112, 0, 12, 1, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 118 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetArcRatio", 11 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 176, 27, 0, 48, 23, 0, 95, 1, 112, 0, 12, 1, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 119 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetOriginaLSize", 15 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 176, 28, 0, 48, 23, 0, 95, 1, 112, 0, 12, 1, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 120 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetCompressedSize", 17 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 176, 29, 0, 48, 23, 0, 95, 1, 112, 0, 12, 1, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 121 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetRatio", 8 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			1, 0, 0, 0, 176, 30, 0, 48, 23, 0, 95, 1, 112, 0, 12, 1, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 123 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushStaticByRef( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 35 );
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
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushStatic( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( T7ZIP_CREATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSFrame( symbols + 64 );
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00023;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmLessEqualThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00023;
	hb_xvmSetLine( 133 );
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushSelf();
	hb_xvmPushStatic( 2 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 134 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "a", 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 136 );
	hb_xvmPushSymbol( symbols + 20 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 137 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -hide", 6 );
	if( hb_xvmPlusEqPop() ) break;
lab00001: ;
	hb_xvmSetLine( 140 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -t", 3 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 142 );
	goto lab00005;
lab00002: ;
	hb_xvmSetLine( 145 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterEqualThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmLessEqualThenIntIs( 9L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 146 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -mx", 4 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00003: ;
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushStatic( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 156 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushSelf();
	hb_xvmPushStatic( 3 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 157 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -m0=", 5 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00006;
lab00005: ;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00002;
		}
		{
			hb_stackPop();
			goto lab00004;
		}
	}
lab00006: ;
	hb_xvmSetLine( 161 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 162 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -p", 3 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00007: ;
	hb_xvmSetLine( 165 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 166 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -r", 3 );
	if( hb_xvmPlusEqPop() ) break;
lab00008: ;
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 170 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -ms=off", 8 );
	if( hb_xvmPlusEqPop() ) break;
lab00009: ;
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 174 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -mmt", 5 );
	if( hb_xvmPlusEqPop() ) break;
lab00010: ;
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 178 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 1 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
lab00011: ;
	hb_xvmSetLine( 179 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -x!", 4 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 180 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
lab00012: ;
	hb_xvmEnumEnd();
	goto lab00014;
lab00013: ;
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 182 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -x!", 4 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00014: ;
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 186 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 1 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
lab00015: ;
	hb_xvmSetLine( 187 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -v", 3 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "b", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 188 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
lab00016: ;
	hb_xvmEnumEnd();
	goto lab00018;
lab00017: ;
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 190 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " -v", 3 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "b", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00018: ;
	hb_xvmSetLine( 193 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 196 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 1 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
lab00019: ;
	hb_xvmSetLine( 197 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 198 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
lab00020: ;
	hb_xvmEnumEnd();
	goto lab00022;
lab00021: ;
	hb_xvmSetLine( 199 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 200 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00022: ;
	hb_xvmSetLine( 203 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushSymbol( symbols + 11 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00023: ;
	hb_xvmSetLine( 206 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32803 );
#else
	hb_xvmPushLong( 32803L );
#endif
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( T7ZIP_ERRORDESCRIPTION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSFrame( symbols + 64 );
	hb_xvmSetLine( 209 );
	hb_xvmPushSelf();
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 213 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 214 );
	hb_xvmPushStringConst( "ERROR_OK", 8 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushStatic( 1 );
	{
		static const HB_BYTE codeblock[ 20 ] = {
			1, 0, 1, 0, 1, 0, 95, 1, 92, 2, 1, 48, 58, 0, 95, 255, 
			112, 0, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 216 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 219 );
	hb_xvmPushStringConst( "ERROR_UNKNOWN", 13 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HB_7ZIPCONVERTFILENAME )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 224 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 225 );
	hb_xvmPushStringConst( "\"", 1 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\"", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 228 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INIT( _7ZINIT )
{
   do {
	hb_xvmSetLine( 233 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 234 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_EXIT( _7ZEXIT )
{
   do {
	hb_xvmSetLine( 239 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 240 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 64, 4 );
	hb_xvmSFrame( symbols + 64 );
	hb_xvmPushStringConst( "ERROR_DISK_SPACE", 16 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32773 );
#else
	hb_xvmPushLong( 32773L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_READ_ONLY", 15 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32774 );
#else
	hb_xvmPushLong( 32774L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_USER_SKIP", 15 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32775 );
#else
	hb_xvmPushLong( 32775L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_UNKNOWN_TYPE", 18 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32776 );
#else
	hb_xvmPushLong( 32776L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_METHOD", 12 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32777 );
#else
	hb_xvmPushLong( 32777L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_PASSWORD_FILE", 19 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32778 );
#else
	hb_xvmPushLong( 32778L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_VERSION", 13 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32779 );
#else
	hb_xvmPushLong( 32779L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_FILE_CRC", 14 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32780 );
#else
	hb_xvmPushLong( 32780L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_FILE_OPEN", 15 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32781 );
#else
	hb_xvmPushLong( 32781L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_MORE_FRESH", 16 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32782 );
#else
	hb_xvmPushLong( 32782L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_NOT_EXIST", 15 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32783 );
#else
	hb_xvmPushLong( 32783L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_ALREADY_EXIST", 19 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32784 );
#else
	hb_xvmPushLong( 32784L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_TOO_MANY_FILES", 20 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32785 );
#else
	hb_xvmPushLong( 32785L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_MAKEDIRECTORY", 19 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32786 );
#else
	hb_xvmPushLong( 32786L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_CANNOT_WRITE", 18 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32787 );
#else
	hb_xvmPushLong( 32787L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_HUFFMAN_CODE", 18 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32788 );
#else
	hb_xvmPushLong( 32788L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_COMMENT_HEADER", 20 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32789 );
#else
	hb_xvmPushLong( 32789L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_HEADER_CRC", 16 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32790 );
#else
	hb_xvmPushLong( 32790L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_HEADER_BROKEN", 19 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32791 );
#else
	hb_xvmPushLong( 32791L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_ARC_FILE_OPEN", 19 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32792 );
#else
	hb_xvmPushLong( 32792L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_NOT_ARC_FILE", 18 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32793 );
#else
	hb_xvmPushLong( 32793L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_CANNOT_READ", 17 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32794 );
#else
	hb_xvmPushLong( 32794L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_FILE_STYLE", 16 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32795 );
#else
	hb_xvmPushLong( 32795L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_COMMAND_NAME", 18 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32796 );
#else
	hb_xvmPushLong( 32796L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_MORE_HEAP_MEMORY", 22 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32797 );
#else
	hb_xvmPushLong( 32797L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_ENOUGH_MEMORY", 19 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32798 );
#else
	hb_xvmPushLong( 32798L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_ALREADY_RUNNING", 21 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32799 );
#else
	hb_xvmPushLong( 32799L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_USER_CANCEL", 17 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32800 );
#else
	hb_xvmPushLong( 32800L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_HARC_ISNOT_OPENED", 23 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32801 );
#else
	hb_xvmPushLong( 32801L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_NOT_SEARCH_MODE", 21 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32802 );
#else
	hb_xvmPushLong( 32802L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_NOT_SUPPORT", 17 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32803 );
#else
	hb_xvmPushLong( 32803L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_TIME_STAMP", 16 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32804 );
#else
	hb_xvmPushLong( 32804L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_TMP_OPEN", 14 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32805 );
#else
	hb_xvmPushLong( 32805L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_LONG_FILE_NAME", 20 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32806 );
#else
	hb_xvmPushLong( 32806L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_ARC_READ_ONLY", 19 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32807 );
#else
	hb_xvmPushLong( 32807L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_SAME_NAME_FILE", 20 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32808 );
#else
	hb_xvmPushLong( 32808L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_NOT_FIND_ARC_FILE", 23 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32809 );
#else
	hb_xvmPushLong( 32809L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_RESPONSE_READ", 19 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32810 );
#else
	hb_xvmPushLong( 32810L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_NOT_FILENAME", 18 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32811 );
#else
	hb_xvmPushLong( 32811L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_TMP_COPY", 14 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32812 );
#else
	hb_xvmPushLong( 32812L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_EOF", 9 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32813 );
#else
	hb_xvmPushLong( 32813L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_ADD_TO_LARC", 17 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32814 );
#else
	hb_xvmPushLong( 32814L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_TMP_BACK_SPACE", 20 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32815 );
#else
	hb_xvmPushLong( 32815L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_SHARING", 13 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32816 );
#else
	hb_xvmPushLong( 32816L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_NOT_FIND_FILE", 19 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32817 );
#else
	hb_xvmPushLong( 32817L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_LOG_FILE", 14 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32818 );
#else
	hb_xvmPushLong( 32818L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_NO_DEVICE", 15 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32819 );
#else
	hb_xvmPushLong( 32819L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_GET_ATTRIBUTES", 20 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32820 );
#else
	hb_xvmPushLong( 32820L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_SET_ATTRIBUTES", 20 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32821 );
#else
	hb_xvmPushLong( 32821L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_GET_INFORMATION", 21 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32822 );
#else
	hb_xvmPushLong( 32822L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_GET_POINT", 15 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32823 );
#else
	hb_xvmPushLong( 32823L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_SET_POINT", 15 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32824 );
#else
	hb_xvmPushLong( 32824L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_CONVERT_TIME", 18 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32825 );
#else
	hb_xvmPushLong( 32825L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_GET_TIME", 14 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32826 );
#else
	hb_xvmPushLong( 32826L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_SET_TIME", 14 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32827 );
#else
	hb_xvmPushLong( 32827L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_CLOSE_FILE", 16 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32828 );
#else
	hb_xvmPushLong( 32828L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_HEAP_MEMORY", 17 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32829 );
#else
	hb_xvmPushLong( 32829L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_HANDLE", 12 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32830 );
#else
	hb_xvmPushLong( 32830L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_TIME_STAMP_RANGE", 22 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32831 );
#else
	hb_xvmPushLong( 32831L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_MAKE_ARCHIVE", 18 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32832 );
#else
	hb_xvmPushLong( 32832L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_NOT_CONFIRM_NAME", 22 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32833 );
#else
	hb_xvmPushLong( 32833L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_UNEXPECTED_EOF", 20 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32834 );
#else
	hb_xvmPushLong( 32834L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_INVALID_END_MARK", 22 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32835 );
#else
	hb_xvmPushLong( 32835L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_INVOLVED_LZH", 18 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32836 );
#else
	hb_xvmPushLong( 32836L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_NO_END_MARK", 17 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32837 );
#else
	hb_xvmPushLong( 32837L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_HDR_INVALID_SIZE", 22 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32838 );
#else
	hb_xvmPushLong( 32838L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_UNKNOWN_LEVEL", 19 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32839 );
#else
	hb_xvmPushLong( 32839L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_BROKEN_DATA", 17 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32840 );
#else
	hb_xvmPushLong( 32840L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_7ZIP_START", 16 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 33024 );
#else
	hb_xvmPushLong( 33024L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_WARNING", 13 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 33025 );
#else
	hb_xvmPushLong( 33025L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_FATAL", 11 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 33026 );
#else
	hb_xvmPushLong( 33026L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_DURING_DECOMPRESSION", 26 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 33027 );
#else
	hb_xvmPushLong( 33027L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_DIR_FILE_WITH_64BIT_SIZE", 30 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 33028 );
#else
	hb_xvmPushLong( 33028L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "ERROR_FILE_CHANGED_DURING_OPERATION", 35 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 33029 );
#else
	hb_xvmPushLong( 33029L );
#endif
	hb_xvmArrayGen( 2 );
	hb_xvmArrayGen( 74 );
	hb_xvmPopStatic( 1 );
	hb_xvmPushStringConst( "7z", 2 );
	hb_xvmPushStringConst( "zip", 3 );
	hb_xvmPushStringConst( "gzip", 4 );
	hb_xvmPushStringConst( "bzip2", 5 );
	hb_xvmPushStringConst( "tar", 3 );
	hb_xvmPushStringConst( "iso", 3 );
	hb_xvmPushStringConst( "udf", 3 );
	hb_xvmArrayGen( 7 );
	hb_xvmPopStatic( 2 );
	hb_xvmPushStringConst( "LZMA", 4 );
	hb_xvmPushStringConst( "LZMA2", 5 );
	hb_xvmPushStringConst( "PPMd", 4 );
	hb_xvmPushStringConst( "BZip2", 5 );
	hb_xvmPushStringConst( "Deflate", 7 );
	hb_xvmPushStringConst( "Copy", 4 );
	hb_xvmPushStringConst( "Deflate64", 9 );
	hb_xvmArrayGen( 7 );
	hb_xvmPopStatic( 3 );
	/* *** END PROC *** */
   } while( 0 );
}

