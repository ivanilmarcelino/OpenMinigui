/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_savepdf.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _CREATEPDF );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( HB_FNAMEEXTSET );
HB_FUNC_EXTERN( HB_FILEEXISTS );
HB_FUNC_EXTERN( MSGYESNO );
HB_FUNC_EXTERN( CLEANPROGRAMMEMORY );
HB_FUNC_EXTERN( ISVISTAORLATER );
HB_FUNC_EXTERN( EMPTYWORKINGSET );
HB_FUNC_EXTERN( __BREAKBLOCK );
HB_FUNC_EXTERN( HPDF_NEW );
HB_FUNC_STATIC( UPDF_ERROR );
HB_FUNC_EXTERN( BREAK );
HB_FUNC_EXTERN( HB_CDPSELECT );
HB_FUNC_EXTERN( HB_USERNAME );
HB_FUNC_EXTERN( WAITWINDOW );
HB_FUNC_EXTERN( HPDF_SETCOMPRESSIONMODE );
HB_FUNC_EXTERN( HPDF_USEUTFENCODINGS );
HB_FUNC_EXTERN( HPDF_SETCURRENTENCODER );
HB_FUNC_STATIC( PDFSETINFO );
HB_FUNC_EXTERN( MIN );
HB_FUNC_EXTERN( BT_BITMAPLOADEMF );
HB_FUNC_EXTERN( BT_BITMAPSAVEFILE );
HB_FUNC_EXTERN( BMPSIZE );
HB_FUNC_EXTERN( BT_BITMAPRELEASE );
HB_FUNC_EXTERN( HPDF_ADDPAGE );
HB_FUNC_EXTERN( HPDF_PAGE_SETSIZE );
HB_FUNC_EXTERN( HPDF_PAGE_GETHEIGHT );
HB_FUNC_EXTERN( HPDF_PAGE_GETWIDTH );
HB_FUNC_STATIC( PUTPAGEIMAGE );
HB_FUNC_EXTERN( FERASE );
HB_FUNC_EXTERN( HPDF_SAVETOFILE );
HB_FUNC_EXTERN( HPDF_RESETERROR );
HB_FUNC_EXTERN( HPDF_FREE );
HB_FUNC_EXTERN( WAPI_SHELLEXECUTE );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( HB_FNAMEEXT );
HB_FUNC_EXTERN( HPDF_LOADPNGIMAGEFROMFILE );
HB_FUNC_EXTERN( HPDF_LOADJPEGIMAGEFROMFILE );
HB_FUNC_EXTERN( HPDF_PAGE_DRAWIMAGE );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( TIME );
HB_FUNC_EXTERN( HPDF_SETINFOATTR );
HB_FUNC_EXTERN( HPDF_SETINFODATEATTR );
HB_FUNC_EXTERN( YEAR );
HB_FUNC_EXTERN( MONTH );
HB_FUNC_EXTERN( DAY );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( HPDF_GETERROR );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( HB_CODEPAGE_UTF8 );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_SAVEPDF )
{ "_CREATEPDF", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _CREATEPDF )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "HB_FNAMEEXTSET", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FNAMEEXTSET )}, NULL },
{ "HB_FILEEXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FILEEXISTS )}, NULL },
{ "MSGYESNO", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGYESNO )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLEANPROGRAMMEMORY", {HB_FS_PUBLIC}, {HB_FUNCNAME( CLEANPROGRAMMEMORY )}, NULL },
{ "ISVISTAORLATER", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISVISTAORLATER )}, NULL },
{ "EMPTYWORKINGSET", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTYWORKINGSET )}, NULL },
{ "__BREAKBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __BREAKBLOCK )}, NULL },
{ "HPDF_NEW", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_NEW )}, NULL },
{ "UPDF_ERROR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( UPDF_ERROR )}, NULL },
{ "BREAK", {HB_FS_PUBLIC}, {HB_FUNCNAME( BREAK )}, NULL },
{ "HB_CDPSELECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_CDPSELECT )}, NULL },
{ "HB_USERNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_USERNAME )}, NULL },
{ "WAITWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( WAITWINDOW )}, NULL },
{ "HPDF_SETCOMPRESSIONMODE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_SETCOMPRESSIONMODE )}, NULL },
{ "HPDF_USEUTFENCODINGS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_USEUTFENCODINGS )}, NULL },
{ "HPDF_SETCURRENTENCODER", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_SETCURRENTENCODER )}, NULL },
{ "PDFSETINFO", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( PDFSETINFO )}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "BT_BITMAPLOADEMF", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BITMAPLOADEMF )}, NULL },
{ "BT_BITMAPSAVEFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BITMAPSAVEFILE )}, NULL },
{ "BMPSIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BMPSIZE )}, NULL },
{ "BT_BITMAPRELEASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BITMAPRELEASE )}, NULL },
{ "HPDF_ADDPAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_ADDPAGE )}, NULL },
{ "HPDF_PAGE_SETSIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_PAGE_SETSIZE )}, NULL },
{ "HPDF_PAGE_GETHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_PAGE_GETHEIGHT )}, NULL },
{ "HPDF_PAGE_GETWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_PAGE_GETWIDTH )}, NULL },
{ "PUTPAGEIMAGE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( PUTPAGEIMAGE )}, NULL },
{ "FERASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERASE )}, NULL },
{ "HPDF_SAVETOFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_SAVETOFILE )}, NULL },
{ "HPDF_RESETERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_RESETERROR )}, NULL },
{ "HPDF_FREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_FREE )}, NULL },
{ "WAPI_SHELLEXECUTE", {HB_FS_PUBLIC}, {HB_FUNCNAME( WAPI_SHELLEXECUTE )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "HB_FNAMEEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FNAMEEXT )}, NULL },
{ "HPDF_LOADPNGIMAGEFROMFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_LOADPNGIMAGEFROMFILE )}, NULL },
{ "HPDF_LOADJPEGIMAGEFROMFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_LOADJPEGIMAGEFROMFILE )}, NULL },
{ "HPDF_PAGE_DRAWIMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_PAGE_DRAWIMAGE )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "TIME", {HB_FS_PUBLIC}, {HB_FUNCNAME( TIME )}, NULL },
{ "HPDF_SETINFOATTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_SETINFOATTR )}, NULL },
{ "HPDF_SETINFODATEATTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_SETINFODATEATTR )}, NULL },
{ "YEAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( YEAR )}, NULL },
{ "MONTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( MONTH )}, NULL },
{ "DAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( DAY )}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "HPDF_GETERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HPDF_GETERROR )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "HB_CODEPAGE_UTF8", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_CODEPAGE_UTF8 )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_SAVEPDF, "h_savepdf.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_SAVEPDF
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_SAVEPDF )
   #include "hbiniseg.h"
#endif

HB_FUNC( _CREATEPDF )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 18, 4 );
	hb_xvmSetLine( 56 );
	hb_xvmPushStringConst( "Simple PDF Creator", 18 );
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 57 );
	hb_xvmLocalSetInt( 17, 3L );
	hb_xvmSetLine( 58 );
	hb_xvmLocalSetInt( 18, 0L );
	hb_xvmSetLine( 60 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 61 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 65 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 66 );
	hb_xvmPushStringConst( "- Source folder", 15 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 21 );
lab00001: ;
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 70 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushStringConst( "- Number of total pages", 23 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00002: ;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 74 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushStringConst( "- Output file", 13 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00003: ;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushStringConst( "cannot be empty!", 16 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Warning", 7 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushStringConst( "Untitled", 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "pdf", 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "File ", 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " already exists!", 16 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Overwrite\?", 10 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Warning!", 8 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 5 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 89 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 93 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 271L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00007;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmDo( 0 ) ) break;
lab00007: ;
	hb_xvmSetLine( 96 );
	hb_xvmSeqAlways();
	do {
	hb_xvmSeqBegin();
	for( ;; ) {
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSeqBlock() ) break;
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "CREATE", 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmDo( 0 ) ) break;
lab00008: ;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushStringConst( "UTF8", 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 103 );
	hb_xvmPushStringConst( "UTF-8", 5 );
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushStringConst( "Creating PDF file", 17 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 110 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "COMPRESS", 8 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmDo( 0 ) ) break;
lab00009: ;
	hb_xvmSetLine( 114 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 118 );
	hb_xvmLocalSetInt( 22, 0L );
lab00010: ;
	hb_xvmSetLine( 121 );
	if( hb_xvmLocalIncPush( 22 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushInteger( 20 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 122 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 124 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( "png", 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 125 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmAddInt( -850L ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 130 );
	hb_xvmLocalSetInt( 18, 1L );
lab00011: ;
	hb_xvmSetLine( 133 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 139 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 142 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmSetLine( 148 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "There was an error with image file:", 35 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "!", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Warning", 7 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 149 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 20 );
lab00012: ;
	hb_xvmSetLine( 154 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "SAVE", 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmDo( 0 ) ) break;
lab00013: ;
	hb_xvmSetLine( 159 );
	hb_stackPop();
	if( hb_xvmSeqEndTest() ) break;
	goto lab00014;
	}
	hb_xvmSetLine( 161 );
	if( hb_xvmSeqRecover() ) break;
	hb_stackPop();
	hb_xvmSetLine( 163 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 20 );
lab00014: ;
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmSetLine( 165 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 166 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 168 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 169 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 271L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00016;
lab00015: ;
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmDo( 0 ) ) break;
lab00016: ;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmSetLine( 172 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "View ", 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " (Y/N) \?", 8 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Please select", 13 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 5 ) ) break;
	goto lab00018;
lab00017: ;
	hb_xvmPushLocal( 3 );
lab00018: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 174 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "open", 4 );
	hb_xvmPushStringConst( "\"", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\"", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00019: ;
	hb_xvmSetLine( 178 );
	hb_xvmPushLocal( 20 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( PUTPAGEIMAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 5 );
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ".PNG", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
lab00002: ;
	hb_xvmSetLine( 211 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 20 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmAddInt( -30L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmAddInt( -20L ) ) break;
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 213 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmEqualInt( 0L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( PDFSETINFO )
{
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 237 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 238 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 240 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 247 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 7 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 6 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 249 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( UPDF_ERROR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 271 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 274 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( "CREATE", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 277 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "CREATE", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 278 );
	hb_xvmPushStringConst( "PDF file creation operation failed!", 35 );
	hb_xvmPopLocal( 4 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 279 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "COMPRESS", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 280 );
	hb_xvmPushStringConst( "PDF file compress operation failed!", 35 );
	hb_xvmPopLocal( 4 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 281 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SAVE", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 282 );
	hb_xvmPushStringConst( "PDF file save operation failed!", 31 );
	hb_xvmPopLocal( 4 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 284 );
	hb_xvmPushStringConst( "Error(s) occurred!", 18 );
	hb_xvmPopLocal( 4 );
lab00004: ;
	hb_xvmSetLine( 287 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "Error Code: ", 12 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " (HPDF)", 7 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 288 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

