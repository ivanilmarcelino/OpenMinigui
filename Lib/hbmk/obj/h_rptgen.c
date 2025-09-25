/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_rptgen.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEREPORT );
HB_FUNC_EXTERN( __MVPUBLIC );
HB_FUNC( _ENDREPORT );
HB_FUNC_EXTERN( AADD );
HB_FUNC( _BEGINLAYOUT );
HB_FUNC( _ENDLAYOUT );
HB_FUNC( _BEGINHEADER );
HB_FUNC( _ENDHEADER );
HB_FUNC( _BEGINDETAIL );
HB_FUNC( _ENDDETAIL );
HB_FUNC( _BEGINFOOTER );
HB_FUNC( _ENDFOOTER );
HB_FUNC( _BEGINSUMMARY );
HB_FUNC( _ENDSUMMARY );
HB_FUNC( _BEGINTEXT );
HB_FUNC( _ENDTEXT );
HB_FUNC( _BANDHEIGHT );
HB_FUNC( EXECUTEREPORT );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( RIGHT );
HB_FUNC_EXTERN( PDFINIT );
HB_FUNC_EXTERN( PDFOPEN );
HB_FUNC_EXTERN( RECNO );
HB_FUNC_EXTERN( __MVGET );
HB_FUNC_EXTERN( GETPRINTER );
HB_FUNC_EXTERN( GETDEFAULTPRINTER );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( _HMG_PRINTER_INITUSERMESSAGES );
HB_FUNC_EXTERN( _HMG_PRINTER_SETPRINTERPROPERTIES );
HB_FUNC_EXTERN( STRZERO );
HB_FUNC_EXTERN( SECONDS );
HB_FUNC_EXTERN( _HMG_PRINTER_SETJOBNAME );
HB_FUNC_EXTERN( _HMG_PRINTER_STARTDOC );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( PDFNEWPAGE );
HB_FUNC_EXTERN( _HMG_PRINTER_STARTPAGE_PREVIEW );
HB_FUNC_EXTERN( GETTEMPFOLDER );
HB_FUNC_EXTERN( _HMG_PRINTER_STARTPAGE );
HB_FUNC_STATIC( _PROCESSBAND );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( _HMG_PRINTER_ENDPAGE_PREVIEW );
HB_FUNC_EXTERN( _HMG_PRINTER_ENDPAGE );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( _HMG_PRINTER_SHOWPREVIEW );
HB_FUNC_EXTERN( _HMG_PRINTER_ENDDOC );
HB_FUNC_EXTERN( PDFCLOSE );
HB_FUNC_EXTERN( FCREATE );
HB_FUNC_EXTERN( FWRITE );
HB_FUNC_EXTERN( FCLOSE );
HB_FUNC_EXTERN( DBGOTO );
HB_FUNC_EXTERN( __MVEXIST );
HB_FUNC_EXTERN( __MVXRELEASE );
HB_FUNC_STATIC( _PRINTOBJECT );
HB_FUNC_STATIC( _PRINTTEXT );
HB_FUNC_STATIC( _PRINTIMAGE );
HB_FUNC_STATIC( _PRINTLINE );
HB_FUNC_STATIC( _PRINTRECTANGLE );
HB_FUNC_EXTERN( _HMG_PRINTER_H_MULTILINE_PRINT );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( PDFSETFONT );
HB_FUNC_EXTERN( PDFATSAY );
HB_FUNC_EXTERN( PDFTEXTWIDTH );
HB_FUNC_EXTERN( CHR );
HB_FUNC_EXTERN( _HMG_PRINTER_H_IMAGE );
HB_FUNC_EXTERN( PDFIMAGE );
HB_FUNC_EXTERN( _HMG_PRINTER_H_LINE );
HB_FUNC_EXTERN( PDFBOX );
HB_FUNC_EXTERN( _HMG_PRINTER_H_RECTANGLE );
HB_FUNC( _BEGINLINE );
HB_FUNC( _ENDLINE );
HB_FUNC( _BEGINIMAGE );
HB_FUNC( _ENDIMAGE );
HB_FUNC( _BEGINRECTANGLE );
HB_FUNC( _ENDRECTANGLE );
HB_FUNC( _BEGINGROUP );
HB_FUNC( _ENDGROUP );
HB_FUNC( _BEGINGROUPHEADER );
HB_FUNC( _ENDGROUPHEADER );
HB_FUNC( _BEGINGROUPFOOTER );
HB_FUNC( _ENDGROUPFOOTER );
HB_FUNC( _DBSUM );
HB_FUNC_EXTERN( TYPE );
HB_FUNC_EXTERN( DBEVAL );
HB_FUNC( _BEGINDATA );
HB_FUNC( _ENDDATA );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_RPTGEN )
{ "_DEFINEREPORT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEREPORT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPUBLIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPUBLIC )}, NULL },
{ "_ENDREPORT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDREPORT )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "_BEGINLAYOUT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINLAYOUT )}, NULL },
{ "_ENDLAYOUT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDLAYOUT )}, NULL },
{ "_BEGINHEADER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINHEADER )}, NULL },
{ "_ENDHEADER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDHEADER )}, NULL },
{ "_BEGINDETAIL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINDETAIL )}, NULL },
{ "_ENDDETAIL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDDETAIL )}, NULL },
{ "_BEGINFOOTER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINFOOTER )}, NULL },
{ "_ENDFOOTER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDFOOTER )}, NULL },
{ "_BEGINSUMMARY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINSUMMARY )}, NULL },
{ "_ENDSUMMARY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDSUMMARY )}, NULL },
{ "_BEGINTEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINTEXT )}, NULL },
{ "_ENDTEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDTEXT )}, NULL },
{ "_BANDHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BANDHEIGHT )}, NULL },
{ "EXECUTEREPORT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( EXECUTEREPORT )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "RIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RIGHT )}, NULL },
{ "PDFINIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDFINIT )}, NULL },
{ "PDFOPEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDFOPEN )}, NULL },
{ "RECNO", {HB_FS_PUBLIC}, {HB_FUNCNAME( RECNO )}, NULL },
{ "__MVGET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVGET )}, NULL },
{ "GETPRINTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPRINTER )}, NULL },
{ "GETDEFAULTPRINTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDEFAULTPRINTER )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "_HMG_PRINTER_INITUSERMESSAGES", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_INITUSERMESSAGES )}, NULL },
{ "_HMG_PRINTER_SETPRINTERPROPERTIES", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_SETPRINTERPROPERTIES )}, NULL },
{ "_HMG_MINIPRINT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STRZERO", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRZERO )}, NULL },
{ "SECONDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SECONDS )}, NULL },
{ "_HMG_PRINTER_SETJOBNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_SETJOBNAME )}, NULL },
{ "_HMG_PRINTER_STARTDOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_STARTDOC )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PDFNEWPAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDFNEWPAGE )}, NULL },
{ "_HMG_PRINTER_STARTPAGE_PREVIEW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_STARTPAGE_PREVIEW )}, NULL },
{ "GETTEMPFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEMPFOLDER )}, NULL },
{ "_HMG_PRINTER_STARTPAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_STARTPAGE )}, NULL },
{ "_PROCESSBAND", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _PROCESSBAND )}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "_HMG_PRINTER_ENDPAGE_PREVIEW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_ENDPAGE_PREVIEW )}, NULL },
{ "_HMG_PRINTER_ENDPAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_ENDPAGE )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "_HMG_PRINTER_SHOWPREVIEW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_SHOWPREVIEW )}, NULL },
{ "_HMG_PRINTER_ENDDOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_ENDDOC )}, NULL },
{ "PDFCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDFCLOSE )}, NULL },
{ "FCREATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCREATE )}, NULL },
{ "FWRITE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FWRITE )}, NULL },
{ "FCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCLOSE )}, NULL },
{ "DBGOTO", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTO )}, NULL },
{ "__MVEXIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVEXIST )}, NULL },
{ "__MVXRELEASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVXRELEASE )}, NULL },
{ "_PRINTOBJECT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _PRINTOBJECT )}, NULL },
{ "_PRINTTEXT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _PRINTTEXT )}, NULL },
{ "_PRINTIMAGE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _PRINTIMAGE )}, NULL },
{ "_PRINTLINE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _PRINTLINE )}, NULL },
{ "_PRINTRECTANGLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _PRINTRECTANGLE )}, NULL },
{ "_HMG_PRINTER_H_MULTILINE_PRINT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_H_MULTILINE_PRINT )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "PDFSETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDFSETFONT )}, NULL },
{ "PDFATSAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDFATSAY )}, NULL },
{ "PDFTEXTWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDFTEXTWIDTH )}, NULL },
{ "CHR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHR )}, NULL },
{ "_HMG_PRINTER_H_IMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_H_IMAGE )}, NULL },
{ "PDFIMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDFIMAGE )}, NULL },
{ "_HMG_PRINTER_H_LINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_H_LINE )}, NULL },
{ "PDFBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDFBOX )}, NULL },
{ "_HMG_PRINTER_H_RECTANGLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_H_RECTANGLE )}, NULL },
{ "_BEGINLINE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINLINE )}, NULL },
{ "_ENDLINE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDLINE )}, NULL },
{ "_BEGINIMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINIMAGE )}, NULL },
{ "_ENDIMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDIMAGE )}, NULL },
{ "_BEGINRECTANGLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINRECTANGLE )}, NULL },
{ "_ENDRECTANGLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDRECTANGLE )}, NULL },
{ "_BEGINGROUP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINGROUP )}, NULL },
{ "_ENDGROUP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDGROUP )}, NULL },
{ "_BEGINGROUPHEADER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINGROUPHEADER )}, NULL },
{ "_ENDGROUPHEADER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDGROUPHEADER )}, NULL },
{ "_BEGINGROUPFOOTER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINGROUPFOOTER )}, NULL },
{ "_ENDGROUPFOOTER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDGROUPFOOTER )}, NULL },
{ "_DBSUM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DBSUM )}, NULL },
{ "TYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( TYPE )}, NULL },
{ "DBEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEVAL )}, NULL },
{ "_BEGINDATA", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINDATA )}, NULL },
{ "_ENDDATA", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDDATA )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_RPTGEN, "h_rptgen.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_RPTGEN
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_RPTGEN )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEREPORT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 83 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 118L ) ) break;
	hb_xvmSetLine( 84 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 119L ) ) break;
	hb_xvmSetLine( 86 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 120L ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 121L ) ) break;
	hb_xvmSetLine( 89 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 122L ) ) break;
	hb_xvmSetLine( 91 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 123L ) ) break;
	hb_xvmSetLine( 92 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 124L ) ) break;
	hb_xvmSetLine( 94 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 155L ) ) break;
	hb_xvmSetLine( 95 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 156L ) ) break;
	hb_xvmSetLine( 97 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 157L ) ) break;
	hb_xvmSetLine( 98 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 158L ) ) break;
	hb_xvmSetLine( 99 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 159L ) ) break;
	hb_xvmSetLine( 100 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 160L ) ) break;
	hb_xvmSetLine( 101 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 126L ) ) break;
	hb_xvmSetLine( 102 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 127L ) ) break;
	hb_xvmSetLine( 103 );
	hb_xvmPushStringConst( "MAIN", 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 161L ) ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 164L ) ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 165L ) ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "_TEMPLATE_", 10 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 110 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 162L ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 114 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPopLocal( 1 );
lab00002: ;
	hb_xvmSetLine( 118 );
	hb_xvmArrayGen( 0 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroSymbol() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPop( 43 ) ) break;
	hb_xvmSetLine( 120 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDREPORT )
{
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 148 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 150 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 120L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 153 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 127L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 124L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 123L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 125L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 161 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 163 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 126L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 121L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 122L ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmArrayGen( 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPop( 43 ) ) break;
	hb_xvmSetLine( 165 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINLAYOUT )
{
   do {
	hb_xvmSetLine( 183 );
	hb_xvmPushStringConst( "LAYOUT", 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 161L ) ) break;
	hb_xvmSetLine( 185 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDLAYOUT )
{
   do {
	hb_xvmSetLine( 203 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 204 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 118L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 119L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 208 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINHEADER )
{
   do {
	hb_xvmSetLine( 226 );
	hb_xvmPushStringConst( "HEADER", 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 161L ) ) break;
	hb_xvmSetLine( 228 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 160L ) ) break;
	hb_xvmSetLine( 230 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDHEADER )
{
   do {
	hb_xvmSetLine( 248 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINDETAIL )
{
   do {
	hb_xvmSetLine( 267 );
	hb_xvmPushStringConst( "DETAIL", 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 161L ) ) break;
	hb_xvmSetLine( 269 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 158L ) ) break;
	hb_xvmSetLine( 271 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDDETAIL )
{
   do {
	hb_xvmSetLine( 289 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINFOOTER )
{
   do {
	hb_xvmSetLine( 307 );
	hb_xvmPushStringConst( "FOOTER", 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 161L ) ) break;
	hb_xvmSetLine( 309 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 157L ) ) break;
	hb_xvmSetLine( 311 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDFOOTER )
{
   do {
	hb_xvmSetLine( 329 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINSUMMARY )
{
   do {
	hb_xvmSetLine( 347 );
	hb_xvmPushStringConst( "SUMMARY", 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 161L ) ) break;
	hb_xvmSetLine( 349 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDSUMMARY )
{
   do {
	hb_xvmSetLine( 367 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINTEXT )
{
   do {
	hb_xvmSetLine( 386 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 116L ) ) break;
	hb_xvmSetLine( 387 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 388 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 389 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 390 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 391 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 392 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 393 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmSetLine( 394 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmSetLine( 395 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmSetLine( 396 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmSetLine( 397 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmSetLine( 398 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 341L ) ) break;
	hb_xvmSetLine( 399 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 288L ) ) break;
	hb_xvmSetLine( 401 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDTEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 438 );
	hb_xvmPushStringConst( "TEXT", 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 288L ) ) break;
	hb_xvmArrayGen( 15 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 440 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "HEADER", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 442 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00001: ;
	hb_xvmSetLine( 444 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "DETAIL", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 446 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 448 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "FOOTER", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 450 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 452 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "SUMMARY", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 454 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 126L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 456 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "GROUPHEADER", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 458 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 121L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 460 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "GROUPFOOTER", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 462 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 122L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 466 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BANDHEIGHT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 489 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "HEADER", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 491 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 152L ) ) break;
	goto lab00006;
lab00001: ;
	hb_xvmSetLine( 493 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "DETAIL", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 495 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 153L ) ) break;
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 497 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "FOOTER", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 499 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 154L ) ) break;
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 501 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "SUMMARY", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 503 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 127L ) ) break;
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 505 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "GROUPHEADER", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 507 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 124L ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 509 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "GROUPFOOTER", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 511 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 123L ) ) break;
lab00006: ;
	hb_xvmSetLine( 515 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( EXECUTEREPORT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 35, 4 );
	hb_xvmSetLine( 583 );
	hb_xvmPushInteger( 18 );
	hb_xvmPushInteger( 2 );
	hb_xvmArrayDim( 2 );
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 584 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 585 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 35 );
	hb_xvmSetLine( 591 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 120L ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 592 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushStringConst( "Only One Group Level Allowed", 28 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 595 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 149L ) ) break;
	hb_xvmSetLine( 596 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 150L ) ) break;
	hb_xvmSetLine( 597 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 163L ) ) break;
	hb_xvmSetLine( 599 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 601 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ".PDF", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 603 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 150L ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 605 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ".HTML", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 607 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 163L ) ) break;
lab00003: ;
	hb_xvmSetLine( 613 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 615 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( "<html>\x0D\x0A", 8 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 617 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( "<style>\x0D\x0A", 9 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 618 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( "div {position:absolute}\x0D\x0A", 25 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 619 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( ".line { }\x0D\x0A", 11 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 620 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( "</style>\x0D\x0A", 10 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 622 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( "<body>\x0D\x0A", 8 );
	if( hb_xvmPlusEqPop() ) break;
lab00004: ;
	hb_xvmSetLine( 626 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 627 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 628 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 200 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
lab00005: ;
	hb_xvmSetLine( 631 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 638 );
	if( hb_xvmPushAlias() ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 639 );
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 642 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 20 );
lab00007: ;
	hb_xvmSetLine( 654 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 656 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 657 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 658 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 659 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 660 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 661 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 662 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 663 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 8L ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 665 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 666 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 667 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 668 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 669 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 670 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 671 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 672 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 8L ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 673 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 9L ) ) break;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 674 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 676 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 677 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 678 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 679 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 681 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 682 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 2 );
lab00008: ;
	hb_xvmSetLine( 685 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 686 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 3 );
lab00009: ;
	hb_xvmSetLine( 689 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 691 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 692 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 11 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 694 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 11 );
lab00011: ;
	hb_xvmSetLine( 697 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 698 );
	/* *** END PROC *** */
	break;
lab00012: ;
	hb_xvmSetLine( 711 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_xvmSetLine( 713 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 715 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 256L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
	hb_xvmSetLine( 723 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	if( hb_xvmFunction( 11 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 19L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 14L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 15L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00014;
lab00013: ;
	hb_xvmPushLogical( HB_FALSE );
lab00014: ;
	hb_xvmPopLocal( 30 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 23L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMultByInt( 100L ) ) break;
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 24L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 25L ) ) break;
	goto lab00024;
lab00015: ;
	hb_xvmSetLine( 731 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	if( hb_xvmFunction( 11 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 19L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 14L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 15L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00017;
lab00016: ;
	hb_xvmPushLogical( HB_FALSE );
lab00017: ;
	hb_xvmPopLocal( 30 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 23L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMultByInt( 100L ) ) break;
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 24L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 25L ) ) break;
	goto lab00024;
lab00018: ;
	hb_xvmSetLine( 737 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 256L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 744 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	if( hb_xvmFunction( 11 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 19L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 14L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 15L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00019;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00020;
lab00019: ;
	hb_xvmPushLogical( HB_FALSE );
lab00020: ;
	hb_xvmPopLocal( 30 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 23L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMultByInt( 100L ) ) break;
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 24L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 25L ) ) break;
	goto lab00024;
lab00021: ;
	hb_xvmSetLine( 752 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	hb_xvmPushInteger( -999 );
	if( hb_xvmFunction( 11 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 19L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 14L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 15L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00023;
lab00022: ;
	hb_xvmPushLogical( HB_FALSE );
lab00023: ;
	hb_xvmPopLocal( 30 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 23L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMultByInt( 100L ) ) break;
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 24L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 25L ) ) break;
lab00024: ;
	hb_xvmSetLine( 757 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_xvmSetLine( 758 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushStringConst( "Report: Can't Init Printer.", 27 );
	if( hb_xvmDo( 1 ) ) break;
lab00025: ;
	hb_xvmSetLine( 771 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmGreaterEqualThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00028;
	hb_xvmPushLocal( 15 );
	if( hb_xvmLessEqualThenIntIs( 18L, &fValue ) ) break;
	if( !fValue )
		goto lab00028;
	hb_xvmSetLine( 773 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xFC" "j@", 10, 1 );
	hb_xvmPushDouble( * ( double * ) "fffffvq@", 10, 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 774 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xFC" "j@", 10, 1 );
	hb_xvmPushDouble( * ( double * ) "fffffvq@", 10, 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 775 );
	hb_xvmPushDouble( * ( double * ) "fffffvq@", 10, 1 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xFC" "z@", 10, 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 776 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xFC" "z@", 10, 1 );
	hb_xvmPushDouble( * ( double * ) "fffffvq@", 10, 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 777 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xFC" "j@", 10, 1 );
	hb_xvmPushDouble( * ( double * ) "\x9A\x99\x99\x99\x99" "9v@", 10, 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 778 );
	hb_xvmPushDouble( * ( double * ) "fffffva@", 10, 1 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xFC" "j@", 10, 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 779 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\x04" "g@", 10, 2 );
	hb_xvmPushDouble( * ( double * ) "33333\xAB" "p@", 10, 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 780 );
	hb_xvmPushInteger( 297 );
	hb_xvmPushInteger( 420 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 781 );
	hb_xvmPushInteger( 210 );
	hb_xvmPushInteger( 297 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 782 );
	hb_xvmPushInteger( 210 );
	hb_xvmPushInteger( 297 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 783 );
	hb_xvmPushInteger( 148 );
	hb_xvmPushInteger( 210 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 11L ) ) break;
	hb_xvmSetLine( 784 );
	hb_xvmPushInteger( 250 );
	hb_xvmPushInteger( 354 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 12L ) ) break;
	hb_xvmSetLine( 785 );
	hb_xvmPushInteger( 182 );
	hb_xvmPushInteger( 257 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 13L ) ) break;
	hb_xvmSetLine( 786 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xFC" "j@", 10, 1 );
	hb_xvmPushDouble( * ( double * ) "33333\xA3" "t@", 10, 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 14L ) ) break;
	hb_xvmSetLine( 787 );
	hb_xvmPushInteger( 215 );
	hb_xvmPushInteger( 275 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 15L ) ) break;
	hb_xvmSetLine( 788 );
	hb_xvmPushInteger( 254 );
	hb_xvmPushDouble( * ( double * ) "\x9A\x99\x99\x99\x99" "9v@", 10, 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 16L ) ) break;
	hb_xvmSetLine( 789 );
	hb_xvmPushDouble( * ( double * ) "fffffvq@", 10, 1 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xFC" "z@", 10, 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmSetLine( 790 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xFC" "j@", 10, 1 );
	hb_xvmPushDouble( * ( double * ) "fffffvq@", 10, 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmArrayItemPop( 18L ) ) break;
	hb_xvmSetLine( 792 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00026;
	hb_xvmSetLine( 794 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 13 );
	goto lab00029;
lab00026: ;
	hb_xvmSetLine( 796 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00027;
	hb_xvmSetLine( 798 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 13 );
	goto lab00029;
lab00027: ;
	hb_xvmSetLine( 802 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushStringConst( "Report: Orientation Not Supported.", 34 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00029;
lab00028: ;
	hb_xvmSetLine( 808 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushStringConst( "Report: Paper Size Not Supported.", 33 );
	if( hb_xvmDo( 1 ) ) break;
lab00029: ;
	hb_xvmSetLine( 813 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00047;
	hb_xvmSetLine( 822 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00030;
	hb_xvmSetLine( 824 );
	hb_xvmPushStringConst( "LETTER", 6 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00030: ;
	hb_xvmSetLine( 826 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00031;
	hb_xvmSetLine( 828 );
	hb_xvmPushStringConst( "LEGAL", 5 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00031: ;
	hb_xvmSetLine( 830 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( !fValue )
		goto lab00032;
	hb_xvmSetLine( 832 );
	hb_xvmPushStringConst( "A4", 2 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00032: ;
	hb_xvmSetLine( 834 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00033;
	hb_xvmSetLine( 836 );
	hb_xvmPushStringConst( "LEDGER", 6 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00033: ;
	hb_xvmSetLine( 838 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 7L, &fValue ) ) break;
	if( !fValue )
		goto lab00034;
	hb_xvmSetLine( 840 );
	hb_xvmPushStringConst( "EXECUTIVE", 9 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00034: ;
	hb_xvmSetLine( 842 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 8L, &fValue ) ) break;
	if( !fValue )
		goto lab00035;
	hb_xvmSetLine( 844 );
	hb_xvmPushStringConst( "A3", 2 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00035: ;
	hb_xvmSetLine( 846 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 20L, &fValue ) ) break;
	if( !fValue )
		goto lab00036;
	hb_xvmSetLine( 848 );
	hb_xvmPushStringConst( "COM10", 5 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00036: ;
	hb_xvmSetLine( 850 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 12L, &fValue ) ) break;
	if( !fValue )
		goto lab00037;
	hb_xvmSetLine( 852 );
	hb_xvmPushStringConst( "JIS B4", 6 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00037: ;
	hb_xvmSetLine( 854 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00038;
	hb_xvmSetLine( 856 );
	hb_xvmPushStringConst( "B5", 2 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00038: ;
	hb_xvmSetLine( 858 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 94L, &fValue ) ) break;
	if( !fValue )
		goto lab00039;
	hb_xvmSetLine( 860 );
	hb_xvmPushStringConst( "JPOST", 5 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00039: ;
	hb_xvmSetLine( 862 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 28L, &fValue ) ) break;
	if( !fValue )
		goto lab00040;
	hb_xvmSetLine( 864 );
	hb_xvmPushStringConst( "C5", 2 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00040: ;
	hb_xvmSetLine( 866 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 27L, &fValue ) ) break;
	if( !fValue )
		goto lab00041;
	hb_xvmSetLine( 868 );
	hb_xvmPushStringConst( "DL", 2 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00041: ;
	hb_xvmSetLine( 870 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 34L, &fValue ) ) break;
	if( !fValue )
		goto lab00042;
	hb_xvmSetLine( 872 );
	hb_xvmPushStringConst( "B5", 2 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00042: ;
	hb_xvmSetLine( 874 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 37L, &fValue ) ) break;
	if( !fValue )
		goto lab00043;
	hb_xvmSetLine( 876 );
	hb_xvmPushStringConst( "MONARCH", 7 );
	hb_xvmPopLocal( 34 );
	goto lab00044;
lab00043: ;
	hb_xvmSetLine( 880 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushStringConst( "Report: PDF Paper Size Not Supported.", 37 );
	if( hb_xvmDo( 1 ) ) break;
lab00044: ;
	hb_xvmSetLine( 891 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00045;
	hb_xvmSetLine( 893 );
	hb_xvmPushStringConst( "P", 1 );
	hb_xvmPopLocal( 35 );
	goto lab00047;
lab00045: ;
	hb_xvmSetLine( 895 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00046;
	hb_xvmSetLine( 897 );
	hb_xvmPushStringConst( "L", 1 );
	hb_xvmPopLocal( 35 );
	goto lab00047;
lab00046: ;
	hb_xvmSetLine( 901 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushStringConst( "Report: Orientation Not Supported.", 34 );
	if( hb_xvmDo( 1 ) ) break;
lab00047: ;
	hb_xvmSetLine( 915 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00048;
	hb_xvmSetLine( 917 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 26 );
lab00048: ;
	hb_xvmSetLine( 921 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 117L ) ) break;
	hb_xvmSetLine( 923 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00051;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00051;
	hb_xvmSetLine( 925 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 21L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00049;
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 18L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 20L ) ) break;
	goto lab00050;
lab00049: ;
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 26L ) ) break;
lab00050: ;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 27L ) ) break;
lab00051: ;
	hb_xvmSetLine( 929 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_xvmSetLine( 930 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmDo( 0 ) ) break;
lab00052: ;
	hb_xvmSetLine( 933 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 934 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 936 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00053;
	hb_xvmSetLine( 937 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 39 );
	goto lab00054;
lab00053: ;
	hb_xvmSetLine( 939 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 39 );
lab00054: ;
	hb_xvmSetLine( 942 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00093;
	hb_xvmSetLine( 944 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00083;
	hb_xvmSetLine( 946 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00055;
	hb_xvmSetLine( 948 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushInteger( 6 );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00057;
lab00055: ;
	hb_xvmSetLine( 952 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00056;
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 43 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "\\", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_hmg_print_preview_", 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	hb_xvmPushInteger( 18 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEq() ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".Emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 19L ) ) break;
	goto lab00057;
lab00056: ;
	hb_xvmPushFuncSymbol( symbols + 44 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00057: ;
	hb_xvmSetLine( 956 );
	hb_xvmLocalSetInt( 19, 0L );
	hb_xvmSetLine( 958 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 960 );
	hb_xvmCopyLocals( 16, 19 );
lab00058: ;
	hb_xvmSetLine( 964 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00061;
	hb_xvmSetLine( 966 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00059;
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00061;
lab00059: ;
	hb_xvmSetLine( 968 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00060;
	hb_xvmSetLine( 970 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 971 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPlusEqPop() ) break;
lab00060: ;
	hb_xvmSetLine( 975 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 976 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 978 );
	hb_xvmCopyLocals( 26, 28 );
	hb_xvmSetLine( 980 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 29 );
lab00061: ;
	hb_xvmSetLine( 986 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 988 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 990 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00062;
	hb_xvmSetLine( 991 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 992 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 39 );
	goto lab00063;
lab00062: ;
	hb_xvmSetLine( 994 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 995 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 39 );
lab00063: ;
	hb_xvmSetLine( 998 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00078;
	hb_xvmSetLine( 1007 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00070;
	hb_xvmSetLine( 1014 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00069;
	hb_xvmSetLine( 1016 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 1017 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1019 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00067;
	hb_xvmSetLine( 1021 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00064;
	hb_xvmPushFuncSymbol( symbols + 47 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00065;
lab00064: ;
	hb_xvmPushFuncSymbol( symbols + 48 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00065: ;
	hb_xvmSetLine( 1022 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00066;
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 43 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "\\", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_hmg_print_preview_", 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	hb_xvmPushInteger( 18 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEq() ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".Emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 19L ) ) break;
	goto lab00068;
lab00066: ;
	hb_xvmPushFuncSymbol( symbols + 44 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00068;
lab00067: ;
	hb_xvmSetLine( 1026 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushInteger( 6 );
	if( hb_xvmDo( 3 ) ) break;
lab00068: ;
	hb_xvmSetLine( 1030 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 117 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 1032 );
	hb_xvmLocalSetInt( 19, 0L );
	hb_xvmSetLine( 1033 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1034 );
	hb_xvmCopyLocals( 16, 19 );
lab00069: ;
	hb_xvmSetLine( 1038 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1039 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPlusEqPop() ) break;
lab00070: ;
	hb_xvmSetLine( 1050 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00077;
	hb_xvmSetLine( 1057 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00076;
	hb_xvmSetLine( 1059 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 1060 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1062 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00074;
	hb_xvmSetLine( 1064 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00071;
	hb_xvmPushFuncSymbol( symbols + 47 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00072;
lab00071: ;
	hb_xvmPushFuncSymbol( symbols + 48 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00072: ;
	hb_xvmSetLine( 1065 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00073;
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 43 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "\\", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_hmg_print_preview_", 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	hb_xvmPushInteger( 18 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEq() ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".Emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPop( 19L ) ) break;
	goto lab00075;
lab00073: ;
	hb_xvmPushFuncSymbol( symbols + 44 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00075;
lab00074: ;
	hb_xvmSetLine( 1069 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushInteger( 6 );
	if( hb_xvmDo( 3 ) ) break;
lab00075: ;
	hb_xvmSetLine( 1073 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 117 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 1075 );
	hb_xvmLocalSetInt( 19, 0L );
	hb_xvmSetLine( 1076 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1077 );
	hb_xvmCopyLocals( 16, 19 );
lab00076: ;
	hb_xvmSetLine( 1081 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00080;
lab00077: ;
	goto lab00080;
lab00078: ;
	hb_xvmSetLine( 1091 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00079;
	hb_xvmSetLine( 1093 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 26 );
lab00079: ;
	hb_xvmSetLine( 1097 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00058;
lab00080: ;
	hb_xvmSetLine( 1105 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 1107 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1109 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00082;
	hb_xvmSetLine( 1111 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00081;
	hb_xvmPushFuncSymbol( symbols + 47 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00082;
lab00081: ;
	hb_xvmPushFuncSymbol( symbols + 48 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00082: ;
	hb_xvmSetLine( 1115 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 117 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	goto lab00054;
lab00083: ;
	hb_xvmSetLine( 1119 );
	hb_xvmLocalSetInt( 19, 0L );
	hb_xvmSetLine( 1121 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1123 );
	hb_xvmCopyLocals( 16, 19 );
lab00084: ;
	hb_xvmSetLine( 1127 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00086;
	hb_xvmSetLine( 1129 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00086;
	hb_xvmSetLine( 1131 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00085;
	hb_xvmSetLine( 1133 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1134 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPlusEqPop() ) break;
lab00085: ;
	hb_xvmSetLine( 1138 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1139 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 1141 );
	hb_xvmCopyLocals( 26, 28 );
	hb_xvmSetLine( 1143 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 29 );
lab00086: ;
	hb_xvmSetLine( 1149 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1151 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 1153 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00087;
	hb_xvmSetLine( 1154 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1155 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 39 );
	goto lab00088;
lab00087: ;
	hb_xvmSetLine( 1157 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 1158 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 39 );
lab00088: ;
	hb_xvmSetLine( 1161 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00091;
	hb_xvmSetLine( 1168 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00089;
	hb_xvmSetLine( 1170 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1171 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPlusEqPop() ) break;
lab00089: ;
	hb_xvmSetLine( 1180 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00090;
	hb_xvmSetLine( 1181 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1182 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPlusEqPop() ) break;
lab00090: ;
	goto lab00092;
lab00091: ;
	hb_xvmSetLine( 1189 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00084;
	hb_xvmSetLine( 1190 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 1191 );
	goto lab00084;
lab00092: ;
	hb_xvmSetLine( 1195 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00054;
lab00093: ;
	hb_xvmSetLine( 1201 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00095;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00095;
	hb_xvmSetLine( 1203 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00094;
	hb_xvmPushFuncSymbol( symbols + 50 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00097;
lab00094: ;
	hb_xvmPushFuncSymbol( symbols + 51 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00097;
lab00095: ;
	hb_xvmSetLine( 1205 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00096;
	hb_xvmSetLine( 1207 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00097;
lab00096: ;
	hb_xvmSetLine( 1209 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00097;
	hb_xvmSetLine( 1211 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( "</body>\x0D\x0A", 9 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 1212 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( "</html>\x0D\x0A", 9 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 1214 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 1216 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 49 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1218 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmDo( 1 ) ) break;
lab00097: ;
	hb_xvmSetLine( 1222 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00098;
	hb_xvmSetLine( 1223 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmDo( 1 ) ) break;
lab00098: ;
	hb_xvmSetLine( 1226 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00099;
	hb_xvmSetLine( 1230 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00099: ;
	hb_xvmSetLine( 1234 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _PROCESSBAND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 1258 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1260 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1258 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00002: ;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1264 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _PRINTOBJECT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 1287 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "TEXT", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1289 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 1291 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "IMAGE", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1293 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 1295 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "LINE", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1297 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 1299 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "RECTANGLE", 9 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1301 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00004: ;
	hb_xvmSetLine( 1305 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _PRINTTEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 18, 2 );
	hb_xvmSetLine( 1343 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1344 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1345 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1346 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1347 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1348 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1349 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 8L ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1350 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 9L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 1351 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 1352 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 11L ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 1353 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 12L ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 1354 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 13L ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 1355 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 14L ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 1356 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 15L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 1357 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 1358 );
	hb_xvmLocalSetInt( 18, 0L );
	hb_xvmSetLine( 1359 );
	hb_xvmLocalSetInt( 19, 5L );
	hb_xvmSetLine( 1362 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1364 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 1366 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1368 );
	hb_xvmPushStringConst( "CENTER", 6 );
	hb_xvmPopLocal( 17 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 1370 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1372 );
	hb_xvmPushStringConst( "RIGHT", 5 );
	hb_xvmPopLocal( 17 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 1374 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 1376 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 17 );
lab00003: ;
	hb_xvmSetLine( 1380 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 17 );
	if( hb_xvmDo( 19 ) ) break;
	goto lab00034;
lab00004: ;
	hb_xvmSetLine( 1382 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 1384 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 1386 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00009;
lab00005: ;
	hb_xvmSetLine( 1388 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 1390 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00009;
lab00006: ;
	hb_xvmSetLine( 1392 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 1394 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 371L ) ) break;
	if( hb_xvmArrayItemPush( 24L ) ) break;
	goto lab00008;
lab00007: ;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 371L ) ) break;
	if( hb_xvmArrayItemPush( 25L ) ) break;
lab00008: ;
	hb_xvmPopLocal( 3 );
lab00009: ;
	hb_xvmSetLine( 1398 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 1400 );
	hb_xvmPushStringConst( "center", 6 );
	hb_xvmPopLocal( 20 );
	goto lab00012;
lab00010: ;
	hb_xvmSetLine( 1402 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 1404 );
	hb_xvmPushStringConst( "right", 5 );
	hb_xvmPopLocal( 20 );
	goto lab00012;
lab00011: ;
	hb_xvmSetLine( 1406 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 1408 );
	hb_xvmPushStringConst( "left", 4 );
	hb_xvmPopLocal( 20 );
lab00012: ;
	hb_xvmSetLine( 1412 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( "<div style=position:absolute;left:", 34 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;top:", 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;width:", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;font-size:", 13 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "pt;font-family:\"", 16 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\";text-align:", 13 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ";font-weight:", 13 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushStringConst( "bold", 4 );
	goto lab00014;
lab00013: ;
	hb_xvmPushStringConst( "normal", 6 );
lab00014: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ";font-style:", 12 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushStringConst( "italic", 6 );
	goto lab00016;
lab00015: ;
	hb_xvmPushStringConst( "normal", 6 );
lab00016: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ";text-decoration:", 17 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmPushStringConst( "underline", 9 );
	goto lab00018;
lab00017: ;
	hb_xvmPushStringConst( "none", 4 );
lab00018: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ";color:rgb(", 11 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ");>", 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "</div>", 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00034;
lab00019: ;
	hb_xvmSetLine( 1414 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmSetLine( 1416 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 1418 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00024;
lab00020: ;
	hb_xvmSetLine( 1420 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 1422 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00024;
lab00021: ;
	hb_xvmSetLine( 1424 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 1426 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 371L ) ) break;
	if( hb_xvmArrayItemPush( 24L ) ) break;
	goto lab00023;
lab00022: ;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 371L ) ) break;
	if( hb_xvmArrayItemPush( 25L ) ) break;
lab00023: ;
	hb_xvmPopLocal( 3 );
lab00024: ;
	hb_xvmSetLine( 1430 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_xvmSetLine( 1432 );
	hb_xvmLocalSetInt( 18, 0L );
	goto lab00028;
lab00025: ;
	hb_xvmSetLine( 1434 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00026;
	hb_xvmSetLine( 1436 );
	hb_xvmLocalSetInt( 18, 1L );
	goto lab00028;
lab00026: ;
	hb_xvmSetLine( 1438 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00027;
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 1440 );
	hb_xvmLocalSetInt( 18, 2L );
	goto lab00028;
lab00027: ;
	hb_xvmSetLine( 1442 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 1444 );
	hb_xvmLocalSetInt( 18, 3L );
lab00028: ;
	hb_xvmSetLine( 1448 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1450 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00030;
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 1452 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmSetLine( 1454 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "\xFE", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "ffffff9@", 10, 1 );
	if( hb_xvmMult() ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "M", 1 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00034;
lab00029: ;
	hb_xvmSetLine( 1458 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "\xFD", 1 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "ffffff9@", 10, 1 );
	if( hb_xvmMult() ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "M", 1 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00034;
lab00030: ;
	hb_xvmSetLine( 1462 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00032;
	hb_xvmSetLine( 1464 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 1466 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "\xFE", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "ffffff9@", 10, 1 );
	if( hb_xvmMult() ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushStringConst( "M", 1 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00034;
lab00031: ;
	hb_xvmSetLine( 1470 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "\xFD", 1 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushDouble( * ( double * ) "ffffff9@", 10, 1 );
	if( hb_xvmMult() ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushStringConst( "M", 1 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00034;
lab00032: ;
	hb_xvmSetLine( 1474 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00034;
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00034;
	hb_xvmSetLine( 1476 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmSetLine( 1478 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "\xFE", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "M", 1 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00034;
lab00033: ;
	hb_xvmSetLine( 1482 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushStringConst( "\xFD", 1 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "M", 1 );
	if( hb_xvmDo( 4 ) ) break;
lab00034: ;
	hb_xvmSetLine( 1490 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _PRINTIMAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 2 );
	hb_xvmSetLine( 1520 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1521 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1522 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1523 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1524 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1526 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 1528 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 7 ) ) break;
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 1530 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1532 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ".JPG", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1534 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "M", 1 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 6 ) ) break;
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 1538 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushStringConst( "Report: Only JPG images allowed.", 32 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 1542 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1544 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( "<div style=position:absolute;left:", 34 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;top:", 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;> <img src=\"", 15 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\" ", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "width=", 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xCC\x0E@", 10, 2 );
	if( hb_xvmMult() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm height=", 10 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xCC\x0E@", 10, 2 );
	if( hb_xvmMult() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm/> </div>", 11 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00004: ;
	hb_xvmSetLine( 1548 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _PRINTLINE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 2 );
	hb_xvmSetLine( 1579 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1580 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1581 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1582 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1583 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1584 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1586 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 1588 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 11 ) ) break;
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 1590 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1592 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1593 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushStringConst( "Report: Only horizontal and vertical lines are supported with PDF output.", 73 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 1596 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushStringConst( "M", 1 );
	hb_xvmPushStringConst( "\xFD", 1 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 8 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 1598 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1600 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( "<div style=\"left:", 17 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;top:", 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;width:", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;height:0mm;BORDER-STYLE:SOLID;BORDER-COLOR:", 46 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "rgb(", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ";BORDER-WIDTH:", 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;BACKGROUND-COLOR:#FFFFFF;\"><span class=\"line\"></span></DIV>", 62 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00004: ;
	hb_xvmSetLine( 1604 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _PRINTRECTANGLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 2 );
	hb_xvmSetLine( 1635 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1636 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1637 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1638 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1639 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1640 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1643 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 1645 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 11 ) ) break;
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 1647 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1649 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushStringConst( "M", 1 );
	hb_xvmPushStringConst( "\xFD", 1 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 1650 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushStringConst( "M", 1 );
	hb_xvmPushStringConst( "\xFD", 1 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 1651 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushStringConst( "M", 1 );
	hb_xvmPushStringConst( "\xFD", 1 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 1652 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushStringConst( "M", 1 );
	hb_xvmPushStringConst( "\xFD", 1 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 8 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 1654 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1656 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 149 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushStringConst( "<div style=\"left:", 17 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;top:", 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;width:", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;height:", 10 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;BORDER-STYLE:SOLID;BORDER-COLOR:", 35 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "rgb(", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ";BORDER-WIDTH:", 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mm;BACKGROUND-COLOR:#FFFFFF;\"><span class=\"line\"></span></DIV>", 62 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00003: ;
	hb_xvmSetLine( 1660 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINLINE )
{
   do {
	hb_xvmSetLine( 1675 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 110L ) ) break;
	hb_xvmSetLine( 1676 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 111L ) ) break;
	hb_xvmSetLine( 1677 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 112L ) ) break;
	hb_xvmSetLine( 1678 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 113L ) ) break;
	hb_xvmSetLine( 1679 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 114L ) ) break;
	hb_xvmSetLine( 1680 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 115L ) ) break;
	hb_xvmSetLine( 1682 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDLINE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 1708 );
	hb_xvmPushStringConst( "LINE", 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 110L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 111L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 112L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 113L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 115L ) ) break;
	hb_xvmArrayGen( 7 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 1710 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "HEADER", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1712 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00001: ;
	hb_xvmSetLine( 1714 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "DETAIL", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1716 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 1718 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "FOOTER", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1720 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 1722 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "SUMMARY", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1724 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 126L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 1726 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "GROUPHEADER", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 1728 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 121L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 1730 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "GROUPFOOTER", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 1732 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 122L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 1736 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINIMAGE )
{
   do {
	hb_xvmSetLine( 1752 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 1753 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 1754 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 1755 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 1756 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 1757 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 311L ) ) break;
	hb_xvmSetLine( 1759 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDIMAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 1785 );
	hb_xvmPushStringConst( "IMAGE", 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 311L ) ) break;
	hb_xvmArrayGen( 7 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 1787 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "HEADER", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1789 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00001: ;
	hb_xvmSetLine( 1791 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "DETAIL", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1793 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 1795 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "FOOTER", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1797 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 1799 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "SUMMARY", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1801 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 126L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 1803 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "GROUPHEADER", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 1805 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 121L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 1807 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "GROUPFOOTER", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 1809 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 122L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 1813 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINRECTANGLE )
{
   do {
	hb_xvmSetLine( 1828 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 110L ) ) break;
	hb_xvmSetLine( 1829 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 111L ) ) break;
	hb_xvmSetLine( 1830 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 112L ) ) break;
	hb_xvmSetLine( 1831 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 113L ) ) break;
	hb_xvmSetLine( 1832 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 114L ) ) break;
	hb_xvmSetLine( 1833 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 115L ) ) break;
	hb_xvmSetLine( 1835 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDRECTANGLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 1861 );
	hb_xvmPushStringConst( "RECTANGLE", 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 110L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 111L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 112L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 113L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 115L ) ) break;
	hb_xvmArrayGen( 7 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 1863 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "HEADER", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1865 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00001: ;
	hb_xvmSetLine( 1867 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "DETAIL", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1869 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 1871 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "FOOTER", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1873 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 1875 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "SUMMARY", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1877 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 126L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 1879 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "GROUPHEADER", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 1881 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 121L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 1883 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushStringConst( "GROUPFOOTER", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 1885 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPush( 122L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 1889 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINGROUP )
{
   do {
	hb_xvmSetLine( 1903 );
	hb_xvmPushStringConst( "GROUP", 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 161L ) ) break;
	hb_xvmSetLine( 1905 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	hb_xvmPushInteger( 120 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 1907 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDGROUP )
{
   do {
	hb_xvmSetLine( 1925 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINGROUPHEADER )
{
   do {
	hb_xvmSetLine( 1942 );
	hb_xvmPushStringConst( "GROUPHEADER", 11 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 161L ) ) break;
	hb_xvmSetLine( 1944 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDGROUPHEADER )
{
   do {
	hb_xvmSetLine( 1961 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINGROUPFOOTER )
{
   do {
	hb_xvmSetLine( 1978 );
	hb_xvmPushStringConst( "GROUPFOOTER", 11 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 450L ) ) break;
	if( hb_xvmArrayItemPop( 161L ) ) break;
	hb_xvmSetLine( 1980 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDGROUPFOOTER )
{
   do {
	hb_xvmSetLine( 1997 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DBSUM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 2023 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 2024 );
	hb_xvmLocalSetInt( 2, 0L );
	hb_xvmPushFuncSymbol( symbols + 90 );
	{
		static const HB_BYTE codeblock[ 17 ] = {
			0, 0, 2, 0, 2, 0, 1, 0, 96, 255, 255, 95, 254, 40, 43, 139, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 2025 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 2028 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINDATA )
{
   do {
	hb_xvmSetLine( 2042 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDDATA )
{
   do {
	hb_xvmSetLine( 2056 );
	/* *** END PROC *** */
   } while( 0 );
}

