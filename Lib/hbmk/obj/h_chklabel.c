/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_chklabel.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINECHKLABEL );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( HB_UAT );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ULEFT );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( CVALTOCHAR );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_STATIC( GETCHECKBMP );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( INITDIALOGLABEL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( SETWINDOWTEXT );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITCHKLABEL );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( _DEFINETIMER );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( _SHOWCONTROL );
HB_FUNC_EXTERN( _HIDECONTROL );
HB_FUNC_EXTERN( _SETCONTROLWIDTH );
HB_FUNC_EXTERN( GETTEXTWIDTH );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( GETBORDERWIDTH );
HB_FUNC_EXTERN( _SETCONTROLHEIGHT );
HB_FUNC_EXTERN( REDRAWWINDOW );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_STATIC( CANSI2BMP );
HB_FUNC_EXTERN( TEMPFILE );
HB_FUNC_EXTERN( GETTEMPFOLDER );
HB_FUNC_EXTERN( FILE );
HB_FUNC_EXTERN( FERASE );
HB_FUNC_EXTERN( FCREATE );
HB_FUNC_EXTERN( FWRITE );
HB_FUNC_EXTERN( FCLOSE );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_STATIC( CHEX2BIN );
HB_FUNC_STATIC( CANSI2HEX );
HB_FUNC_EXTERN( STUFF );
HB_FUNC_EXTERN( PADR );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( MAX );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( L2BIN );
HB_FUNC_EXTERN( INT );
HB_FUNC_EXTERN( I2BIN );
HB_FUNC_EXTERN( CHR );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_CHKLABEL )
{ "_DEFINECHKLABEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINECHKLABEL )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "HB_UAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UAT )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ULEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ULEFT )}, NULL },
{ "SELECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SELECT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "CVALTOCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CVALTOCHAR )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "GETCHECKBMP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETCHECKBMP )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "INITDIALOGLABEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITDIALOGLABEL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "SETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTEXT )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITCHKLABEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITCHKLABEL )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "_DEFINETIMER", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINETIMER )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "_SHOWCONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SHOWCONTROL )}, NULL },
{ "_HIDECONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HIDECONTROL )}, NULL },
{ "_SETCONTROLWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETCONTROLWIDTH )}, NULL },
{ "GETTEXTWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEXTWIDTH )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "GETBORDERWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBORDERWIDTH )}, NULL },
{ "_SETCONTROLHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETCONTROLHEIGHT )}, NULL },
{ "REDRAWWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( REDRAWWINDOW )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "CANSI2BMP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( CANSI2BMP )}, NULL },
{ "TEMPFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( TEMPFILE )}, NULL },
{ "GETTEMPFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEMPFOLDER )}, NULL },
{ "FILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FILE )}, NULL },
{ "FERASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERASE )}, NULL },
{ "FCREATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCREATE )}, NULL },
{ "FWRITE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FWRITE )}, NULL },
{ "FCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCLOSE )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "CHEX2BIN", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( CHEX2BIN )}, NULL },
{ "CANSI2HEX", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( CANSI2HEX )}, NULL },
{ "STUFF", {HB_FS_PUBLIC}, {HB_FUNCNAME( STUFF )}, NULL },
{ "PADR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PADR )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "MAX", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAX )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "L2BIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( L2BIN )}, NULL },
{ "INT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INT )}, NULL },
{ "I2BIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( I2BIN )}, NULL },
{ "CHR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHR )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_CHKLABEL, "h_chklabel.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_CHKLABEL
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_CHKLABEL )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINECHKLABEL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 12, 37 );
	hb_xvmSetLine( 67 );
	hb_xvmLocalSetInt( 44, 0L );
	hb_xvmSetLine( 71 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 48 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 49 );
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 49 );
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 18 );
	{
		static const HB_BYTE codeblock[ 29 ] = {
			0, 0, 2, 0, 2, 0, 1, 0, 176, 4, 0, 95, 255, 95, 254, 106, 
			8, 67, 104, 101, 99, 107, 101, 100, 0, 100, 12, 4, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 23 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 24 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 35 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 40 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 40 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLocalByRef( 23 );
	hb_xvmPushLocalByRef( 24 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 90 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " : You must specify a fully qualified field name.", 49 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 41 );
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 41 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 96 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 34 );
lab00003: ;
	hb_xvmSetLine( 101 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
lab00004: ;
	hb_xvmSetLine( 102 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00006;
lab00005: ;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00006: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 107 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 108 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 110 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 111 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushLocal( 16 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 112 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 15 );
lab00008: ;
	hb_xvmSetLine( 116 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 47 );
	hb_xvmSetLine( 118 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmPushLocal( 47 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 119 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 2 );
lab00010: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00011: ;
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00012: ;
	hb_xvmSetLine( 126 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLocal( 47 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Already defined.", 17 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00013: ;
	hb_xvmSetLine( 130 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
lab00014: ;
	hb_xvmSetLine( 134 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_xvmSetLine( 135 );
	hb_xvmCopyLocals( 32, 42 );
	hb_xvmSetLine( 136 );
	hb_xvmPushInteger( 2 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00016;
lab00015: ;
	hb_xvmPushLocal( 42 );
lab00016: ;
	hb_xvmPushLocal( 32 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
lab00017: ;
	hb_xvmSetLine( 140 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 43 );
	hb_xvmSetLine( 141 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 44 );
	hb_xvmSetLine( 143 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 145 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 147 );
	hb_xvmLocalSetInt( 45, 1073742080L );
	hb_xvmSetLine( 149 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 150 );
	hb_xvmPushLocalByRef( 45 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8388608 );
#else
	hb_xvmPushLong( 8388608L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00018: ;
	hb_xvmSetLine( 153 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
	hb_xvmSetLine( 154 );
	hb_xvmPushLocalByRef( 45 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00019: ;
	hb_xvmSetLine( 157 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 158 );
	hb_xvmPushLocalByRef( 45 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1048576 );
#else
	hb_xvmPushLong( 1048576L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00020: ;
	hb_xvmSetLine( 161 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 162 );
	hb_xvmPushLocalByRef( 45 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2097152 );
#else
	hb_xvmPushLong( 2097152L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00021: ;
	hb_xvmSetLine( 165 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 166 );
	if( hb_xvmLocalAddInt( 45, 2 ) ) break;
lab00022: ;
	hb_xvmSetLine( 169 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 170 );
	if( hb_xvmLocalAddInt( 45, 1 ) ) break;
lab00023: ;
	hb_xvmSetLine( 173 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 174 );
	if( hb_xvmLocalAddInt( 45, 512 ) ) break;
lab00024: ;
	hb_xvmSetLine( 177 );
	hb_xvmPushLocal( 47 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 180 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 21, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 46 );
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 44 );
	hb_xvmPushStringConst( "static", 6 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 46 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00029;
lab00025: ;
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 39 );
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 188 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 190 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 193 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 194 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00026: ;
	hb_xvmSetLine( 197 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00029;
lab00027: ;
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 220 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00028;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
lab00028: ;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 32 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmFunction( 23 ) ) break;
	hb_xvmPopLocal( 39 );
lab00029: ;
	hb_xvmSetLine( 224 );
	hb_xvmPushLocal( 47 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00033;
	hb_xvmSetLine( 226 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00030;
	hb_xvmSetLine( 227 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00031;
lab00030: ;
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 230 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 40 );
lab00031: ;
	hb_xvmSetLine( 234 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmSetLine( 235 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmDo( 2 ) ) break;
lab00032: ;
	hb_xvmSetLine( 238 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmSetLine( 239 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00033: ;
	hb_xvmSetLine( 245 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 43 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 250 );
	hb_xvmPushStringConst( "CHECKLABEL", 10 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 251 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 252 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 253 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 254 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 255 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 258 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 260 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 261 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 262 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 263 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 264 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 265 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 266 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 267 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 268 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 269 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 270 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 271 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmPushInteger( 1 );
	goto lab00035;
lab00034: ;
	hb_xvmPushInteger( 0 );
lab00035: ;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 272 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00036;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00037;
lab00036: ;
	hb_xvmPushInteger( -1 );
lab00037: ;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 273 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00038;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00039;
lab00038: ;
	hb_xvmPushInteger( -1 );
lab00039: ;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 274 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 275 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 276 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 277 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 278 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 280 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 282 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00041;
lab00040: ;
	hb_xvmPushLogical( HB_TRUE );
lab00041: ;
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 286 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 288 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmArrayGen( 3 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00042;
	hb_xvmPushLocal( 47 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00042;
	hb_xvmSetLine( 293 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushStringConst( "BlinkTimer", 10 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 500 );
	{
		static const HB_BYTE codeblock[ 73 ] = {
			0, 0, 3, 0, 44, 0, 1, 0, 2, 0, 98, 11, 0, 93, 171, 0, 
			1, 95, 255, 1, 92, 3, 1, 68, 98, 11, 0, 93, 171, 0, 1, 95, 
			255, 1, 92, 3, 2, 98, 11, 0, 93, 171, 0, 1, 95, 255, 1, 92, 
			3, 1, 28, 13, 176, 37, 0, 95, 254, 95, 253, 12, 2, 25, 11, 176, 
			38, 0, 95, 254, 95, 253, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
lab00042: ;
	hb_xvmSetLine( 296 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00051;
	hb_xvmPushLocal( 47 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00051;
	hb_xvmSetLine( 298 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00043;
	hb_xvmPushLocal( 22 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00044;
lab00043: ;
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00045;
lab00044: ;
	hb_xvmPushInteger( 0 );
lab00045: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00046;
	hb_xvmPushLocal( 33 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00046;
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00048;
lab00046: ;
	hb_xvmPushLocal( 33 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00047;
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	goto lab00048;
lab00047: ;
	hb_xvmPushInteger( 0 );
lab00048: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 299 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmLessThenIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00049;
	hb_xvmPushInteger( 22 );
	goto lab00050;
lab00049: ;
	hb_xvmPushLocal( 9 );
	if( hb_xvmAddInt( 16L ) ) break;
lab00050: ;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 300 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmDo( 1 ) ) break;
lab00051: ;
	hb_xvmSetLine( 303 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_xvmSetLine( 304 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmPushMemvar( symbols + 11 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmDo( 2 ) ) break;
lab00052: ;
	hb_xvmSetLine( 315 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 44 );
	hb_xvmPushLocal( 49 );
	hb_xvmPushLocal( 48 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 317 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( GETCHECKBMP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 0 );
	hb_xvmSetLine( 340 );
	hb_xvmPushStringConst( "42 4D F6 00 00 00 00 00 00 00 76 00 00 00 28 0000 00 10 00 00 00 10 00 00 00 01 00 04 00 00 0000 00 80 00 00 00 C4 0E 00 00 C4 0E 00 00 00 0000 00 00 00 00 00 00 00 00 00 00 00 80 00 00 8000 00 00 80 80 00 80 00 00 00 80 00 80 00 80 8000 00 80 80 80 00 C0 C0 C0 00 00 00 FF 00 00 FF00 00 00 FF FF 00 FF 00 00 00 FF 00 FF 00 FF FF00 00 FF FF FF 00 FF FF FF FF FF FF FF FF FF FFFF 7F FF FF FF FF FF FF F8 07 FF FF FF FF FF FF80 00 7F FF FF FF FF FF 80 00 78 FF FF FF FF F800 70 08 FF FF FF FF 80 07 80 07 8F FF FF F8 007F F7 00 8F FF FF F8 07 8F F8 00 78 FF FF F8 8FFF FF 70 07 8F FF FF FF FF FF 87 00 7F FF FF FFFF FF F8 70 07 FF FF FF FF FF FF 87 00 8F FF FFFF FF FF F8 70 8F FF FF FF FF FF FF 88 8F FF FFFF FF FF FF FF FF", 722 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 342 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 343 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 344 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "BMP", 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 346 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 347 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 350 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	if( hb_xvmLessThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 351 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 354 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 356 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 357 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 5 );
lab00003: ;
	hb_xvmSetLine( 360 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 362 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( CANSI2BMP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 370 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 372 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterEqualThenIntIs( 8L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 373 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 374 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 375 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 8 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00001;
lab00002: ;
	hb_xvmSetLine( 378 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 379 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00003: ;
	hb_xvmSetLine( 382 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( CANSI2HEX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 390 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 392 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 394 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterEqualThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 395 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 396 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmLocalAdd( 3 );
	hb_xvmSetLine( 397 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00001;
lab00002: ;
	hb_xvmSetLine( 400 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( CHEX2BIN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 1 );
	hb_xvmSetLine( 410 );
	hb_xvmLocalSetInt( 4, 0L );
	hb_xvmLocalSetInt( 5, 0L );
	hb_xvmPushStringConst( "0", 1 );
	hb_xvmPushStringConst( "1", 1 );
	hb_xvmPushStringConst( "2", 1 );
	hb_xvmPushStringConst( "3", 1 );
	hb_xvmPushStringConst( "4", 1 );
	hb_xvmPushStringConst( "5", 1 );
	hb_xvmPushStringConst( "6", 1 );
	hb_xvmPushStringConst( "7", 1 );
	hb_xvmPushStringConst( "8", 1 );
	hb_xvmPushStringConst( "9", 1 );
	hb_xvmPushStringConst( "A", 1 );
	hb_xvmPushStringConst( "B", 1 );
	hb_xvmPushStringConst( "C", 1 );
	hb_xvmPushStringConst( "D", 1 );
	hb_xvmPushStringConst( "E", 1 );
	hb_xvmPushStringConst( "F", 1 );
	hb_xvmArrayGen( 16 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 412 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 414 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 415 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 416 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 16 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPower() ) break;
	if( hb_xvmMult() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 417 );
	if( hb_xvmLocalInc( 4 ) ) break;
	hb_xvmSetLine( 414 );
	if( hb_xvmLocalAddInt( 2, -1 ) ) break;
	hb_xvmPushLocal( 2 );
lab00002: ;
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 420 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00005;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00005: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

