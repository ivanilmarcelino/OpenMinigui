/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_mru.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( ADDMRUITEM );
HB_FUNC_STATIC( CHECKFORDUPLICATEMRU );
HB_FUNC_STATIC( REORDERMRULIST );
HB_FUNC( ADDMENUELEMENT );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( AT );
HB_FUNC_EXTERN( _MODIFYMENUITEM );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( _INSERTMENUITEM );
HB_FUNC_EXTERN( HB_AINS );
HB_FUNC_EXTERN( ASIZE );
HB_FUNC_EXTERN( _REMOVEMENUITEM );
HB_FUNC_EXTERN( HB_ADEL );
HB_FUNC( SAVEMRUFILELIST );
HB_FUNC_EXTERN( _BEGININI );
HB_FUNC_EXTERN( _SETINI );
HB_FUNC_EXTERN( _ENDINI );
HB_FUNC( _DEFINEMRUITEM );
HB_FUNC_EXTERN( _GETINI );
HB_FUNC_EXTERN( _DEFINEMENUITEM );
HB_FUNC( CLEARMRULIST );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_MRU )
{ "ADDMRUITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ADDMRUITEM )}, NULL },
{ "CHECKFORDUPLICATEMRU", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( CHECKFORDUPLICATEMRU )}, NULL },
{ "REORDERMRULIST", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( REORDERMRULIST )}, NULL },
{ "ADDMENUELEMENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ADDMENUELEMENT )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "AT", {HB_FS_PUBLIC}, {HB_FUNCNAME( AT )}, NULL },
{ "_MODIFYMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( _MODIFYMENUITEM )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "_INSERTMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( _INSERTMENUITEM )}, NULL },
{ "HB_AINS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_AINS )}, NULL },
{ "ASIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASIZE )}, NULL },
{ "_REMOVEMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( _REMOVEMENUITEM )}, NULL },
{ "HB_ADEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ADEL )}, NULL },
{ "SAVEMRUFILELIST", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SAVEMRUFILELIST )}, NULL },
{ "_BEGININI", {HB_FS_PUBLIC}, {HB_FUNCNAME( _BEGININI )}, NULL },
{ "_SETINI", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETINI )}, NULL },
{ "_ENDINI", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDINI )}, NULL },
{ "_DEFINEMRUITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEMRUITEM )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_GETINI", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETINI )}, NULL },
{ "_DEFINEMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEMENUITEM )}, NULL },
{ "CLEARMRULIST", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLEARMRULIST )}, NULL },
{ "__ENUMISLAST", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "(_INITSTATICS00002)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_MRU, "h_mru.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_MRU
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_MRU )
   #include "hbiniseg.h"
#endif

HB_FUNC( ADDMRUITEM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSFrame( symbols + 30 );
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 71 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqualIntIs( -1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 74 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqualIntIs( 1L, &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 78 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( CHECKFORDUPLICATEMRU )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSFrame( symbols + 30 );
	hb_xvmSetLine( 83 );
	hb_xvmLocalSetInt( 2, -1L );
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStatic( 2 );
	{
		static const HB_BYTE codeblock[ 20 ] = {
			1, 0, 1, 0, 1, 0, 176, 5, 0, 95, 1, 92, 2, 1, 12, 1, 
			95, 255, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 91 );
	hb_xvmCopyLocals( 3, 2 );
lab00001: ;
	hb_xvmSetLine( 95 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ADDMENUELEMENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 2 );
	hb_xvmSFrame( symbols + 30 );
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessThenIntIs( 40L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushStringConst( "...", 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmAddInt( -34L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
lab00002: ;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 106 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	{
		static const HB_BYTE codeblock[ 2 ] = {
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	goto lab00004;
lab00003: ;
	hb_xvmPushStringConst( "{|| ", 4 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushStringConst( "(", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " \"", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\" ) }", 5 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmMacroPush( 43 ) ) break;
lab00004: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 109 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 111 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 112 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushStringConst( "&1 ", 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 1 );
	hb_xvmArrayGen( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00012;
lab00005: ;
	hb_xvmSetLine( 116 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 9 );
	goto lab00008;
lab00006: ;
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStatic( 2 );
	{
		static const HB_BYTE codeblock[ 15 ] = {
			1, 0, 1, 0, 9, 0, 95, 1, 92, 5, 1, 95, 255, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 118 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 119 );
	hb_xvmCopyLocals( 9, 8 );
	goto lab00009;
lab00007: ;
	hb_xvmSetLine( 116 );
	if( hb_xvmLocalIncPush( 9 ) ) break;
lab00008: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
lab00009: ;
	hb_xvmSetLine( 124 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 6 );
	hb_xvmSetLine( 125 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 126 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushStringConst( "&1 ", 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 128 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 8 );
	hb_xvmArrayGen( 5 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 9 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 130 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 131 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 132 );
	hb_xvmPushStringConst( "&", 1 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStatic( 2 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmLocalAdd( 5 );
	hb_xvmSetLine( 133 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 129 );
	if( hb_xvmLocalIncPush( 9 ) ) break;
lab00011: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 135 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 136 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00012: ;
	hb_xvmSetLine( 142 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 144 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( REORDERMRULIST )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSFrame( symbols + 30 );
	hb_xvmSetLine( 153 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 154 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
lab00001: ;
	hb_xvmSetLine( 159 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SAVEMRUFILELIST )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSFrame( symbols + 30 );
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 169 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 171 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushStatic( 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmPushStringConst( "", 0 );
lab00003: ;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 172 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 169 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00004: ;
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 177 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINEMRUITEM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 6 );
	hb_xvmSFrame( symbols + 30 );
	hb_xvmSetLine( 182 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 183 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 184 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 191 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( " (Empty) ", 9 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	hb_xvmPopLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 10 );
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 4 );
lab00004: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushStringConst( "MRU", 3 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 6 );
lab00006: ;
	hb_xvmPopLocal( 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushStringConst( "mru.ini", 7 );
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
	hb_xvmPushStringConst( "MRU", 3 );
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 3 );
lab00010: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 193 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 194 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 195 );
	if( hb_xvmPushMemvar( symbols + 24 ) ) break;
	if( hb_xvmArrayItemPush( 191L ) ) break;
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 197 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopStatic( 2 );
	hb_xvmSetLine( 198 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 199 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 10 );
	goto lab00013;
lab00011: ;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 207 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmSetLine( 209 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 210 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 212 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 213 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
lab00012: ;
	hb_xvmSetLine( 203 );
	if( hb_xvmLocalIncPush( 10 ) ) break;
lab00013: ;
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
lab00014: ;
	hb_xvmSetLine( 224 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 228 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 229 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 5 );
lab00015: ;
	hb_xvmSetLine( 232 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmEnumStart( 1, 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
lab00016: ;
	hb_xvmSetLine( 233 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 234 );
	if( hb_xvmEnumPrev() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
lab00017: ;
	hb_xvmEnumEnd();
	goto lab00019;
lab00018: ;
	hb_xvmSetLine( 238 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 13 ) ) break;
lab00019: ;
	hb_xvmSetLine( 242 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CLEARMRULIST )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSFrame( symbols + 30 );
	hb_xvmSetLine( 252 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmEnumStart( 1, 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 253 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 254 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 255 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushStringConst( " (Empty) ", 9 );
	{
		static const HB_BYTE codeblock[ 2 ] = {
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 258 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopStatic( 2 );
	hb_xvmSetLine( 259 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 261 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 279 );
	if( hb_xvmEnumPrev() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00004: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 281 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 30, 2 );
	hb_xvmSFrame( symbols + 30 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 6 );
	hb_xvmPopStatic( 1 );
	/* *** END PROC *** */
   } while( 0 );
}

