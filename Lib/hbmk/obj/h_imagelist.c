/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_imagelist.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEIMAGELIST );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( _GETID );
HB_FUNC_EXTERN( INITIMAGELIST );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( IL_ADD );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( IL_ADDMASKED );
HB_FUNC( _ADDIMAGETOIMAGELIST );
HB_FUNC_EXTERN( _GETCONTROLWIDTH );
HB_FUNC_EXTERN( _GETCONTROLHEIGHT );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC( _ADDIMAGEMASKEDTOIMAGELIST );
HB_FUNC( _IMAGELISTSETBKCOLOR );
HB_FUNC_EXTERN( IL_SETBKCOLOR );
HB_FUNC( _ERASEIMAGE );
HB_FUNC_EXTERN( IL_ERASEIMAGE );
HB_FUNC( _BEGINDRAGIMAGE );
HB_FUNC_EXTERN( IL_BEGINDRAG );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_IMAGELIST )
{ "_DEFINEIMAGELIST", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEIMAGELIST )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "_GETID", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETID )}, NULL },
{ "INITIMAGELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITIMAGELIST )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "IL_ADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( IL_ADD )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "IL_ADDMASKED", {HB_FS_PUBLIC}, {HB_FUNCNAME( IL_ADDMASKED )}, NULL },
{ "_ADDIMAGETOIMAGELIST", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ADDIMAGETOIMAGELIST )}, NULL },
{ "_GETCONTROLWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLWIDTH )}, NULL },
{ "_GETCONTROLHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLHEIGHT )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "_ADDIMAGEMASKEDTOIMAGELIST", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ADDIMAGEMASKEDTOIMAGELIST )}, NULL },
{ "_IMAGELISTSETBKCOLOR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _IMAGELISTSETBKCOLOR )}, NULL },
{ "IL_SETBKCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( IL_SETBKCOLOR )}, NULL },
{ "_ERASEIMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ERASEIMAGE )}, NULL },
{ "IL_ERASEIMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( IL_ERASEIMAGE )}, NULL },
{ "_BEGINDRAGIMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINDRAGIMAGE )}, NULL },
{ "IL_BEGINDRAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( IL_BEGINDRAG )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_IMAGELIST, "h_imagelist.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_IMAGELIST
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_IMAGELIST )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEIMAGELIST )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 9 );
	hb_xvmSetLine( 60 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushInteger( 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 61 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 62 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 63 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 64 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 65 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 66 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 68 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 69 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 5 );
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
lab00003: ;
	hb_xvmSetLine( 80 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 11 );
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 83 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushInteger( 10 );
	goto lab00005;
lab00004: ;
	hb_xvmPushLocal( 14 );
lab00005: ;
	hb_xvmPopLocal( 8 );
lab00006: ;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 98 );
	hb_xvmPushStringConst( "IMAGELIST", 9 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 99 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 100 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 102 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 103 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 104 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 107 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 110 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 111 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 112 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 113 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 114 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 115 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 116 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 117 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 118 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 119 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 120 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00008;
lab00007: ;
	hb_xvmPushInteger( -1 );
lab00008: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 121 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00010;
lab00009: ;
	hb_xvmPushInteger( -1 );
lab00010: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 122 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 125 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 126 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 128 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 132 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 134 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 135 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 139 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 140 );
	hb_xvmPushSymbol( symbols + 14 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00011: ;
	hb_xvmSetLine( 143 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 10 );
	goto lab00021;
lab00012: ;
	hb_xvmSetLine( 144 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
	hb_xvmSetLine( 146 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	goto lab00014;
lab00013: ;
	hb_xvmPushStringConst( "", 0 );
lab00014: ;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 17 );
	goto lab00019;
lab00015: ;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 150 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 13 );
	goto lab00017;
lab00016: ;
	hb_xvmSetLine( 152 );
	hb_xvmCopyLocals( 7, 13 );
lab00017: ;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 17 );
	goto lab00019;
lab00018: ;
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 17 );
lab00019: ;
	hb_xvmSetLine( 159 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmEqualIntIs( -1L, &fValue ) ) break;
	if( !fValue )
		goto lab00020;
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "Image: ", 7 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not added. Check image size.", 32 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00020: ;
	hb_xvmSetLine( 143 );
	if( hb_xvmLocalIncPush( 10 ) ) break;
lab00021: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 164 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ADDIMAGETOIMAGELIST )
{
   do {
	hb_xvmFrame( 3, 4 );
	hb_xvmSetLine( 171 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 172 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 5 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ADDIMAGEMASKEDTOIMAGELIST )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 4 );
	hb_xvmSetLine( 180 );
	hb_xvmLocalSetInt( 8, 0L );
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 186 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 8 );
lab00001: ;
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 5 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _IMAGELISTSETBKCOLOR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 194 );
	hb_xvmLocalSetInt( 5, 0L );
	hb_xvmSetLine( 196 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 197 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 198 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 5 );
lab00001: ;
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ERASEIMAGE )
{
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 209 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 210 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 212 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINDRAGIMAGE )
{
   do {
	hb_xvmFrame( 2, 5 );
	hb_xvmSetLine( 219 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 220 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 221 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPop( 12L ) ) break;
	hb_xvmSetLine( 222 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 224 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

