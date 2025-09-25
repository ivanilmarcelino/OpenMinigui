/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_pager.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _BEGINPAGER );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( _DEFINESPLITBOX );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITPAGER );
HB_FUNC_EXTERN( _GETID );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( SETBKCOLORPAGER );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC( _ENDPAGER );
HB_FUNC_EXTERN( _ENDSPLITBOX );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_PAGER )
{ "_BEGINPAGER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINPAGER )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_DEFINESPLITBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINESPLITBOX )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITPAGER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITPAGER )}, NULL },
{ "_GETID", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETID )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "SETBKCOLORPAGER", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETBKCOLORPAGER )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "_ENDPAGER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDPAGER )}, NULL },
{ "_ENDSPLITBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDSPLITBOX )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_PAGER, "h_pager.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_PAGER
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_PAGER )
   #include "hbiniseg.h"
#endif

HB_FUNC( _BEGINPAGER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 10 );
	hb_xvmSetLine( 58 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 425L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 59 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "DEFINE PAGER Structures can't be nested.", 40 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 62 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 63 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "PAGERBOX can't be defined inside Tab control.", 45 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 66 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 67 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
lab00003: ;
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 73 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "PAGERBOX Can't Be Defined inside SplitChild Windows.", 52 );
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 86 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 25L ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 425L ) ) break;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 94 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 426L ) ) break;
	hb_xvmSetLine( 96 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 14 );
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00006: ;
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00007: ;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 110 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 115 );
	hb_xvmPushStringConst( "PAGER", 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 116 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 117 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 118 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 119 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 120 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 121 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 122 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 125 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 126 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 128 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 131 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 132 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 134 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 135 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 138 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 139 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 140 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 141 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 142 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 144 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 146 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 148 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 149 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 150 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 151 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 152 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 153 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 160 );
	hb_xvmPushLocal( 13 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDPAGER )
{
   do {
	hb_xvmSetLine( 166 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 425L ) ) break;
	hb_xvmSetLine( 167 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 25L ) ) break;
	hb_xvmSetLine( 168 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 170 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

