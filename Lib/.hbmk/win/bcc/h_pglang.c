/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_pglang.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( INITPGMESSAGES );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( SET );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_PGLANG )
{ "INITPGMESSAGES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITPGMESSAGES )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_PGLANG, "h_pglang.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_PGLANG
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_PGLANG )
   #include "hbiniseg.h"
#endif

HB_FUNC( INITPGMESSAGES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 63 );
	hb_xvmPushStringConst( "Apply", 5 );
	hb_xvmPushStringConst( "Help", 4 );
	hb_xvmPushStringConst( "Cancel", 6 );
	hb_xvmPushStringConst( "OK", 2 );
	hb_xvmPushStringConst( "Save", 4 );
	hb_xvmArrayGen( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 436L ) ) break;
	hb_xvmSetLine( 76 );
	hb_xvmPushStringConst( "Property Item type: ", 20 );
	hb_xvmPushStringConst( " wrong defined.", 15 );
	hb_xvmPushStringConst( "Property Item ID double defined.", 32 );
	hb_xvmPushStringConst( "Property Value for ", 19 );
	hb_xvmPushStringConst( "Property InputMask for ", 23 );
	hb_xvmPushStringConst( "Property Data for ", 18 );
	hb_xvmPushStringConst( "Item is not type of Category!", 29 );
	hb_xvmPushStringConst( "Category ", 9 );
	hb_xvmPushStringConst( " not found!", 11 );
	hb_xvmPushStringConst( "Property Item ID ", 17 );
	hb_xvmPushStringConst( " double defined.", 16 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Not added Item", 15 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Invalid Entry", 13 );
	hb_xvmArrayGen( 12 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 437L ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushStringConst( "Are you sure \?", 14 );
	hb_xvmPushStringConst( "No File to save", 15 );
	hb_xvmPushStringConst( "Error", 5 );
	hb_xvmPushStringConst( "Warning", 7 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 438L ) ) break;
	hb_xvmSetLine( 80 );
	hb_xvmPushStringConst( "UTF-8", 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 439L ) ) break;
	hb_xvmSetLine( 86 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 270L ) ) break;
	hb_xvmPushStringConst( "FI", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 87 );
	hb_xvmPushStringConst( "FI", 2 );
	hb_xvmPopLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00002: ;
	hb_xvmSetLine( 94 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "CS", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 105 );
	hb_xvmPushStringConst( "Apply", 5 );
	hb_xvmPushStringConst( "Help", 4 );
	hb_xvmPushStringConst( "Cancel", 6 );
	hb_xvmPushStringConst( "OK", 2 );
	hb_xvmPushStringConst( "Save", 4 );
	hb_xvmArrayGen( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 436L ) ) break;
	goto lab00005;
lab00003: ;
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "HR", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 158 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "EU", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 191 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "FR", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 224 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "DE", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 257 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "IT", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 290 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "PL", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 301 );
	hb_xvmPushStringConst( "Akceptuj", 8 );
	hb_xvmPushStringConst( "Pomoc", 5 );
	hb_xvmPushStringConst( "Rezygnuj", 8 );
	hb_xvmPushStringConst( "OK", 2 );
	hb_xvmPushStringConst( "Zapisz", 6 );
	hb_xvmArrayGen( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 436L ) ) break;
	hb_xvmSetLine( 314 );
	hb_xvmPushStringConst( "Property Item typu: ", 20 );
	hb_xvmPushStringConst( " B\xB3\xEA" "dnie zdefiniowana.", 22 );
	hb_xvmPushStringConst( "Property Item ID podw\xF3" "jnie zdefiniowana", 39 );
	hb_xvmPushStringConst( "Property warto\x9C\xE6 dla ", 21 );
	hb_xvmPushStringConst( "Property InputMask dla ", 23 );
	hb_xvmPushStringConst( "Property Data dla ", 18 );
	hb_xvmPushStringConst( "Item nie jest typu Category!", 28 );
	hb_xvmPushStringConst( "Category ", 9 );
	hb_xvmPushStringConst( " nie znaleziono!", 16 );
	hb_xvmPushStringConst( "Property Item ID ", 17 );
	hb_xvmPushStringConst( " podw\xF3" "jnie zdefiniowano.", 24 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Item nie zosta\xB3 dodany", 23 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Invalid Entry", 13 );
	hb_xvmArrayGen( 12 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 437L ) ) break;
	hb_xvmSetLine( 317 );
	hb_xvmPushStringConst( "Czy jeste\x9C pewny \?", 18 );
	hb_xvmPushStringConst( "Nie zdefiniowano zbioru do zapisu", 33 );
	hb_xvmPushStringConst( "B\xB3\xB9" "d", 4 );
	hb_xvmPushStringConst( "Ostrzerzenie", 12 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 438L ) ) break;
	hb_xvmSetLine( 319 );
	hb_xvmPushStringConst( "ISO-8859-2", 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 439L ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 322 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "PT", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 355 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "RU", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 388 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "UK", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "UA", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 421 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "ES", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 454 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "FI", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 487 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "NL", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 520 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SL", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 553 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SK", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 586 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "HU", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 619 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "EL", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 652 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "BG", 2 );
	if( hb_xvmExactlyEqual() ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 689 );
	/* *** END PROC *** */
   } while( 0 );
}

