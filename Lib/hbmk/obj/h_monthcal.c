/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_monthcal.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEMONTHCAL );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( INITEXCOMMONCONTROLS );
HB_FUNC( INITDIALOGMONTHCALENDAR );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITMONTHCAL );
HB_FUNC_EXTERN( SETMONTHCALVALUE );
HB_FUNC_EXTERN( YEAR );
HB_FUNC_EXTERN( MONTH );
HB_FUNC_EXTERN( DAY );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( ISVISTAORLATER );
HB_FUNC( ADDMONTHCALBOLDDAY );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( SETWINDOWTHEME );
HB_FUNC_EXTERN( SETPOSMONTHCAL );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( LEN );
HB_FUNC( OMONTHCALEVENTS );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( SETFOCUS );
HB_FUNC_EXTERN( VIRTUALCHILDCONTROLFOCUSPROCESS );
HB_FUNC_EXTERN( _DOCONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC( SETDAYSTATE );
HB_FUNC_EXTERN( HB_AINS );
HB_FUNC( DELMONTHCALBOLDDAY );
HB_FUNC_EXTERN( HB_ADEL );
HB_FUNC( ISMONTHCALBOLDDAY );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( GETMONTHRANGE );
HB_FUNC_EXTERN( AFILL );
HB_FUNC_EXTERN( EOM );
HB_FUNC_EXTERN( C_SETDAYSTATE );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_MONTHCAL )
{ "_DEFINEMONTHCAL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEMONTHCAL )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "INITEXCOMMONCONTROLS", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITEXCOMMONCONTROLS )}, NULL },
{ "INITDIALOGMONTHCALENDAR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGMONTHCALENDAR )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITMONTHCAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITMONTHCAL )}, NULL },
{ "SETMONTHCALVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETMONTHCALVALUE )}, NULL },
{ "YEAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( YEAR )}, NULL },
{ "MONTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( MONTH )}, NULL },
{ "DAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( DAY )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "ISVISTAORLATER", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISVISTAORLATER )}, NULL },
{ "ADDMONTHCALBOLDDAY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ADDMONTHCALBOLDDAY )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "SETWINDOWTHEME", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTHEME )}, NULL },
{ "SETPOSMONTHCAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOSMONTHCAL )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "OMONTHCALEVENTS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( OMONTHCALEVENTS )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETFOCUS )}, NULL },
{ "VIRTUALCHILDCONTROLFOCUSPROCESS", {HB_FS_PUBLIC}, {HB_FUNCNAME( VIRTUALCHILDCONTROLFOCUSPROCESS )}, NULL },
{ "_DOCONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DOCONTROLEVENTPROCEDURE )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "SETDAYSTATE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETDAYSTATE )}, NULL },
{ "HB_AINS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_AINS )}, NULL },
{ "DELMONTHCALBOLDDAY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DELMONTHCALBOLDDAY )}, NULL },
{ "HB_ADEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ADEL )}, NULL },
{ "ISMONTHCALBOLDDAY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ISMONTHCALBOLDDAY )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "GETMONTHRANGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETMONTHRANGE )}, NULL },
{ "AFILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFILL )}, NULL },
{ "EOM", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOM )}, NULL },
{ "C_SETDAYSTATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( C_SETDAYSTATE )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_MONTHCAL, "h_monthcal.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_MONTHCAL
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_MONTHCAL )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEMONTHCAL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 32 );
	hb_xvmSetLine( 59 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 65 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 40 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 41 );
	hb_xvmSetLine( 68 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 41 );
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 71 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 30 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 29 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushLocalByRef( 21 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 83 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00002: ;
	hb_xvmSetLine( 84 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00004;
lab00003: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00004: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 89 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 90 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 91 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 92 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00006: ;
	hb_xvmSetLine( 94 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 39 );
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmPushLocal( 39 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 2 );
lab00008: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00009: ;
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00010: ;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushLocal( 39 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 9 );
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
lab00011: ;
	hb_xvmSetLine( 108 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 35 );
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 111 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 113 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 115 );
	hb_xvmLocalSetInt( 38, 1082130433L );
	hb_xvmSetLine( 117 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 118 );
	hb_xvmPushLocalByRef( 38 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00012: ;
	hb_xvmSetLine( 121 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 122 );
	hb_xvmPushLocalByRef( 38 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00013: ;
	hb_xvmSetLine( 125 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 126 );
	if( hb_xvmLocalAddInt( 38, 16 ) ) break;
lab00014: ;
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 130 );
	if( hb_xvmLocalAddInt( 38, 8 ) ) break;
lab00015: ;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 134 );
	if( hb_xvmLocalAddInt( 38, 4 ) ) break;
lab00016: ;
	hb_xvmSetLine( 137 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 141 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 15, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 143 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushStringConst( "SysMonthCal32", 13 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00021;
lab00017: ;
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 38 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 151 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00018;
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00019;
lab00018: ;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00019: ;
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00021;
lab00020: ;
	hb_xvmSetLine( 166 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 168 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 171 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 17 ) ) break;
	hb_xvmPopLocal( 34 );
lab00021: ;
	hb_xvmSetLine( 175 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00023;
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 180 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00022: ;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 186 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00023: ;
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 198 );
	hb_xvmPushStringConst( "MONTHCAL", 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 199 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 202 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 204 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 206 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 207 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 208 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 209 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 210 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 211 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 212 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 213 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmPushLocal( 28 );
	goto lab00025;
lab00024: ;
	hb_xvmPushStringConst( "", 0 );
lab00025: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 214 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 215 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 216 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 217 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 218 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 219 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 220 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00026;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00027;
lab00026: ;
	hb_xvmPushInteger( -1 );
lab00027: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 221 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00028;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00029;
lab00028: ;
	hb_xvmPushInteger( -1 );
lab00029: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 222 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 223 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 224 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 225 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 227 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 228 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 229 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 230 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00031;
lab00030: ;
	hb_xvmPushLogical( HB_TRUE );
lab00031: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 232 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 233 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 234 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 235 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 236 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 237 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 239 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00033;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 243 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00032;
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00032;
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00032;
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
lab00032: ;
	hb_xvmSetLine( 245 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 247 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
lab00033: ;
	hb_xvmSetLine( 255 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 4106 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00034: ;
	hb_xvmSetLine( 259 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmSetLine( 260 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 4106 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00035: ;
	hb_xvmSetLine( 263 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00036;
	hb_xvmSetLine( 264 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 4106 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00036: ;
	hb_xvmSetLine( 267 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmSetLine( 268 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 4106 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00037: ;
	hb_xvmSetLine( 271 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 4106 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00038: ;
	hb_xvmSetLine( 275 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmSetLine( 276 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 4106 );
	hb_xvmPushInteger( 5 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00039: ;
	hb_xvmSetLine( 279 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
	hb_xvmSetLine( 280 );
	hb_xvmPushSymbol( symbols + 37 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 282 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 41 );
	hb_xvmSetLine( 283 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 40 );
lab00040: ;
	hb_xvmSetLine( 287 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 289 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGMONTHCALENDAR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 295 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 297 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 299 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 300 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00001: ;
	hb_xvmSetLine( 303 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( OMONTHCALEVENTS )
{
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 308 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 313 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 317 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 319 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 323 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 324 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00005;
lab00003: ;
	hb_xvmSetLine( 330 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmPushLocal( 2 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 33L )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 7L )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 8L )
		{
			hb_stackPop();
			goto lab00003;
		}
		hb_stackPop();
	}
lab00005: ;
	hb_xvmSetLine( 334 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ADDMONTHCALBOLDDAY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 340 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 343 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 345 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 6 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 1, 0, 3, 0, 95, 1, 95, 255, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 346 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 347 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 348 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 349 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 350 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 353 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DELMONTHCALBOLDDAY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 359 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 362 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 364 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 365 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 366 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 369 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ISMONTHCALBOLDDAY )
{
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 374 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 377 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 379 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreaterThenInt( 0L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETDAYSTATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 13, 2 );
	hb_xvmSetLine( 389 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 391 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 392 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 393 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 394 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 397 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmMultByInt( 32L ) ) break;
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 398 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 400 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 401 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 403 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 404 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 6 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 1, 0, 7, 0, 95, 1, 95, 255, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 406 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 407 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 408 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 409 );
	hb_xvmLocalSetInt( 14, 0L );
	hb_xvmSetLine( 410 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 411 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 15 );
lab00002: ;
	hb_xvmSetLine( 413 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 414 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 415 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmMultByInt( 32L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 416 );
	if( hb_xvmLocalInc( 13 ) ) break;
	hb_xvmSetLine( 417 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 420 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	goto lab00002;
lab00003: ;
	hb_xvmSetLine( 422 );
	if( hb_xvmLocalInc( 14 ) ) break;
	hb_xvmSetLine( 423 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmInc() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00002;
lab00004: ;
	hb_xvmSetLine( 428 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 430 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

