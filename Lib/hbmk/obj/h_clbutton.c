/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_clbutton.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC_INIT( _INITCLBUTTON );
HB_FUNC_EXTERN( INSTALLEVENTHANDLER );
HB_FUNC_EXTERN( INSTALLMETHODHANDLER );
HB_FUNC_EXTERN( INSTALLPROPERTYHANDLER );
HB_FUNC( _DEFINECLBUTTON );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _GETID );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC( INITCLBUTTON );
HB_FUNC( CLBUTTON_SETNOTE );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC( CLBUTTON_SETIMAGE );
HB_FUNC( RELEASECLBUTTONIMAGELIST );
HB_FUNC_EXTERN( GETCONTROLTYPE );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC_EXTERN( IMAGELIST_DESTROY );
HB_FUNC( CLBUTTONEVENTHANDLER );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( HIWORD );
HB_FUNC_EXTERN( _DOCONTROLEVENTPROCEDURE );
HB_FUNC( CLBUTTON_SETSHIELD );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC( CLBUTTONSETFOCUS );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( LOWORD );
HB_FUNC_EXTERN( SETFOCUS );
HB_FUNC( CLBUTTONENABLE );
HB_FUNC_EXTERN( ENABLEWINDOW );
HB_FUNC( CLBUTTONDISABLE );
HB_FUNC_EXTERN( DISABLEWINDOW );
HB_FUNC( SETCLBUTTONHANDLE );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC( GETCLBUTTONHANDLE );
HB_FUNC( SETCLBUTTONCAPTION );
HB_FUNC_EXTERN( SETWINDOWTEXT );
HB_FUNC( GETCLBUTTONCAPTION );
HB_FUNC_EXTERN( GETWINDOWTEXT );
HB_FUNC( SETCLBUTTONNOTETEXT );
HB_FUNC( GETCLBUTTONNOTETEXT );
HB_FUNC_EXTERN( _GETTOOLTIP );
HB_FUNC( SETCLBUTTONPICTURE );
HB_FUNC( GETCLBUTTONPICTURE );
HB_FUNC_EXTERN( _GETPICTURE );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_CLBUTTON )
{ "_INITCLBUTTON$", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( _INITCLBUTTON )}, NULL },
{ "INSTALLEVENTHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLEVENTHANDLER )}, NULL },
{ "INSTALLMETHODHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLMETHODHANDLER )}, NULL },
{ "INSTALLPROPERTYHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLPROPERTYHANDLER )}, NULL },
{ "_DEFINECLBUTTON", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINECLBUTTON )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_GETID", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETID )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITCLBUTTON", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITCLBUTTON )}, NULL },
{ "CLBUTTON_SETNOTE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLBUTTON_SETNOTE )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "CLBUTTON_SETIMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLBUTTON_SETIMAGE )}, NULL },
{ "RELEASECLBUTTONIMAGELIST", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RELEASECLBUTTONIMAGELIST )}, NULL },
{ "GETCONTROLTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLTYPE )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "IMAGELIST_DESTROY", {HB_FS_PUBLIC}, {HB_FUNCNAME( IMAGELIST_DESTROY )}, NULL },
{ "CLBUTTONEVENTHANDLER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLBUTTONEVENTHANDLER )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "HIWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HIWORD )}, NULL },
{ "_DOCONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DOCONTROLEVENTPROCEDURE )}, NULL },
{ "CLBUTTON_SETSHIELD", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLBUTTON_SETSHIELD )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "CLBUTTONSETFOCUS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLBUTTONSETFOCUS )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "LOWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOWORD )}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETFOCUS )}, NULL },
{ "CLBUTTONENABLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLBUTTONENABLE )}, NULL },
{ "ENABLEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ENABLEWINDOW )}, NULL },
{ "CLBUTTONDISABLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLBUTTONDISABLE )}, NULL },
{ "DISABLEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISABLEWINDOW )}, NULL },
{ "SETCLBUTTONHANDLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETCLBUTTONHANDLE )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "GETCLBUTTONHANDLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETCLBUTTONHANDLE )}, NULL },
{ "SETCLBUTTONCAPTION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETCLBUTTONCAPTION )}, NULL },
{ "SETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTEXT )}, NULL },
{ "GETCLBUTTONCAPTION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETCLBUTTONCAPTION )}, NULL },
{ "GETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWTEXT )}, NULL },
{ "SETCLBUTTONNOTETEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETCLBUTTONNOTETEXT )}, NULL },
{ "GETCLBUTTONNOTETEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETCLBUTTONNOTETEXT )}, NULL },
{ "_GETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETTOOLTIP )}, NULL },
{ "SETCLBUTTONPICTURE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETCLBUTTONPICTURE )}, NULL },
{ "GETCLBUTTONPICTURE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETCLBUTTONPICTURE )}, NULL },
{ "_GETPICTURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETPICTURE )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_CLBUTTON, "h_clbutton.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_CLBUTTON
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_CLBUTTON )
   #include "hbiniseg.h"
#endif

HB_FUNC_INIT( _INITCLBUTTON )
{
   do {
	hb_xvmSetLine( 15 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "CLButtonEventHandler", 20 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 16 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "Release", 7 );
	hb_xvmPushStringConst( "ReleaseCLButtonImageList", 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 17 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "SetShield", 9 );
	hb_xvmPushStringConst( "CLButton_SetShield", 18 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 18 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	hb_xvmPushStringConst( "CLButtonSetFocus", 16 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 19 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "Enable", 6 );
	hb_xvmPushStringConst( "CLButtonEnable", 14 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 20 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "Disable", 7 );
	hb_xvmPushStringConst( "CLButtonDisable", 15 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 21 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Handle", 6 );
	hb_xvmPushStringConst( "SetCLButtonHandle", 17 );
	hb_xvmPushStringConst( "GetCLButtonHandle", 17 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 22 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Caption", 7 );
	hb_xvmPushStringConst( "SetCLButtonCaption", 18 );
	hb_xvmPushStringConst( "GetCLButtonCaption", 18 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 23 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "NoteText", 8 );
	hb_xvmPushStringConst( "SetCLButtonNoteText", 19 );
	hb_xvmPushStringConst( "GetCLButtonNoteText", 19 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 24 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Picture", 7 );
	hb_xvmPushStringConst( "SetCLButtonPicture", 18 );
	hb_xvmPushStringConst( "GetCLButtonPicture", 18 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 26 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINECLBUTTON )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 11 );
	hb_xvmSetLine( 36 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 37 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 7 );
lab00001: ;
	hb_xvmSetLine( 41 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 42 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 43 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 44 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
lab00002: ;
	hb_xvmSetLine( 47 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 48 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 51 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 52 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00004: ;
	hb_xvmSetLine( 55 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 56 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Already defined.", 17 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 59 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushInteger( 180 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 60 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushInteger( 60 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 62 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 14 );
	hb_xvmSetLine( 64 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 65 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 67 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 8 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 81 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 91 );
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 92 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 93 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 94 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 95 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 96 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 97 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 98 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 99 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 100 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 101 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 102 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 103 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 104 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 107 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 110 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 111 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 112 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 113 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00008;
lab00007: ;
	hb_xvmPushInteger( -1 );
lab00008: ;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 114 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00010;
lab00009: ;
	hb_xvmPushInteger( -1 );
lab00010: ;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 115 );
	hb_xvmPushStringConst( "Arrow", 5 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 116 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 117 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 118 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 119 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 120 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 121 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 122 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 125 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 126 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 128 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 132 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 133 );
	hb_xvmPushSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00011: ;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 137 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayPop() ) break;
lab00012: ;
	hb_xvmSetLine( 141 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RELEASECLBUTTONIMAGELIST )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 148 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 150 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 153 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 156 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 160 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00003: ;
	hb_xvmSetLine( 164 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CLBUTTONEVENTHANDLER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 171 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 176 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 273L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 180 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 183 );
	hb_xvmLocalSetInt( 5, 0L );
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 191 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CLBUTTON_SETSHIELD )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 199 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 203 );
	hb_xvmPushStringConst( "Shield", 6 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 5644 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLongLong( HB_LL( 4294967295 ) );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 207 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 211 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 215 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CLBUTTONSETFOCUS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 2 );
	hb_xvmSetLine( 227 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 229 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 232 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 233 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 234 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 235 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 236 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 237 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 244 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushInteger( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	hb_xvmSetLine( 234 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
lab00003: ;
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 244 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 246 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00005: ;
	hb_xvmSetLine( 250 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CLBUTTONENABLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 258 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 260 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 264 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 268 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CLBUTTONDISABLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 274 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 276 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 278 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 282 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 286 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETCLBUTTONHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 292 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 294 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushStringConst( "This Property is Read Only!", 27 );
	hb_xvmPushStringConst( "Warning", 7 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 296 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 300 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 304 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETCLBUTTONHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 309 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 311 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 313 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	hb_xvmSetLine( 315 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 319 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 323 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETCLBUTTONCAPTION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 328 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 330 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 332 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	hb_xvmSetLine( 334 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 338 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 342 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETCLBUTTONCAPTION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 347 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 349 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 351 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	hb_xvmSetLine( 353 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 357 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 361 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETCLBUTTONNOTETEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 368 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 370 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 372 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 374 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 376 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 378 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 382 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 386 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETCLBUTTONNOTETEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 391 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 393 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 395 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	hb_xvmSetLine( 397 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 401 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 405 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETCLBUTTONPICTURE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 412 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 414 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 416 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 418 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 420 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 421 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 424 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 426 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 430 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00003: ;
	hb_xvmSetLine( 434 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETCLBUTTONPICTURE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 441 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "CLBUTTON", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 443 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	hb_xvmSetLine( 445 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 449 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 453 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

#line 460 "h_clbutton.prg"

#define BS_COMMANDLINK     0x0000000E
#define BS_DEFCOMMANDLINK  0x0000000F

#include <mgdefs.h>
#include <commctrl.h>
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
// Button Class Name
#define WC_BUTTON              "Button"
#endif

#ifdef UNICODE
LPWSTR AnsiToWide( LPCSTR );
#endif

HB_FUNC( INITCLBUTTON )
{
#ifndef UNICODE
   LPCSTR lpWindowName = hb_parc( 4 );
#else
   LPWSTR lpWindowName = AnsiToWide( ( char * ) hb_parc( 4 ) );
#endif

   DWORD Style = hb_parl( 5 ) ? BS_DEFCOMMANDLINK : BS_COMMANDLINK;

   hmg_ret_raw_HWND
      ( 
         CreateWindow
            (
         WC_BUTTON,
         lpWindowName,
         Style | WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | BS_PUSHBUTTON | WS_VISIBLE,
         hb_parni( 3 ),
         hb_parni( 2 ),
         hb_parni( 6 ),
         hb_parni( 7 ),
         hmg_par_raw_HWND( 1 ),
         hmg_par_raw_HMENU( 8 ),
         GetModuleHandle( NULL ),
         NULL
            )
      );
}

#ifndef BCM_SETNOTE
#define BCM_SETNOTE  0x00001609
#endif

HB_FUNC( CLBUTTON_SETNOTE )
{
   if( HB_ISCHAR( 2 ) )
   {
      LPSTR  szText        = ( LPSTR ) hb_parc( 2 );
      int    nConvertedLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, szText, -1, NULL, 0 );
      LPWSTR lpwText       = ( LPWSTR ) hb_xgrab( nConvertedLen * 2 + 1 );

      MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, szText, -1, lpwText, nConvertedLen );

      SendMessage( hmg_par_raw_HWND( 1 ), BCM_SETNOTE, 0, ( LPARAM ) lpwText );
      hb_xfree( lpwText );
   }
}

#ifndef BCM_FIRST
#define BCM_FIRST         0x1600
#define BCM_SETIMAGELIST  ( BCM_FIRST + 0x0002 )
#endif

#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 ) || ( defined ( __MINGW32__ ) && defined ( __MINGW32_VERSION ) )
typedef struct
{
   HIMAGELIST himl;
   RECT       margin;
   UINT       uAlign;
} BUTTON_IMAGELIST, * PBUTTON_IMAGELIST;
#endif

HB_FUNC( CLBUTTON_SETIMAGE )
{
   HIMAGELIST       himl;
   BUTTON_IMAGELIST bi;
#ifndef UNICODE
   LPCTSTR lpImageName = hb_parc( 2 );
#else
   LPWSTR  lpImageName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif

   himl = ImageList_LoadImage
          (
      GetModuleHandle( NULL ),
      lpImageName,
      0,
      6,
      CLR_DEFAULT,
      IMAGE_BITMAP,
      LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT
          );

   if( himl == NULL )
      himl = ImageList_LoadImage
             (
         GetModuleHandle( NULL ),
         lpImageName,
         0,
         6,
         CLR_DEFAULT,
         IMAGE_BITMAP,
         LR_LOADFROMFILE | LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT
             );

   bi.himl          = himl;
   bi.margin.left   = 10;
   bi.margin.top    = 10;
   bi.margin.bottom = 10;
   bi.margin.right  = 10;
   bi.uAlign        = 4;

   SendMessage( hmg_par_raw_HWND( 1 ), ( UINT ) BCM_SETIMAGELIST, ( WPARAM ) 0, ( LPARAM ) &bi );

   hmg_ret_raw_HANDLE( himl );
}

