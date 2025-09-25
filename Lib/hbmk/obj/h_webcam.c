/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_webcam.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC_INIT( _INITWEBCAM );
HB_FUNC_EXTERN( INSTALLMETHODHANDLER );
HB_FUNC( _DEFINEWEBCAM );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC( CAP_CREATECAPTUREWINDOW );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC( _STARTWEBCAM );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC( CAP_DRIVERCONNECT );
HB_FUNC_EXTERN( DOEVENTS );
HB_FUNC_EXTERN( _GETCONTROLWIDTH );
HB_FUNC_EXTERN( _GETCONTROLHEIGHT );
HB_FUNC( CAP_SETVIDEOFORMAT );
HB_FUNC_EXTERN( MIN );
HB_FUNC( CAP_PREVIEWSCALE );
HB_FUNC( CAP_PREVIEWRATE );
HB_FUNC_EXTERN( GETCONTROLVALUE );
HB_FUNC( CAP_PREVIEW );
HB_FUNC_EXTERN( DESTROYWINDOW );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC( _RELEASEWEBCAM );
HB_FUNC_EXTERN( GETCONTROLTYPE );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC( CAP_DRIVERDISCONNECT );
HB_FUNC_EXTERN( _ERASECONTROL );
HB_FUNC_EXTERN( GETFORMINDEX );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_WEBCAM )
{ "_INITWEBCAM$", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( _INITWEBCAM )}, NULL },
{ "INSTALLMETHODHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLMETHODHANDLER )}, NULL },
{ "_DEFINEWEBCAM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEWEBCAM )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "CAP_CREATECAPTUREWINDOW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_CREATECAPTUREWINDOW )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_STARTWEBCAM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _STARTWEBCAM )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "CAP_DRIVERCONNECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_DRIVERCONNECT )}, NULL },
{ "DOEVENTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOEVENTS )}, NULL },
{ "_GETCONTROLWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLWIDTH )}, NULL },
{ "_GETCONTROLHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLHEIGHT )}, NULL },
{ "CAP_SETVIDEOFORMAT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_SETVIDEOFORMAT )}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "CAP_PREVIEWSCALE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_PREVIEWSCALE )}, NULL },
{ "CAP_PREVIEWRATE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_PREVIEWRATE )}, NULL },
{ "GETCONTROLVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLVALUE )}, NULL },
{ "CAP_PREVIEW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_PREVIEW )}, NULL },
{ "DESTROYWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( DESTROYWINDOW )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "_RELEASEWEBCAM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _RELEASEWEBCAM )}, NULL },
{ "GETCONTROLTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLTYPE )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "CAP_DRIVERDISCONNECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_DRIVERDISCONNECT )}, NULL },
{ "_ERASECONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ERASECONTROL )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_WEBCAM, "h_webcam.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_WEBCAM
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_WEBCAM )
   #include "hbiniseg.h"
#endif

HB_FUNC_INIT( _INITWEBCAM )
{
   do {
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "Start", 5 );
	hb_xvmPushStringConst( "_StartWebCam", 12 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "Release", 7 );
	hb_xvmPushStringConst( "_ReleaseWebCam", 14 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 84 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINEWEBCAM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 10 );
	hb_xvmSetLine( 126 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 320 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 240 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 128 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushInteger( 30 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 130 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 131 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 134 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 135 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 137 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00002: ;
	hb_xvmSetLine( 140 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 141 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 6 );
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
lab00004: ;
	hb_xvmSetLine( 148 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 13 );
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 156 );
	hb_xvmCopyLocals( 2, 12 );
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushStringConst( "WebCam", 6 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1342177280 );
#else
	hb_xvmPushLong( 1342177280L );
#endif
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 8 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 162 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 163 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 166 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00006: ;
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushStringConst( "WEBCAM", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 171 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 172 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 174 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 179 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 180 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 188 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 190 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00008;
lab00007: ;
	hb_xvmPushInteger( -1 );
lab00008: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00010;
lab00009: ;
	hb_xvmPushInteger( -1 );
lab00010: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 194 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 197 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 199 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 204 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 207 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 209 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 211 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 213 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushStringConst( "Webcam service is unavailable!", 30 );
	hb_xvmPushStringConst( "Alert", 5 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00011: ;
	hb_xvmSetLine( 217 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _STARTWEBCAM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 2 );
	hb_xvmSetLine( 249 );
	hb_xvmLocalSetInt( 6, 1L );
	hb_xvmSetLine( 252 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 255 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushLocal( 6 );
	if( hb_xvmLocalInc( 6 ) ) break;
	if( hb_xvmLessThenIntIs( 3L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00002: ;
	hb_xvmSetLine( 259 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 260 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 261 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 263 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 320 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 240 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 267 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
lab00003: ;
	hb_xvmPopLocal( 7 );
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 270 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 273 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 275 );
	hb_xvmPushLocal( 7 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _RELEASEWEBCAM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 305 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "WEBCAM", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 307 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 309 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 311 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 313 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 315 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 319 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 323 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00003: ;
	hb_xvmSetLine( 327 );
	/* *** END PROC *** */
   } while( 0 );
}

#line 333 "h_webcam.prg"

#include <mgdefs.h>
#include <vfw.h>

#if defined( __BORLANDC__ )
#pragma warn -use /* unused var */
#pragma warn -eff /* no effect */
#endif

#ifdef UNICODE
LPWSTR AnsiToWide( LPCSTR );
#endif

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_CREATECAPTUREWINDOW )
*------------------------------------------------------------------------------*
*
*  Description:
*     Creates a capture window using the capCreateCaptureWindow function from the VFW API.
*
*  Parameters:
*     1 - lpszWindowName: The name of the capture window (string).
*     2 - dwStyle: The style of the capture window (DWORD).
*     3 - x: The x-coordinate of the window's top-left corner (numeric).
*     4 - y: The y-coordinate of the window's top-left corner (numeric).
*     5 - nWidth: The width of the window (numeric).
*     6 - nHeight: The height of the window (numeric).
*     7 - hWndParent: The handle of the parent window (HWND).
*     8 - nID: The ID of the window (numeric).
*
*  Return Value:
*     The handle of the created capture window (HWND).
*
*  Purpose:
*     This function is a Harbour wrapper for the capCreateCaptureWindow function from the VFW API.
*     It allows Harbour code to create a capture window, which is necessary for capturing video from a webcam.
*     The function takes various parameters that define the window's properties, such as its name, style, position, size, and parent window.
*
*  Notes:
*     - This function directly calls the capCreateCaptureWindow function from the VFW API.
*     - The hmg_ret_raw_HWND macro is used to return the window handle as a raw HWND value.
*
*/
HB_FUNC( CAP_CREATECAPTUREWINDOW )
{
#ifndef UNICODE
   LPCSTR lpszWindowName = hb_parc( 1 );
#else
   LPWSTR lpszWindowName = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif

   hmg_ret_raw_HWND
      (
         capCreateCaptureWindow
            (
         lpszWindowName,
         hmg_par_DWORD( 2 ),
         hb_parni( 3 ),
         hb_parni( 4 ),
         hb_parni( 5 ),
         hb_parni( 6 ),
         hmg_par_raw_HWND( 7 ),
         hb_parni( 8 )
            )
      );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_DRIVERCONNECT )
*------------------------------------------------------------------------------*
*
*  Description:
*     Connects to a capture driver using the capDriverConnect function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*     2 - i: The index of the capture driver to connect to (numeric).
*
*  Return Value:
*     .T. if the connection was successful, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capDriverConnect function from the VFW API.
*     It allows Harbour code to connect to a specific capture driver, which is necessary for accessing the webcam.
*     The function takes the handle of the capture window and the index of the driver to connect to.
*
*  Notes:
*     - This function directly calls the capDriverConnect function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_DRIVERCONNECT )
{
   hb_retl( capDriverConnect( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_DRIVERDISCONNECT )
*------------------------------------------------------------------------------*
*
*  Description:
*     Disconnects from a capture driver using the capDriverDisconnect function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*
*  Return Value:
*     .T. if the disconnection was successful, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capDriverDisconnect function from the VFW API.
*     It allows Harbour code to disconnect from a capture driver, releasing the webcam resources.
*     The function takes the handle of the capture window.
*
*  Notes:
*     - This function directly calls the capDriverDisconnect function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_DRIVERDISCONNECT )
{
   hb_retl( capDriverDisconnect( hmg_par_raw_HWND( 1 ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_SETVIDEOFORMAT )
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets the video format for the capture window using the capSetVideoFormat function from the VFW API.
*
*  Parameters:
*     1 - hCapWnd: The handle of the capture window (HWND).
*     2 - nWidth: The desired width of the video (numeric).
*     3 - nHeight: The desired height of the video (numeric).
*
*  Return Value:
*     .T. if the video format was set successfully, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capSetVideoFormat function from the VFW API.
*     It allows Harbour code to set the video format for the capture window, specifying the desired width and height of the video.
*     The function retrieves the current video format, modifies the width and height, and then sets the new format.
*
*  Notes:
*     - This function directly calls the capSetVideoFormat function from the VFW API.
*     - The function initializes a BITMAPINFO structure with the desired width and height.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_SETVIDEOFORMAT )
{
   BITMAPINFO binf;
   HWND hCapWnd = hmg_par_raw_HWND( 1 );

   capGetVideoFormat( hCapWnd, &binf, sizeof( BITMAPINFO ) );

   binf.bmiHeader.biWidth        = hb_parni( 2 );
   binf.bmiHeader.biHeight       = hb_parni( 3 );
   binf.bmiHeader.biPlanes       = 1;
   binf.bmiHeader.biBitCount     = 24;
   binf.bmiHeader.biCompression  = BI_RGB;
   binf.bmiHeader.biSizeImage    = 0;
   binf.bmiHeader.biClrUsed      = 0;
   binf.bmiHeader.biClrImportant = 0;

   hb_retl( capSetVideoFormat( hCapWnd, &binf, sizeof( BITMAPINFO ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_PREVIEWRATE )
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets the preview frame rate for the capture window using the capPreviewRate function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*     2 - wMS: The desired frame rate in milliseconds per frame (WORD).
*
*  Return Value:
*     .T. if the preview rate was set successfully, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capPreviewRate function from the VFW API.
*     It allows Harbour code to set the preview frame rate for the capture window, controlling how often the preview image is updated.
*     The function takes the handle of the capture window and the desired frame rate in milliseconds per frame.
*
*  Notes:
*     - This function directly calls the capPreviewRate function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_PREVIEWRATE )
{
   hb_retl( capPreviewRate( hmg_par_raw_HWND( 1 ), hmg_par_WORD( 2 ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_PREVIEWSCALE )
*------------------------------------------------------------------------------*
*
*  Description:
*     Enables or disables preview scaling for the capture window using the capPreviewScale function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*     2 - fScale: .T. to enable preview scaling, .F. to disable it (logical).
*
*  Return Value:
*     .T. if the preview scaling was set successfully, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capPreviewScale function from the VFW API.
*     It allows Harbour code to enable or disable preview scaling for the capture window.
*     When preview scaling is enabled, the preview image is scaled to fit the window.
*     The function takes the handle of the capture window and a logical value indicating whether to enable or disable scaling.
*
*  Notes:
*     - This function directly calls the capPreviewScale function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_PREVIEWSCALE )
{
   hb_retl( capPreviewScale( hmg_par_raw_HWND( 1 ), hb_parl( 2 ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_PREVIEW )
*------------------------------------------------------------------------------*
*
*  Description:
*     Enables or disables preview mode for the capture window using the capPreview function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*     2 - fPreview: .T. to enable preview mode, .F. to disable it (logical).
*
*  Return Value:
*     .T. if the preview mode was set successfully, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capPreview function from the VFW API.
*     It allows Harbour code to enable or disable preview mode for the capture window.
*     When preview mode is enabled, the capture window displays a live preview of the video being captured.
*     The function takes the handle of the capture window and a logical value indicating whether to enable or disable preview mode.
*
*  Notes:
*     - This function directly calls the capPreview function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_PREVIEW )
{
   hb_retl( capPreview( hmg_par_raw_HWND( 1 ), hb_parl( 2 ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_EDITCOPY )
*------------------------------------------------------------------------------*
*
*  Description:
*     Copies the current frame from the capture window to the clipboard using the capEditCopy function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*
*  Return Value:
*     .T. if the frame was copied successfully, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capEditCopy function from the VFW API.
*     It allows Harbour code to copy the current frame from the capture window to the clipboard,
*     allowing the user to paste the frame into other applications.
*     The function takes the handle of the capture window.
*
*  Notes:
*     - This function directly calls the capEditCopy function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_EDITCOPY )
{
   hb_retl( capEditCopy( hmg_par_raw_HWND( 1 ) ) );
}

