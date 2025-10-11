/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "SCRLLBAR.PRG"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( TSBSCRLBAR );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( TCONTROL );
HB_FUNC_STATIC( TSBSCRLBAR_NEW );
HB_FUNC_STATIC( TSBSCRLBAR_WINNEW );
HB_FUNC_EXTERN( GETSCROLLPOS );
HB_FUNC_EXTERN( GETSCRLRANGE );
HB_FUNC_STATIC( TSBSCRLBAR_HANDLEEVENT );
HB_FUNC_STATIC( TSBSCRLBAR_SETMODE );
HB_FUNC_STATIC( TSBSCRLBAR_SETPAGE );
HB_FUNC_EXTERN( SETSCROLLPOS );
HB_FUNC_EXTERN( SETSCROLLRANGE );
HB_FUNC_STATIC( TSBSCRLBAR_MOUSEMOVE );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( NOR );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( SETSCROLLINFO );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_SCRLLBAR )
{ "TSBSCRLBAR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSBSCRLBAR )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "TCONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( TCONTROL )}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMULTICLSDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TSBSCRLBAR_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSBSCRLBAR_NEW )}, NULL },
{ "TSBSCRLBAR_WINNEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSBSCRLBAR_WINNEW )}, NULL },
{ "ADDINLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETSCROLLPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSCROLLPOS )}, NULL },
{ "LISCHILD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LVERTICAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETSCRLRANGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSCRLRANGE )}, NULL },
{ "TSBSCRLBAR_HANDLEEVENT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSBSCRLBAR_HANDLEEVENT )}, NULL },
{ "GETPOS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETRANGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETPOS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGOUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NMAX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGODOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NMIN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGOTOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BGOBOTTOM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BPAGEUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NPGSTEP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BPAGEDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TSBSCRLBAR_SETMODE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSBSCRLBAR_SETMODE )}, NULL },
{ "TSBSCRLBAR_SETPAGE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSBSCRLBAR_SETPAGE )}, NULL },
{ "SETSCROLLPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETSCROLLPOS )}, NULL },
{ "LREDRAW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LSHOWDISABLED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "L32BIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NMIN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NMAX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETSCROLLRANGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETSCROLLRANGE )}, NULL },
{ "BPOS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TSBSCRLBAR_MOUSEMOVE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSBSCRLBAR_MOUSEMOVE )}, NULL },
{ "BTRACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "THUMBPOS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETSYSCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSCOLOR )}, NULL },
{ "_CCAPTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NTOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NLEFT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NBOTTOM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NTOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NRIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NLEFT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NPGSTEP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LVERTICAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LREDRAW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NSTYLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( NOR )}, NULL },
{ "_BGOUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BGODOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BPAGEUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BPAGEDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BPOS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_OWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LISCHILD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LDRAG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LCAPTURED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LUPDATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BWHEN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BVALID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "SETRANGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CHECKDOTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LSHOWDISABLED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_L32BIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETMODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETPAGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GOUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GODOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PAGEUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PAGEDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "THUMBTRACK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HANDLEEVENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SUPER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MOUSEMOVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LDRAG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETSCROLLINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETSCROLLINFO )}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_SCRLLBAR, "SCRLLBAR.PRG", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_SCRLLBAR
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_SCRLLBAR )
   #include "hbiniseg.h"
#endif

HB_FUNC( TSBSCRLBAR )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 93 );
	hb_xvmSetLine( 21 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStaticByRef( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSeqAlways();
	do {
	hb_xvmLocalSetInt( 1, 1L );
	hb_xvmPushSymbol( symbols + 2 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "TSBScrlBar", 10 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 0 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 23 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lVertical", 9 );
	hb_xvmPushStringConst( "lReDraw", 7 );
	hb_xvmPushStringConst( "lIsChild", 8 );
	hb_xvmPushStringConst( "nMin", 4 );
	hb_xvmPushStringConst( "nMax", 4 );
	hb_xvmPushStringConst( "nPgStep", 7 );
	hb_xvmArrayGen( 6 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 24 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bGoUp", 5 );
	hb_xvmPushStringConst( "bGoDown", 7 );
	hb_xvmPushStringConst( "bGoTop", 6 );
	hb_xvmPushStringConst( "bGoBottom", 9 );
	hb_xvmPushStringConst( "bPageUp", 7 );
	hb_xvmPushStringConst( "bPageDown", 9 );
	hb_xvmPushStringConst( "bPos", 4 );
	hb_xvmArrayGen( 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 25 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bTrack", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 26 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "l32Bit", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 27 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lShowDisabled", 13 );
	hb_xvmPushStringConst( "hWnd", 4 );
	hb_xvmPushStringConst( "oWnd", 4 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 28 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lUpdate", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 29 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bWhen", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 30 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bValid", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 33 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "cVarName", 8 );
	hb_xvmPushStringConst( "nMin", 4 );
	hb_xvmPushStringConst( "nMax", 4 );
	hb_xvmPushStringConst( "nPgStep", 7 );
	hb_xvmPushStringConst( "nTop", 4 );
	hb_xvmPushStringConst( "nLeft", 5 );
	hb_xvmPushStringConst( "Cargo", 5 );
	hb_xvmArrayGen( 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 32L ) ) break;
	hb_xvmPushStringConst( "aProperties", 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 37 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 8L ) ) break;
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 41 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WinNew", 6 );
	hb_xvmPushSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 8L ) ) break;
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 44 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetPos", 6 );
	{
		static const HB_BYTE codeblock[ 66 ] = {
			1, 0, 0, 0, 176, 11, 0, 48, 12, 0, 95, 1, 112, 0, 28, 16, 
			48, 13, 0, 48, 14, 0, 95, 1, 112, 0, 112, 0, 25, 9, 48, 13, 
			0, 95, 1, 112, 0, 48, 12, 0, 95, 1, 112, 0, 28, 17, 48, 15, 
			0, 95, 1, 112, 0, 28, 5, 122, 25, 7, 121, 25, 4, 92, 2, 12, 
			2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 47 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetRange", 8 );
	{
		static const HB_BYTE codeblock[ 66 ] = {
			1, 0, 0, 0, 176, 16, 0, 48, 12, 0, 95, 1, 112, 0, 28, 16, 
			48, 13, 0, 48, 14, 0, 95, 1, 112, 0, 112, 0, 25, 9, 48, 13, 
			0, 95, 1, 112, 0, 48, 12, 0, 95, 1, 112, 0, 28, 17, 48, 15, 
			0, 95, 1, 112, 0, 28, 5, 122, 25, 7, 121, 25, 4, 92, 2, 12, 
			2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 49 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "HandleEvent", 11 );
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 56 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GoUp", 4 );
	{
		static const HB_BYTE codeblock[ 67 ] = {
			2, 0, 0, 0, 48, 18, 0, 95, 1, 112, 0, 80, 2, 95, 2, 48, 
			19, 0, 95, 1, 112, 0, 122, 1, 15, 28, 15, 48, 20, 0, 95, 1, 
			173, 2, 0, 95, 2, 112, 1, 73, 48, 21, 0, 95, 1, 112, 0, 100, 
			69, 28, 16, 48, 22, 0, 48, 21, 0, 95, 1, 112, 0, 112, 0, 25, 
			3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 61 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GoDown", 6 );
	{
		static const HB_BYTE codeblock[ 63 ] = {
			2, 0, 0, 0, 48, 18, 0, 95, 1, 112, 0, 80, 2, 95, 2, 48, 
			23, 0, 95, 1, 112, 0, 35, 28, 13, 48, 20, 0, 95, 1, 175, 2, 
			0, 112, 1, 73, 48, 24, 0, 95, 1, 112, 0, 100, 69, 28, 16, 48, 
			22, 0, 48, 24, 0, 95, 1, 112, 0, 112, 0, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 65 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GoTop", 5 );
	{
		static const HB_BYTE codeblock[ 46 ] = {
			1, 0, 0, 0, 48, 20, 0, 95, 1, 48, 25, 0, 95, 1, 112, 0, 
			112, 1, 73, 48, 26, 0, 95, 1, 112, 0, 100, 69, 28, 16, 48, 22, 
			0, 48, 26, 0, 95, 1, 112, 0, 112, 0, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 68 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GoBottom", 8 );
	{
		static const HB_BYTE codeblock[ 46 ] = {
			1, 0, 0, 0, 48, 20, 0, 95, 1, 48, 23, 0, 95, 1, 112, 0, 
			112, 1, 73, 48, 27, 0, 95, 1, 112, 0, 100, 69, 28, 16, 48, 22, 
			0, 48, 27, 0, 95, 1, 112, 0, 112, 0, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 71 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "PageUp", 6 );
	{
		static const HB_BYTE codeblock[ 51 ] = {
			1, 0, 0, 0, 48, 28, 0, 95, 1, 112, 0, 100, 69, 28, 15, 48, 
			22, 0, 48, 28, 0, 95, 1, 112, 0, 112, 0, 73, 48, 20, 0, 95, 
			1, 48, 18, 0, 95, 1, 112, 0, 48, 29, 0, 95, 1, 112, 0, 49, 
			112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 74 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "PageDown", 8 );
	{
		static const HB_BYTE codeblock[ 51 ] = {
			1, 0, 0, 0, 48, 30, 0, 95, 1, 112, 0, 100, 69, 28, 15, 48, 
			22, 0, 48, 30, 0, 95, 1, 112, 0, 112, 0, 73, 48, 20, 0, 95, 
			1, 48, 18, 0, 95, 1, 112, 0, 48, 29, 0, 95, 1, 112, 0, 72, 
			112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 76 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SetMode", 7 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 78 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SetPage", 7 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 84 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SetPos", 6 );
	{
		static const HB_BYTE codeblock[ 89 ] = {
			2, 0, 0, 0, 176, 33, 0, 48, 12, 0, 95, 1, 112, 0, 28, 16, 
			48, 13, 0, 48, 14, 0, 95, 1, 112, 0, 112, 0, 25, 9, 48, 13, 
			0, 95, 1, 112, 0, 48, 12, 0, 95, 1, 112, 0, 28, 17, 48, 15, 
			0, 95, 1, 112, 0, 28, 5, 122, 25, 7, 121, 25, 4, 92, 2, 95, 
			2, 48, 34, 0, 95, 1, 112, 0, 48, 35, 0, 95, 1, 112, 0, 48, 
			36, 0, 95, 1, 112, 0, 12, 6, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 90 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SetRange", 8 );
	{
		static const HB_BYTE codeblock[ 111 ] = {
			3, 0, 0, 0, 48, 37, 0, 95, 1, 95, 2, 112, 1, 73, 48, 38, 
			0, 95, 1, 95, 3, 112, 1, 73, 176, 39, 0, 48, 12, 0, 95, 1, 
			112, 0, 28, 16, 48, 13, 0, 48, 14, 0, 95, 1, 112, 0, 112, 0, 
			25, 9, 48, 13, 0, 95, 1, 112, 0, 48, 12, 0, 95, 1, 112, 0, 
			28, 17, 48, 15, 0, 95, 1, 112, 0, 28, 5, 122, 25, 7, 121, 25, 
			4, 92, 2, 95, 2, 95, 3, 48, 34, 0, 95, 1, 112, 0, 48, 35, 
			0, 95, 1, 112, 0, 48, 36, 0, 95, 1, 112, 0, 12, 7, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 92 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ThumbPos", 8 );
	{
		static const HB_BYTE codeblock[ 33 ] = {
			2, 0, 0, 0, 48, 40, 0, 95, 1, 112, 0, 100, 69, 28, 18, 48, 
			22, 0, 48, 40, 0, 95, 1, 112, 0, 95, 2, 112, 1, 25, 3, 100, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 94 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "MouseMove", 9 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 98 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ThumbTrack", 10 );
	{
		static const HB_BYTE codeblock[ 41 ] = {
			2, 0, 0, 0, 48, 42, 0, 95, 1, 112, 0, 100, 69, 28, 18, 48, 
			22, 0, 48, 42, 0, 95, 1, 112, 0, 95, 2, 112, 1, 25, 11, 48, 
			43, 0, 95, 1, 95, 2, 112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 100 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStaticByRef( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushVParams();
	if( hb_xvmMacroSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSBSCRLBAR_NEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 22 );
	hb_xvmSetLine( 117 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	hb_xvmPopLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 0 );
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 2 );
lab00004: ;
	hb_xvmPopLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushInteger( 0 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 3 );
lab00006: ;
	hb_xvmPopLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushInteger( 0 );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 4 );
lab00008: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushInteger( 1 );
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 5 );
lab00010: ;
	hb_xvmPopLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00012;
lab00011: ;
	hb_xvmPushLocal( 6 );
lab00012: ;
	hb_xvmPopLocal( 6 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushInteger( 16 );
	goto lab00015;
lab00013: ;
	hb_xvmPushInteger( 100 );
	goto lab00015;
lab00014: ;
	hb_xvmPushLocal( 8 );
lab00015: ;
	hb_xvmPopLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushInteger( 100 );
	goto lab00018;
lab00016: ;
	hb_xvmPushInteger( 17 );
	goto lab00018;
lab00017: ;
	hb_xvmPushLocal( 9 );
lab00018: ;
	hb_xvmPopLocal( 9 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00020;
lab00019: ;
	hb_xvmPushLocal( 15 );
lab00020: ;
	hb_xvmPopLocal( 15 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00022;
lab00021: ;
	hb_xvmPushLocal( 16 );
lab00022: ;
	hb_xvmPopLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00024;
lab00023: ;
	hb_xvmPushLocal( 17 );
lab00024: ;
	hb_xvmPopLocal( 17 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00026;
lab00025: ;
	hb_xvmPushLocal( 19 );
lab00026: ;
	hb_xvmPopLocal( 19 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00028;
lab00027: ;
	hb_xvmPushLocal( 22 );
lab00028: ;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 119 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 120 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmPushInteger( 1 );
	goto lab00030;
lab00029: ;
	hb_xvmPushInteger( 14 );
lab00030: ;
	if( hb_xvmMult() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 121 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmPushInteger( 1 );
	goto lab00032;
lab00031: ;
	hb_xvmPushInteger( 8 );
lab00032: ;
	if( hb_xvmMult() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 122 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 123 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 124 );
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 125 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 126 );
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 127 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 128 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 131 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 61 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1073741824 );
#else
	hb_xvmPushLong( 1073741824L );
#endif
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmPushInteger( 1 );
	goto lab00034;
lab00033: ;
	hb_xvmPushInteger( 0 );
lab00034: ;
	hb_xvmPushLocal( 22 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 67108864 );
#else
	hb_xvmPushLong( 67108864L );
#endif
	goto lab00036;
lab00035: ;
	hb_xvmPushInteger( 0 );
lab00036: ;
	if( hb_xvmFunction( 5 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 132 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 133 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 11 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 134 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 135 );
	hb_xvmPushSymbol( symbols + 65 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 136 );
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 137 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 7 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 138 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 139 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 22 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 140 );
	hb_xvmPushSymbol( symbols + 70 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 141 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 18 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 142 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 19 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 143 );
	hb_xvmPushSymbol( symbols + 73 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 20 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 144 );
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 21 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00037;
	hb_xvmSetLine( 147 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "SCROLLBAR", 9 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 148 );
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 25 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 23 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 149 );
	hb_xvmPushSymbol( symbols + 20 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 25 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00037: ;
	hb_xvmSetLine( 152 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmSetLine( 153 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00038: ;
	hb_xvmSetLine( 156 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSBSCRLBAR_WINNEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 15 );
	hb_xvmSetLine( 172 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	hb_xvmPopLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 2 );
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 2 );
lab00004: ;
	hb_xvmPopLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushInteger( 1 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 3 );
lab00006: ;
	hb_xvmPopLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 4 );
lab00008: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 11 );
lab00010: ;
	hb_xvmPopLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00012;
lab00011: ;
	hb_xvmPushLocal( 12 );
lab00012: ;
	hb_xvmPopLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00014;
lab00013: ;
	hb_xvmPushLocal( 13 );
lab00014: ;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 174 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 175 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 176 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 177 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 178 );
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 179 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 180 );
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 181 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 182 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 7 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 183 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 184 );
	hb_xvmPushSymbol( symbols + 65 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 185 );
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 186 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 187 );
	hb_xvmPushSymbol( symbols + 73 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 188 );
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 15 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 189 );
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 190 );
	hb_xvmPushSymbol( symbols + 79 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 191 );
	hb_xvmPushSymbol( symbols + 80 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 193 );
	hb_xvmPushSymbol( symbols + 82 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 194 );
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 195 );
	hb_xvmPushSymbol( symbols + 20 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 197 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSBSCRLBAR_HANDLEEVENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 206 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 2049L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 207 );
	hb_xvmPushSymbol( symbols + 83 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 208 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 210 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 2050L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 211 );
	hb_xvmPushSymbol( symbols + 84 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 212 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 214 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 2051L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 215 );
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 216 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 218 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 2052L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 219 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 220 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 222 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 2058L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 223 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 224 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 226 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 2062L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 227 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 228 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 231 );
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSBSCRLBAR_MOUSEMOVE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 239 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 241 );
	hb_xvmPushSymbol( symbols + 91 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmPushNil();
lab00002: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSBSCRLBAR_SETPAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 251 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 2 );
lab00002: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 253 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 254 );
	hb_xvmLocalSetInt( 3, 2L );
	goto lab00005;
lab00003: ;
	hb_xvmSetLine( 256 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 257 );
	hb_xvmLocalSetInt( 3, 1L );
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 259 );
	hb_xvmLocalSetInt( 3, 0L );
lab00005: ;
	hb_xvmSetLine( 263 );
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 266 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	goto lab00007;
lab00006: ;
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00007: ;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 268 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSBSCRLBAR_SETMODE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 276 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 278 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 280 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 1 );
lab00004: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 93, 1 );
	/* *** END PROC *** */
   } while( 0 );
}

