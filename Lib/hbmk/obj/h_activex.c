/*
 * Harbour 3.2.0dev (r2503200530)
 * Borland C++ 5.8.2 (32-bit)
 * Generated C source from "h_activex.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC_INIT( _INITACTIVEX );
HB_FUNC_EXTERN( INSTALLMETHODHANDLER );
HB_FUNC_EXTERN( INSTALLPROPERTYHANDLER );
HB_FUNC( _DEFINEACTIVEX );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC( TACTIVEX );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( CHANGESTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC( RELEASEACTIVEX );
HB_FUNC_EXTERN( GETCONTROLTYPE );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC_EXTERN( HB_ISOBJECT );
HB_FUNC( SETACTIVEXOBJECT );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC( GETACTIVEXOBJECT );
HB_FUNC( _GETCONTROLOBJECT );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( HBOBJECT );
HB_FUNC_STATIC( TACTIVEX_NEW );
HB_FUNC_STATIC( TACTIVEX_LOAD );
HB_FUNC_STATIC( TACTIVEX_RESIZE );
HB_FUNC_STATIC( TACTIVEX_HIDE );
HB_FUNC_STATIC( TACTIVEX_SHOW );
HB_FUNC_STATIC( TACTIVEX_RELEASE );
HB_FUNC_STATIC( TACTIVEX_REFRESH );
HB_FUNC_STATIC( TACTIVEX_ADJUST );
HB_FUNC_STATIC( TACTIVEX_GETROW );
HB_FUNC_STATIC( TACTIVEX_GETCOL );
HB_FUNC_STATIC( TACTIVEX_GETWIDTH );
HB_FUNC_STATIC( TACTIVEX_GETHEIGHT );
HB_FUNC_STATIC( TACTIVEX_EVENTMAP );
HB_FUNC_STATIC( TACTIVEX_ONERROR );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC( ATLAXWININIT );
HB_FUNC_STATIC( CREATEWINDOWEX );
HB_FUNC_EXTERN( MOVEWINDOW );
HB_FUNC( ATLAXGETDISP );
HB_FUNC_EXTERN( __BREAKBLOCK );
HB_FUNC_EXTERN( CREATEOBJECT );
HB_FUNC_EXTERN( MSGINFO );
HB_FUNC( SETUPCONNECTIONPOINT );
HB_FUNC_EXTERN( DESTROYWINDOW );
HB_FUNC( SHUTDOWNCONNECTIONPOINT );
HB_FUNC( RELEASEDISPATCH );
HB_FUNC( ATLAXWINEND );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( __GETMESSAGE );
HB_FUNC_EXTERN( RIGHT );
HB_FUNC_EXTERN( HB_EXECFROMARRAY );
HB_FUNC_EXTERN( HB_APARAMS );
HB_FUNC_INITSTATICS();
HB_FUNC( CLASS_TACTIVEX );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_ACTIVEX )
{ "_INITACTIVEX$", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( _INITACTIVEX )}, NULL },
{ "INSTALLMETHODHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLMETHODHANDLER )}, NULL },
{ "INSTALLPROPERTYHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLPROPERTYHANDLER )}, NULL },
{ "_DEFINEACTIVEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEACTIVEX )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TACTIVEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX )}, NULL },
{ "LOAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HATL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HSINK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "EVENTMAP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "CHANGESTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHANGESTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "RELEASEACTIVEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RELEASEACTIVEX )}, NULL },
{ "GETCONTROLTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLTYPE )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "HB_ISOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISOBJECT )}, NULL },
{ "RELEASE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETACTIVEXOBJECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETACTIVEXOBJECT )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "GETACTIVEXOBJECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETACTIVEXOBJECT )}, NULL },
{ "_GETCONTROLOBJECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETCONTROLOBJECT )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "HBOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBOBJECT )}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TACTIVEX_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_NEW )}, NULL },
{ "TACTIVEX_LOAD", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_LOAD )}, NULL },
{ "TACTIVEX_RESIZE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_RESIZE )}, NULL },
{ "TACTIVEX_HIDE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_HIDE )}, NULL },
{ "TACTIVEX_SHOW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_SHOW )}, NULL },
{ "TACTIVEX_RELEASE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_RELEASE )}, NULL },
{ "TACTIVEX_REFRESH", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_REFRESH )}, NULL },
{ "TACTIVEX_ADJUST", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_ADJUST )}, NULL },
{ "TACTIVEX_GETROW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_GETROW )}, NULL },
{ "TACTIVEX_GETCOL", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_GETCOL )}, NULL },
{ "TACTIVEX_GETWIDTH", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_GETWIDTH )}, NULL },
{ "TACTIVEX_GETHEIGHT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_GETHEIGHT )}, NULL },
{ "TACTIVEX_EVENTMAP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_EVENTMAP )}, NULL },
{ "SETONERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TACTIVEX_ONERROR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_ONERROR )}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "_NROW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NCOL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NHEIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CWINDOWNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CPROGID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NOLDWINWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NOLDWINHEIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CWINDOWNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATLAXWININIT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ATLAXWININIT )}, NULL },
{ "_HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CREATEWINDOWEX", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( CREATEWINDOWEX )}, NULL },
{ "CPROGID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MOVEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MOVEWINDOW )}, NULL },
{ "NCOL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NROW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NHEIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATLAXGETDISP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ATLAXGETDISP )}, NULL },
{ "_HATL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__BREAKBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __BREAKBLOCK )}, NULL },
{ "_OOLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CREATEOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATEOBJECT )}, NULL },
{ "MSGINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGINFO )}, NULL },
{ "DESCRIPTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETUPCONNECTIONPOINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETUPCONNECTIONPOINT )}, NULL },
{ "AAXEV", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AAXEXEC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HSINK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OOLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BHIDE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NOLDWINWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NOLDWINHEIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BHIDE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DESTROYWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( DESTROYWINDOW )}, NULL },
{ "SHUTDOWNCONNECTIONPOINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SHUTDOWNCONNECTIONPOINT )}, NULL },
{ "RELEASEDISPATCH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RELEASEDISPATCH )}, NULL },
{ "ATLAXWINEND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ATLAXWINEND )}, NULL },
{ "HIDE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SHOW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "__GETMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GETMESSAGE )}, NULL },
{ "RIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RIGHT )}, NULL },
{ "HB_EXECFROMARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EXECFROMARRAY )}, NULL },
{ "HB_APARAMS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_APARAMS )}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL },
{ "CLASS_TACTIVEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLASS_TACTIVEX )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_ACTIVEX, "h_activex.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_ACTIVEX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_ACTIVEX )
   #include "hbiniseg.h"
#endif

HB_FUNC_INIT( _INITACTIVEX )
{
	static const HB_BYTE pcode[] =
	{
		176,1,0,106,8,82,101,108,101,97,115,101,0,106,
		15,82,101,108,101,97,115,101,65,99,116,105,118,101,
		88,0,20,2,176,2,0,106,8,88,79,98,106,101,
		99,116,0,106,17,83,101,116,65,99,116,105,118,101,
		88,79,98,106,101,99,116,0,106,17,71,101,116,65,
		99,116,105,118,101,88,79,98,106,101,99,116,0,20,
		3,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( _DEFINEACTIVEX )
{
	static const HB_BYTE pcode[] =
	{
		13,7,9,98,4,0,92,34,1,31,10,98,4,0,
		92,65,1,28,26,98,4,0,92,65,1,28,10,98,
		4,0,92,63,1,25,8,98,4,0,92,33,1,80,
		2,98,4,0,92,37,1,121,15,28,51,96,4,0,
		98,4,0,92,40,1,98,4,0,92,37,1,1,135,
		96,3,0,98,4,0,92,39,1,98,4,0,92,37,
		1,1,135,98,4,0,92,38,1,98,4,0,92,37,
		1,1,80,2,176,5,0,95,2,12,1,31,41,176,
		6,0,106,9,87,105,110,100,111,119,58,32,0,95,
		2,72,106,17,32,105,115,32,110,111,116,32,100,101,
		102,105,110,101,100,46,0,72,20,1,176,7,0,95,
		1,95,2,12,2,28,54,176,6,0,106,10,67,111,
		110,116,114,111,108,58,32,0,95,1,72,106,5,32,
		79,102,32,0,72,95,2,72,106,18,32,65,108,114,
		101,97,100,121,32,100,101,102,105,110,101,100,46,0,
		72,20,1,176,8,0,95,7,12,1,31,67,176,6,
		0,106,10,67,111,110,116,114,111,108,58,32,0,95,
		1,72,106,5,32,79,102,32,0,72,95,2,72,106,
		31,32,80,82,79,71,73,68,32,80,114,111,112,101,
		114,116,121,32,73,110,118,97,108,105,100,32,84,121,
		112,101,46,0,72,20,1,176,9,0,95,7,12,1,
		28,60,176,6,0,106,10,67,111,110,116,114,111,108,
		58,32,0,95,1,72,106,5,32,79,102,32,0,72,
		95,2,72,106,24,32,80,82,79,71,73,68,32,67,
		97,110,39,116,32,98,101,32,101,109,112,116,121,46,
		0,72,20,1,106,2,95,0,95,2,72,106,2,95,
		0,72,95,1,72,80,12,48,10,0,176,11,0,12,
		0,95,2,95,7,95,3,95,4,95,5,95,6,112,
		6,80,14,48,12,0,95,14,112,0,80,15,48,13,
		0,95,14,112,0,80,10,48,14,0,95,14,112,0,
		80,16,176,9,0,48,15,0,95,14,112,0,12,1,
		31,66,176,16,0,95,8,12,1,28,57,176,17,0,
		95,8,12,1,121,15,28,46,176,16,0,95,8,122,
		1,12,1,28,35,176,18,0,95,8,89,26,0,1,
		0,1,0,14,0,48,19,0,95,255,95,1,122,1,
		95,1,92,2,1,112,2,6,20,2,98,4,0,92,
		41,1,28,15,176,20,0,98,4,0,92,45,1,95,
		10,20,2,176,21,0,95,9,9,12,2,28,14,176,
		22,0,95,10,93,0,2,100,120,20,4,176,23,0,
		95,2,12,1,80,11,176,24,0,12,0,80,13,176,
		25,0,95,12,95,13,20,2,106,8,65,67,84,73,
		86,69,88,0,98,4,0,93,135,0,1,95,13,2,
		95,1,98,4,0,93,136,0,1,95,13,2,95,10,
		98,4,0,93,137,0,1,95,13,2,95,11,98,4,
		0,93,138,0,1,95,13,2,95,14,98,4,0,93,
		139,0,1,95,13,2,106,1,0,98,4,0,93,140,
		0,1,95,13,2,95,8,98,4,0,93,141,0,1,
		95,13,2,100,98,4,0,93,142,0,1,95,13,2,
		106,1,0,98,4,0,93,143,0,1,95,13,2,106,
		1,0,98,4,0,93,144,0,1,95,13,2,106,1,
		0,98,4,0,93,145,0,1,95,13,2,106,1,0,
		98,4,0,93,146,0,1,95,13,2,9,98,4,0,
		93,134,0,1,95,13,2,100,98,4,0,93,147,0,
		1,95,13,2,100,98,4,0,93,148,0,1,95,13,
		2,106,1,0,98,4,0,93,149,0,1,95,13,2,
		4,0,0,98,4,0,93,150,0,1,95,13,2,95,
		3,98,4,0,93,151,0,1,95,13,2,95,4,98,
		4,0,93,152,0,1,95,13,2,95,5,98,4,0,
		93,153,0,1,95,13,2,95,6,98,4,0,93,154,
		0,1,95,13,2,121,98,4,0,93,155,0,1,95,
		13,2,98,4,0,92,37,1,121,15,28,17,98,4,
		0,92,39,1,98,4,0,92,37,1,1,25,4,92,
		255,98,4,0,93,156,0,1,95,13,2,98,4,0,
		92,37,1,121,15,28,17,98,4,0,92,40,1,98,
		4,0,92,37,1,1,25,4,92,255,98,4,0,93,
		157,0,1,95,13,2,106,1,0,98,4,0,93,158,
		0,1,95,13,2,121,98,4,0,93,159,0,1,95,
		13,2,100,98,4,0,93,160,0,1,95,13,2,100,
		98,4,0,93,161,0,1,95,13,2,4,0,0,98,
		4,0,93,168,0,1,95,13,2,106,1,0,98,4,
		0,93,162,0,1,95,13,2,121,98,4,0,93,163,
		0,1,95,13,2,121,98,4,0,93,164,0,1,95,
		13,2,106,1,0,98,4,0,93,165,0,1,95,13,
		2,120,98,4,0,93,166,0,1,95,13,2,95,16,
		98,4,0,93,209,0,1,95,13,2,100,98,4,0,
		93,167,0,1,95,13,2,121,98,4,0,93,169,0,
		1,95,13,2,120,98,4,0,93,170,0,1,95,13,
		2,95,15,98,4,0,93,171,0,1,95,13,2,106,
		1,0,98,4,0,93,168,1,1,95,13,2,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( RELEASEACTIVEX )
{
	static const HB_BYTE pcode[] =
	{
		13,1,2,176,7,0,95,2,95,1,12,2,28,70,
		176,27,0,95,2,95,1,12,2,106,8,65,67,84,
		73,86,69,88,0,8,28,48,98,4,0,93,139,0,
		1,176,28,0,95,2,95,1,12,2,1,80,3,176,
		29,0,95,3,12,1,28,10,48,30,0,95,3,112,
		0,73,120,98,4,0,93,175,0,2,25,10,9,98,
		4,0,93,175,0,2,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( SETACTIVEXOBJECT )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,176,27,0,95,2,95,1,12,2,106,8,
		65,67,84,73,86,69,88,0,8,28,59,176,32,0,
		106,28,84,104,105,115,32,80,114,111,112,101,114,116,
		121,32,105,115,32,82,101,97,100,32,79,110,108,121,
		33,0,106,8,87,97,114,110,105,110,103,0,100,9,
		20,4,120,98,4,0,93,175,0,2,25,10,9,98,
		4,0,93,175,0,2,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( GETACTIVEXOBJECT )
{
	static const HB_BYTE pcode[] =
	{
		13,1,2,176,27,0,95,2,95,1,12,2,106,8,
		65,67,84,73,86,69,88,0,8,28,23,120,98,4,
		0,93,175,0,2,176,34,0,95,2,95,1,12,2,
		80,3,25,10,9,98,4,0,93,175,0,2,95,3,
		110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( _GETCONTROLOBJECT )
{
	static const HB_BYTE pcode[] =
	{
		13,1,2,176,28,0,95,1,95,2,12,2,165,80,
		3,121,8,28,5,100,110,7,98,4,0,93,171,0,
		1,95,3,1,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( TACTIVEX )
{
	static const HB_BYTE pcode[] =
	{
		149,3,0,116,106,0,103,1,0,100,8,29,20,3,
		176,35,0,104,1,0,12,1,29,9,3,166,203,2,
		0,122,80,1,48,10,0,176,36,0,12,0,106,9,
		84,65,99,116,105,118,101,88,0,108,37,4,1,0,
		108,11,112,3,80,2,48,38,0,95,2,100,100,95,
		1,106,5,111,79,108,101,0,4,1,0,9,112,5,
		73,48,38,0,95,2,100,100,95,1,106,5,104,87,
		110,100,0,4,1,0,9,112,5,73,48,38,0,95,
		2,100,100,95,1,106,12,99,87,105,110,100,111,119,
		78,97,109,101,0,4,1,0,9,112,5,73,48,38,
		0,95,2,100,100,95,1,106,8,99,80,114,111,103,
		73,100,0,4,1,0,9,112,5,73,48,38,0,95,
		2,100,100,95,1,106,6,104,83,105,110,107,0,4,
		1,0,9,112,5,73,48,38,0,95,2,100,100,95,
		1,106,5,104,65,116,108,0,4,1,0,9,112,5,
		73,48,38,0,95,2,100,100,95,1,106,5,110,82,
		111,119,0,4,1,0,9,112,5,73,48,38,0,95,
		2,100,100,95,1,106,5,110,67,111,108,0,4,1,
		0,9,112,5,73,48,38,0,95,2,100,100,95,1,
		106,7,110,87,105,100,116,104,0,4,1,0,9,112,
		5,73,48,38,0,95,2,100,100,95,1,106,8,110,
		72,101,105,103,104,116,0,4,1,0,9,112,5,73,
		48,38,0,95,2,100,100,95,1,106,13,110,79,108,
		100,87,105,110,87,105,100,116,104,0,4,1,0,9,
		112,5,73,48,38,0,95,2,100,100,95,1,106,14,
		110,79,108,100,87,105,110,72,101,105,103,104,116,0,
		4,1,0,9,112,5,73,48,38,0,95,2,100,9,
		95,1,106,6,98,72,105,100,101,0,4,1,0,9,
		112,5,73,48,38,0,95,2,100,4,0,0,95,1,
		106,6,97,65,120,69,118,0,4,1,0,9,112,5,
		73,48,38,0,95,2,100,4,0,0,95,1,106,8,
		97,65,120,69,120,101,99,0,4,1,0,9,112,5,
		73,48,39,0,95,2,106,4,78,101,119,0,108,40,
		95,1,112,3,73,48,39,0,95,2,106,5,76,111,
		97,100,0,108,41,95,1,112,3,73,48,39,0,95,
		2,106,7,82,101,83,105,122,101,0,108,42,95,1,
		112,3,73,48,39,0,95,2,106,5,72,105,100,101,
		0,108,43,95,1,112,3,73,48,39,0,95,2,106,
		5,83,104,111,119,0,108,44,95,1,112,3,73,48,
		39,0,95,2,106,8,82,101,108,101,97,115,101,0,
		108,45,95,1,112,3,73,48,39,0,95,2,106,8,
		82,101,102,114,101,115,104,0,108,46,95,1,112,3,
		73,48,39,0,95,2,106,7,65,100,106,117,115,116,
		0,108,47,95,1,112,3,73,48,39,0,95,2,106,
		7,71,101,116,82,111,119,0,108,48,95,1,112,3,
		73,48,39,0,95,2,106,7,71,101,116,67,111,108,
		0,108,49,95,1,112,3,73,48,39,0,95,2,106,
		9,71,101,116,87,105,100,116,104,0,108,50,95,1,
		112,3,73,48,39,0,95,2,106,10,71,101,116,72,
		101,105,103,104,116,0,108,51,95,1,112,3,73,48,
		39,0,95,2,106,9,69,118,101,110,116,77,97,112,
		0,108,52,95,1,112,3,73,48,53,0,95,2,108,
		54,112,1,73,48,55,0,95,2,112,0,73,167,14,
		0,0,176,56,0,104,1,0,95,2,20,2,168,48,
		57,0,95,2,112,0,80,3,176,58,0,95,3,106,
		10,73,110,105,116,67,108,97,115,115,0,12,2,28,
		12,48,59,0,95,3,164,146,1,0,73,95,3,110,
		7,48,57,0,103,1,0,112,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_NEW )
{
	static const HB_BYTE pcode[] =
	{
		13,0,6,176,9,0,95,3,12,1,28,5,121,80,
		3,176,9,0,95,4,12,1,28,5,121,80,4,176,
		9,0,95,5,12,1,28,19,176,60,0,95,1,106,
		6,119,105,100,116,104,0,12,2,80,5,176,9,0,
		95,6,12,1,28,20,176,60,0,95,1,106,7,72,
		101,105,103,104,116,0,12,2,80,6,48,61,0,102,
		95,3,112,1,73,48,62,0,102,95,4,112,1,73,
		48,63,0,102,95,5,112,1,73,48,64,0,102,95,
		6,112,1,73,48,65,0,102,95,1,112,1,73,48,
		66,0,102,95,2,112,1,73,48,67,0,102,176,60,
		0,95,1,106,6,119,105,100,116,104,0,12,2,112,
		1,73,48,68,0,102,176,60,0,95,1,106,7,72,
		101,105,103,104,116,0,12,2,112,1,73,102,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_LOAD )
{
	static const HB_BYTE pcode[] =
	{
		13,4,0,176,23,0,48,69,0,102,112,0,12,1,
		80,4,176,70,0,20,0,48,71,0,102,176,72,0,
		95,4,48,73,0,102,112,0,12,2,112,1,73,176,
		74,0,48,13,0,102,112,0,48,75,0,102,112,0,
		48,76,0,102,112,0,48,77,0,102,112,0,48,78,
		0,102,112,0,120,20,6,176,79,0,48,13,0,102,
		112,0,12,1,80,2,48,80,0,102,95,2,112,1,
		73,113,29,0,0,176,81,0,12,0,178,48,82,0,
		102,176,83,0,95,2,12,1,112,1,73,73,114,19,
		0,0,115,80,1,176,84,0,48,85,0,95,1,112,
		0,20,1,176,86,0,48,14,0,102,112,0,96,3,
		0,48,87,0,102,112,0,48,88,0,102,112,0,12,
		4,121,8,28,11,48,89,0,102,95,3,112,1,73,
		48,90,0,102,112,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_RESIZE )
{
	static const HB_BYTE pcode[] =
	{
		13,0,4,48,91,0,102,112,0,31,22,176,74,0,
		48,13,0,102,112,0,95,2,95,1,95,3,95,4,
		120,20,6,48,61,0,102,95,1,112,1,73,48,62,
		0,102,95,2,112,1,73,48,63,0,102,95,3,112,
		1,73,48,64,0,102,95,4,112,1,73,48,67,0,
		102,176,60,0,48,69,0,102,112,0,106,6,119,105,
		100,116,104,0,12,2,112,1,73,48,68,0,102,176,
		60,0,48,69,0,102,112,0,106,7,72,101,105,103,
		104,116,0,12,2,112,1,73,120,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_ADJUST )
{
	static const HB_BYTE pcode[] =
	{
		13,2,0,48,92,0,102,112,0,48,77,0,102,112,
		0,48,75,0,102,112,0,72,49,80,1,48,93,0,
		102,112,0,48,78,0,102,112,0,48,76,0,102,112,
		0,72,49,80,2,176,74,0,48,13,0,102,112,0,
		48,75,0,102,112,0,48,76,0,102,112,0,176,60,
		0,48,69,0,102,112,0,106,6,119,105,100,116,104,
		0,12,2,48,75,0,102,112,0,49,95,1,49,176,
		60,0,48,69,0,102,112,0,106,7,104,101,105,103,
		104,116,0,12,2,48,76,0,102,112,0,49,95,2,
		49,120,20,6,48,63,0,102,176,60,0,48,69,0,
		102,112,0,106,6,119,105,100,116,104,0,12,2,48,
		75,0,102,112,0,49,95,1,49,112,1,73,48,64,
		0,102,176,60,0,48,69,0,102,112,0,106,7,104,
		101,105,103,104,116,0,12,2,48,76,0,102,112,0,
		49,95,2,49,112,1,73,48,67,0,102,176,60,0,
		48,69,0,102,112,0,106,6,119,105,100,116,104,0,
		12,2,112,1,73,48,68,0,102,176,60,0,48,69,
		0,102,112,0,106,7,72,101,105,103,104,116,0,12,
		2,112,1,73,120,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_GETROW )
{
	static const HB_BYTE pcode[] =
	{
		48,76,0,102,112,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_GETCOL )
{
	static const HB_BYTE pcode[] =
	{
		48,75,0,102,112,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_GETWIDTH )
{
	static const HB_BYTE pcode[] =
	{
		48,77,0,102,112,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_GETHEIGHT )
{
	static const HB_BYTE pcode[] =
	{
		48,78,0,102,112,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_HIDE )
{
	static const HB_BYTE pcode[] =
	{
		176,74,0,48,13,0,102,112,0,121,121,121,121,120,
		20,6,48,94,0,102,120,112,1,73,120,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_SHOW )
{
	static const HB_BYTE pcode[] =
	{
		176,74,0,48,13,0,102,112,0,48,75,0,102,112,
		0,48,76,0,102,112,0,48,77,0,102,112,0,48,
		78,0,102,112,0,120,20,6,48,94,0,102,9,112,
		1,73,120,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_RELEASE )
{
	static const HB_BYTE pcode[] =
	{
		48,13,0,102,112,0,100,69,28,13,176,95,0,48,
		13,0,102,112,0,20,1,176,9,0,48,15,0,102,
		112,0,12,1,31,13,176,96,0,48,15,0,102,112,
		0,20,1,176,97,0,48,14,0,102,112,0,20,1,
		176,98,0,20,0,120,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_REFRESH )
{
	static const HB_BYTE pcode[] =
	{
		48,99,0,102,112,0,73,48,100,0,102,112,0,73,
		120,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_EVENTMAP )
{
	static const HB_BYTE pcode[] =
	{
		13,1,3,176,101,0,48,87,0,102,112,0,95,1,
		12,2,80,4,95,4,121,8,28,44,176,20,0,48,
		87,0,102,112,0,95,1,20,2,176,20,0,48,88,
		0,102,112,0,100,100,4,2,0,20,2,176,17,0,
		48,87,0,102,112,0,12,1,80,4,95,2,95,3,
		4,2,0,48,88,0,102,112,0,95,4,2,100,110,
		7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( TACTIVEX_ONERROR )
{
	static const HB_BYTE pcode[] =
	{
		149,1,0,176,102,0,12,0,80,1,95,1,122,1,
		106,2,95,0,8,28,13,176,103,0,95,1,92,2,
		12,2,80,1,176,104,0,48,90,0,102,112,0,95,
		1,176,105,0,12,0,20,3,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITSTATICS()
{
	static const HB_BYTE pcode[] =
	{
		117,106,0,1,0,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( CLASS_TACTIVEX )
{
	static const HB_BYTE pcode[] =
	{
		7
	};

	hb_vmExecute( pcode, symbols );
}

#line 496 "h_activex.prg"

#ifndef CINTERFACE
#define CINTERFACE   1                    // Enable C-style interfaces if not already defined
#endif
#ifndef NONAMELESSUNION
#define NONAMELESSUNION                   // Enable non-anonymous unions if not already defined
#endif
#include <mgdefs.h>                       // Include MiniGUI definitions
#include <commctrl.h>                     // Include common controls for GUI elements
#include <ocidl.h>                        // Include OLE Control Interface definitions
#include <hbvm.h>                         // Harbour Virtual Machine interface
#include <hbapiitm.h>                     // Harbour API item management
#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );   // Declaration of a function to convert ANSI to wide strings for Unicode
#endif

// Declaration of an external function to retrieve a function address from a DLL
extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );

// Typedefs for function pointers used to initialize and manage ActiveX controls
typedef HRESULT ( WINAPI *LPAtlAxWinInit ) ( void );                 // Typedef for AtlAxWinInit function pointer
typedef HRESULT ( WINAPI *LPAtlAxGetControl ) ( HWND, IUnknown ** ); // Typedef for AtlAxGetControl function pointer

// Static global variables to hold handles and function pointers for ActiveX library
static HMODULE    hAtl = NULL;                  // Handle to Atl.dll
LPAtlAxWinInit    AtlAxWinInit;                 // Pointer to AtlAxWinInit function
LPAtlAxGetControl AtlAxGetControl;

// Pointer to AtlAxGetControl function
// Initializes the ActiveX library by loading Atl.dll and getting function pointers
static void _Ax_Init( void )
{
   if( !hAtl )                                  // If the library is not already loaded
   {
      hAtl = LoadLibrary( TEXT( "Atl.Dll" ) );  // Load Atl.dll library
      AtlAxWinInit = ( LPAtlAxWinInit ) wapi_GetProcAddress( hAtl, "AtlAxWinInit" );            // Get AtlAxWinInit function address
      AtlAxGetControl = ( LPAtlAxGetControl ) wapi_GetProcAddress( hAtl, "AtlAxGetControl" );   // Get AtlAxGetControl function address
      ( AtlAxWinInit ) (); // Initialize ActiveX (AtlAxWinInit function)
   }
}

// Harbour function to initialize the ActiveX library by calling _Ax_Init
HB_FUNC( ATLAXWININIT )
{
   _Ax_Init();
}

// Harbour function to free the ActiveX library
HB_FUNC( ATLAXWINEND )
{
   if( hAtl )              // If the library handle exists
   {
      FreeLibrary( hAtl ); // Free the Atl.dll library
      hAtl = NULL;         // Reset the handle to NULL
   }
}

// Harbour function to get IDispatch interface from an ActiveX control
HB_FUNC( ATLAXGETDISP ) // hWnd -> pDisp
{
   IUnknown    *pUnk;   // Pointer to IUnknown interface
   IDispatch   *pDisp;  // Pointer to IDispatch interface
   _Ax_Init();          // Ensure ActiveX library is initialized
   AtlAxGetControl( hmg_par_raw_HWND( 1 ), &pUnk );            // Get the IUnknown interface of the control
#if defined( __cplusplus )
   pUnk->QueryInterface( IID_IDispatch, ( void ** ) &pDisp );  // Query for IDispatch in C++
#else
   pUnk->lpVtbl->QueryInterface( pUnk, &IID_IDispatch, ( void ** ) &pDisp );  // Query for IDispatch in C-style
#endif
   pUnk->lpVtbl->Release( pUnk );   // Release the IUnknown interface
   hmg_ret_raw_HANDLE( pDisp );     // Return the IDispatch interface to the caller
}

// Harbour function to create an ActiveX window using a ProgID
HB_FUNC_STATIC( CREATEWINDOWEX ) // ( hWnd, cProgId ) -> hActiveXWnd
{
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );  // Get window name in ANSI
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Convert window name to Unicode if applicable
#endif
   hmg_ret_raw_HWND
   (
      CreateWindowEx
         (
            0,
            TEXT( "AtlAxWin" ),     // Extended window creation with AtlAxWin class
            lpWindowName,
            WS_VISIBLE | WS_CHILD,  // Window styles for visibility and child placement
            0,
            0,
            0,
            0, // Position and size (defaults to 0 here)
            hmg_par_raw_HWND( 1 ),  // Parent window handle
            0,
            0,
            NULL
         )
   ); // No additional parameters
}

// Conditional include for hash-based event handling if __USEHASHEVENTS is defined
#ifdef __USEHASHEVENTS
#include <hashapi.h>
#endif

//------------------------------------------------------------------------------
// Prototype for function that converts OLE Variant to Harbour item
HRESULT  hb_oleVariantToItem( PHB_ITEM pItem, VARIANT *pVariant );

// Definition of a custom IDispatch-based COM interface for event handling
#undef INTERFACE
#define INTERFACE IEventHandler

DECLARE_INTERFACE_( INTERFACE, IDispatch )
{
   // IUnknown methods
   STDMETHOD ( QueryInterface ) ( THIS_ REFIID, void ** ) PURE;

   STDMETHOD_ ( ULONG, AddRef ) ( THIS ) PURE;

   STDMETHOD_ ( ULONG, Release ) ( THIS ) PURE;

   // IDispatch methods
   STDMETHOD_ ( ULONG, GetTypeInfoCount ) ( THIS_ UINT * ) PURE;

   STDMETHOD_ ( ULONG, GetTypeInfo ) ( THIS_ UINT, LCID, ITypeInfo ** ) PURE;

   STDMETHOD_ ( ULONG, GetIDsOfNames ) ( THIS_ REFIID, LPOLESTR *, UINT, LCID, DISPID * ) PURE;

   STDMETHOD_ ( ULONG, Invoke ) ( THIS_ DISPID, REFIID, LCID, WORD, DISPPARAMS *, VARIANT *, EXCEPINFO *, UINT * ) PURE;
};

// Explanation of the IEventHandler structure and its extended version
// IEventHandler starts with a pointer to its VTable, a required structure
// for all COM objects. The extended struct (MyRealIEventHandler) contains
// additional private members but is presented as an IEventHandler to external
// applications.
// Extended structure for event handler with private data members
typedef struct
{
   IEventHandler     *lpVtbl;             // Pointer to virtual function table (VTable) for IEventHandler
   DWORD             count;               // Reference count for memory management
   IConnectionPoint  *pIConnectionPoint;  // Pointer to connection point for event handling
   DWORD             dwEventCookie;       // Event subscription identifier
   IID               device_event_interface_iid;   // Interface identifier for the device event
   PHB_ITEM          pEvents;       // Harbour item to store events
#ifndef __USEHASHEVENTS
   PHB_ITEM          pEventsExec;   // Harbour item for direct event execution if not using hash events
#endif
} MyRealIEventHandler;

//------------------------------------------------------------------------------
// Here are IEventHandler's functions.
//------------------------------------------------------------------------------
// Every COM object's interface must have the 3 functions QueryInterface(),
// AddRef(), and Release().
// IEventHandler's QueryInterface()
static HRESULT STDMETHODCALLTYPE QueryInterface( IEventHandler *self, REFIID vTableGuid, void **ppv )
{
   // Check if the GUID matches IEvenetHandler VTable's GUID. We gave the C variable name
   // IID_IEventHandler to our VTable GUID. We can use an OLE function called
   // IsEqualIID to do the comparison for us. Also, if the caller passed a
   // IUnknown GUID, then we'll likewise return the IEventHandler, since it can
   // masquerade as an IUnknown object too. Finally, if the called passed a
   // IDispatch GUID, then we'll return the IExample3, since it can masquerade
   // as an IDispatch too
   if( IsEqualIID( vTableGuid, &IID_IUnknown ) )
   {
      *ppv = ( IUnknown * ) self;

      // Increment the count of callers who have an outstanding pointer to self object
      self->lpVtbl->AddRef( self );
      return S_OK;
   }

   if( IsEqualIID( vTableGuid, &IID_IDispatch ) )
   {
      *ppv = ( IDispatch * ) self;
      self->lpVtbl->AddRef( self );
      return S_OK;
   }

   if( IsEqualIID( vTableGuid, &( ( ( MyRealIEventHandler * ) self )->device_event_interface_iid ) ) )
   {
      *ppv = ( IDispatch * ) self;
      self->lpVtbl->AddRef( self );
      return S_OK;
   }

   // We don't recognize the GUID passed to us. Let the caller know self,
   // by clearing his handle, and returning E_NOINTERFACE.
   *ppv = 0;
   return E_NOINTERFACE;
}

//------------------------------------------------------------------------------
// IEventHandler's AddRef()
static ULONG STDMETHODCALLTYPE AddRef( IEventHandler *self )
{
   // Increment IEventHandler's reference count, and return the updated value.
   // NOTE: We have to typecast to gain access to any data members. These
   // members are not defined  (so that an app can't directly access them).
   // Rather they are defined only above in our MyRealIEventHandler
   // struct. So typecast to that in order to access those data members
   return ++( ( MyRealIEventHandler * ) self )->count;
}

//------------------------------------------------------------------------------
// IEventHandler's Release()
static ULONG STDMETHODCALLTYPE Release( IEventHandler *self )
{
   if( --( ( MyRealIEventHandler * ) self )->count == 0 )
   {
      GlobalFree( self );
      return 0;
   }

   return( ( MyRealIEventHandler * ) self )->count;
}

//------------------------------------------------------------------------------
// IEventHandler's GetTypeInfoCount()
static ULONG STDMETHODCALLTYPE GetTypeInfoCount( IEventHandler *self, UINT *pCount )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( pCount );
   return( ULONG ) E_NOTIMPL;
}

//------------------------------------------------------------------------------
// IEventHandler's GetTypeInfo()
static ULONG STDMETHODCALLTYPE GetTypeInfo( IEventHandler *self, UINT itinfo, LCID lcid, ITypeInfo **pTypeInfo )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( itinfo );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( pTypeInfo );
   return( ULONG ) E_NOTIMPL;
}

//------------------------------------------------------------------------------
// IEventHandler's GetIDsOfNames()
static ULONG STDMETHODCALLTYPE GetIDsOfNames( IEventHandler *self, REFIID riid, LPOLESTR *rgszNames, UINT cNames, LCID lcid, DISPID *rgdispid )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( riid );
   HB_SYMBOL_UNUSED( rgszNames );
   HB_SYMBOL_UNUSED( cNames );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( rgdispid );
   return( ULONG ) E_NOTIMPL;
}

//------------------------------------------------------------------------------
// IEventHandler's Invoke()
// self is where the action happens
// self function receives events (by their ID number) and distributes the processing
// or them or ignores them
static ULONG STDMETHODCALLTYPE Invoke
(
   IEventHandler  *self,
   DISPID         dispid,
   REFIID         riid,
   LCID           lcid,
   WORD           wFlags,
   DISPPARAMS     *params,
   VARIANT        *result,
   EXCEPINFO      *pexcepinfo,
   UINT           *puArgErr
)
{
   PHB_ITEM pItem;
   int      iArg, i;
   PHB_ITEM pItemArray[32];         // max 32 parameters?
   PHB_ITEM *pItems;
   HB_SIZE  ulPos;
   PHB_ITEM Key;

   Key = hb_itemNew( NULL );

   // We implement only a "default" interface
   if( !IsEqualIID( riid, &IID_NULL ) )
   {
      return( ULONG ) DISP_E_UNKNOWNINTERFACE;
   }

   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( wFlags );
   HB_SYMBOL_UNUSED( result );
   HB_SYMBOL_UNUSED( pexcepinfo );
   HB_SYMBOL_UNUSED( puArgErr );

   // delegate work to somewhere else in PRG
   //***************************************
#ifdef __USEHASHEVENTS
   if( hb_hashScan( ( ( MyRealIEventHandler * ) self )->pEvents, hb_itemPutNL( Key, dispid ), &ulPos ) )
   {
      PHB_ITEM pArray = hb_hashGetValueAt( ( ( MyRealIEventHandler * ) self )->pEvents, ulPos );
#else
   #if defined( __XHARBOUR__ )
      ulPos = hb_arrayScan( ( ( MyRealIEventHandler * ) self )->pEvents, hb_itemPutNL( Key, dispid ), NULL, NULL, 0, 0 );
   #else
   ulPos = hb_arrayScan( ( ( MyRealIEventHandler * ) self )->pEvents, hb_itemPutNL( Key, dispid ), NULL, NULL, 0 );
   #endif
   if( ulPos )
   {
      PHB_ITEM pArray = hb_arrayGetItemPtr( ( ( MyRealIEventHandler * ) self )->pEventsExec, ulPos );
#endif
      PHB_ITEM pExec = hb_arrayGetItemPtr( pArray, 1 );

      if( pExec )
      {
         if( hb_vmRequestReenter() )
         {
            switch( hb_itemType( pExec ) )
            {
               case HB_IT_BLOCK:
                  {
#ifdef __XHARBOUR__
                     hb_vmPushSymbol( &hb_symEval );
#else
                     hb_vmPushEvalSym();

#endif
                     hb_vmPush( pExec );
                     break;
                  }

               case HB_IT_STRING:
                  {
                     PHB_ITEM pObject = hb_arrayGetItemPtr( pArray, 2 );
                     hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( hb_itemGetCPtr( pExec ) ) ) );

                     if( HB_IS_OBJECT( pObject ) )
                     {
                        hb_vmPush( pObject );
                     }
                     else
                     {
                        hb_vmPushNil();
                     }
                     break;
                  }

               case HB_IT_POINTER:
                  {
                     hb_vmPushSymbol( hb_dynsymSymbol( ( ( PHB_SYMB ) pExec )->pDynSym ) );
                     hb_vmPushNil();
                     break;
                  }
            }

            iArg = params->cArgs;
            for( i = 1; i <= iArg; i++ )
            {
               pItem = hb_itemNew( NULL );
               hb_oleVariantToItem( pItem, &( params->rgvarg[iArg - i] ) );
               pItemArray[i - 1] = pItem;

               // set bit i
               //ulRefMask |= ( 1L << ( i - 1 ) );
            }

            if( iArg )
            {
               pItems = pItemArray;
               if( iArg )
               {
                  for( i = 0; i < iArg; i++ )
                  {
                     hb_vmPush( ( pItems )[i] );
                  }
               }
            }

            // execute
            hb_vmDo( ( USHORT ) iArg );

            // En caso de que los parametros sean pasados por referencia
            for( i = iArg; i > 0; i-- )
            {
               if( ( ( &( params->rgvarg[iArg - i] ) )->n1.n2.vt & VT_BYREF ) == VT_BYREF )
               {
                  switch( ( &( params->rgvarg[iArg - i] ) )->n1.n2.vt )
                  {
                     //case VT_UI1|VT_BYREF:
                     //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pbVal) = va_arg(argList,unsigned char*);  //pItemArray[i-1]
                     //   break;
                     case VT_I2 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.piVal ) = ( short ) hb_itemGetNI( pItemArray[i - 1] );
                        break;

                     case VT_I4 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.plVal ) = ( long ) hb_itemGetNL( pItemArray[i - 1] );
                        break;

                     case VT_R4 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pfltVal ) = ( float ) hb_itemGetND( pItemArray[i - 1] );
                        break;

                     case VT_R8 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pdblVal ) = ( double ) hb_itemGetND( pItemArray[i - 1] );
                        break;

                     case VT_BOOL | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pboolVal ) = ( VARIANT_BOOL ) ( hb_itemGetL( pItemArray[i - 1] ) ? 0xFFFF : 0 );
                        break;

                     //case VT_ERROR|VT_BYREF:
                     //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pscode) = va_arg(argList, SCODE*);
                     //   break;
                     case VT_DATE | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pdate ) = ( DATE ) ( double ) ( hb_itemGetDL( pItemArray[i - 1] ) - 2415019 );
                        break;

                        //case VT_CY|VT_BYREF:
                        //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pcyVal) = va_arg(argList, CY*);
                        //   break;
                        //case VT_BSTR|VT_BYREF:
                        //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pbstrVal = va_arg(argList, BSTR*);
                        //   break;
                        //case VT_UNKNOWN|VT_BYREF:
                        //   pArg->ppunkVal = va_arg(argList, LPUNKNOWN*);
                        //   break;
                        //case VT_DISPATCH|VT_BYREF:
                        //   pArg->ppdispVal = va_arg(argList, LPDISPATCH*);
                        //   break;
                  }
               }
            }

            hb_vmRequestRestore();
         }
      }
   }

   hb_itemRelease( Key );

   return S_OK;
}

//------------------------------------------------------------------------------
// Here's IEventHandler's VTable. It never changes so we can declare it static
static const IEventHandlerVtbl   IEventHandler_Vtbl = { QueryInterface, AddRef, Release, GetTypeInfoCount, GetTypeInfo, GetIDsOfNames, Invoke };

//------------------------------------------------------------------------------
// constructor
// params:
// device_interface        - refers to the interface type of the COM object (whose event we are trying to receive).
// device_event_interface  - indicates the interface type of the outgoing interface supported by the COM object.
//                           This will be the interface that must be implemented by the Sink object.
//                           is essentially derived from IDispatch, our Sink object (self IEventHandler)
//                           is also derived from IDispatch.
typedef IEventHandler            device_interface;

// Hash  // SetupConnectionPoint( oOle:hObj, @hSink, hEvents )             -> nError
// Array // SetupConnectionPoint( oOle:hObj, @hSink, aEvents, aExecEvent ) -> nError

HB_FUNC( SETUPCONNECTIONPOINT )
{
   // Declaration of local variables
   IConnectionPointContainer  *pIConnectionPointContainerTemp = NULL;
   IUnknown                   *pIUnknown = NULL;
   IConnectionPoint           *m_pIConnectionPoint = NULL;
   IEnumConnectionPoints      *m_pIEnumConnectionPoints;
   HRESULT                    hr;
   IID                        rriid = { 0 };
   register IEventHandler     *selfobj;
   DWORD                      dwCookie = 0;

   device_interface           *pdevice_interface = ( device_interface * ) HB_PARNL( 1 );
   MyRealIEventHandler        *pThis;

   // Allocate memory for the IEventHandler object (as a MyRealIEventHandler).
   // Intentional misrepresentation of size to fit within allocated memory.
   selfobj = ( IEventHandler * ) GlobalAlloc( GMEM_FIXED, sizeof( MyRealIEventHandler ) );

   if( !selfobj )
   {
      // Memory allocation failed, return error code.
      hr = E_OUTOFMEMORY;
   }
   else
   {
      // Set up the IEventHandler object by assigning the correct VTable and initializing the reference count.
      selfobj->lpVtbl = ( IEventHandlerVtbl * ) &IEventHandler_Vtbl;
      ( ( MyRealIEventHandler * ) selfobj )->count = 0;

      // Assign the default interface GUID for events to IDispatch
      ( ( MyRealIEventHandler * ) selfobj )->device_event_interface_iid = IID_IDispatch;

      // Query the IUnknown pointer of self, used later to connect to the device_interface's Connection Point.
      hr = selfobj->lpVtbl->QueryInterface( selfobj, &IID_IUnknown, ( void ** ) ( void * ) &pIUnknown );
      if( hr == S_OK && pIUnknown )
      {
         // Query the device interface for its connection point container interface
         hr = pdevice_interface->lpVtbl->QueryInterface
            (
               pdevice_interface,
               &IID_IConnectionPointContainer,
               ( void ** ) ( void * ) &pIConnectionPointContainerTemp
            );

         if( hr == S_OK && pIConnectionPointContainerTemp )
         {
            // Retrieve the list of connection points available in the interface.
            hr = pIConnectionPointContainerTemp->lpVtbl->EnumConnectionPoints( pIConnectionPointContainerTemp, &m_pIEnumConnectionPoints );

            if( hr == S_OK && m_pIEnumConnectionPoints )
            {
               do
               {
                  // Move to the next available connection point.
                  hr = m_pIEnumConnectionPoints->lpVtbl->Next( m_pIEnumConnectionPoints, 1, &m_pIConnectionPoint, NULL );
                  if( hr == S_OK )
                  {
                     // Get the interface GUID from the connection point.
                     if( m_pIConnectionPoint->lpVtbl->GetConnectionInterface( m_pIConnectionPoint, &rriid ) == S_OK )
                     {
                        break;
                     }
                  }
               }
               while( hr == S_OK );

               // Release the enumerator once finished.
               m_pIEnumConnectionPoints->lpVtbl->Release( m_pIEnumConnectionPoints );
            }

            pIConnectionPointContainerTemp->lpVtbl->Release( pIConnectionPointContainerTemp );
            pIConnectionPointContainerTemp = NULL;
         }

         if( hr == S_OK && m_pIConnectionPoint )
         {
            if( hr == S_OK )
            {
               // Set the device event interface GUID to the retrieved GUID.
               ( ( MyRealIEventHandler * ) selfobj )->device_event_interface_iid = rriid;
            }

            // Connect to the connection point and store the connection cookie.
            hr = m_pIConnectionPoint->lpVtbl->Advise( m_pIConnectionPoint, pIUnknown, &dwCookie );
            ( ( MyRealIEventHandler * ) selfobj )->pIConnectionPoint = m_pIConnectionPoint;
            ( ( MyRealIEventHandler * ) selfobj )->dwEventCookie = dwCookie;
         }

         // Release the IUnknown pointer.
         pIUnknown->lpVtbl->Release( pIUnknown );
         pIUnknown = NULL;
      }
   }

   if( selfobj )
   {
      // If successful, store the events list from parameters and assign to the object.
      pThis = ( MyRealIEventHandler * ) selfobj;

#ifndef __USEHASHEVENTS
      pThis->pEventsExec = hb_itemNew( hb_param( 4, HB_IT_ANY ) );
#endif
      pThis->pEvents = hb_itemNew( hb_param( 3, HB_IT_ANY ) );
      HB_STORNL( ( LONG_PTR ) pThis, 2 );
   }

   // Return the HRESULT result code from the setup process.
   hb_retnl( hr );
}

//------------------------------------------------------------------------------
// Disconnect and clean up the connection point, removing the event sink.
//------------------------------------------------------------------------------
HB_FUNC( SHUTDOWNCONNECTIONPOINT )
{
   MyRealIEventHandler  *self = ( MyRealIEventHandler * ) HB_PARNL( 1 );

   if( self->pIConnectionPoint )
   {
      // Unadvise the connection point, releasing the event sink.
      self->pIConnectionPoint->lpVtbl->Unadvise( self->pIConnectionPoint, self->dwEventCookie );
      self->dwEventCookie = 0;

      // Release the connection point object.
      self->pIConnectionPoint->lpVtbl->Release( self->pIConnectionPoint );
      self->pIConnectionPoint = NULL;
   }
}

//------------------------------------------------------------------------------
// Release a previously acquired IDispatch interface pointer to clean up.
//------------------------------------------------------------------------------
HB_FUNC( RELEASEDISPATCH )
{
   IDispatch   *pObj;

   // Retrieve the IDispatch pointer from parameters and release it.
   pObj = ( IDispatch * ) HB_PARNL( 1 );
   pObj->lpVtbl->Release( pObj );
}

