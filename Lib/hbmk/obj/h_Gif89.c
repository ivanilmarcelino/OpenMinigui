/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_Gif89.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEANIGIF );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( HB_FILEEXISTS );
HB_FUNC_EXTERN( TEMPFILE );
HB_FUNC_EXTERN( GETTEMPFOLDER );
HB_FUNC_EXTERN( RCDATATOFILE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC( TGIF );
HB_FUNC_EXTERN( HB_ISOBJECT );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( FERASE );
HB_FUNC( _RELEASEANIGIF );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_STATIC( _ERASEGIFDEF );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( _DELNAMELIST );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( HBOBJECT );
HB_FUNC_STATIC( TGIF_NEW );
HB_FUNC_STATIC( TGIF_PLAYGIF );
HB_FUNC_STATIC( GIFPLAY );
HB_FUNC_STATIC( TGIF_UPDATE );
HB_FUNC_STATIC( GIFSTOP );
HB_FUNC_STATIC( TGIF_RESTARTGIF );
HB_FUNC_STATIC( GIFISRUNNING );
HB_FUNC_STATIC( TGIF_END );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC( LOADGIF );
HB_FUNC_EXTERN( ACLONE );
HB_FUNC_EXTERN( _GETID );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( _DEFINEIMAGE );
HB_FUNC_EXTERN( _DEFINETIMER );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( DOMETHOD );
HB_FUNC_EXTERN( _RELEASECONTROL );
HB_FUNC_STATIC( READFROMSTREAM );
HB_FUNC_EXTERN( HB_AT );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( HB_PS );
HB_FUNC_EXTERN( CFILENOEXT );
HB_FUNC_EXTERN( STRZERO );
HB_FUNC_EXTERN( FCREATE );
HB_FUNC_EXTERN( FERROR );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( FWRITE );
HB_FUNC_EXTERN( FCLOSE );
HB_FUNC( GETFRAMEDELAY );
HB_FUNC_EXTERN( DOEVENTS );
HB_FUNC_EXTERN( FOPEN );
HB_FUNC_EXTERN( FSEEK );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( FREAD );
HB_FUNC_EXTERN( BIN2W );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_INITSTATICS();
HB_FUNC( CLASS_TGIF );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_GIF89 )
{ "_DEFINEANIGIF", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEANIGIF )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "HB_FILEEXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FILEEXISTS )}, NULL },
{ "TEMPFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( TEMPFILE )}, NULL },
{ "GETTEMPFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEMPFOLDER )}, NULL },
{ "RCDATATOFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( RCDATATOFILE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TGIF", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGIF )}, NULL },
{ "HB_ISOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISOBJECT )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "HGIF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "FERASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERASE )}, NULL },
{ "_RELEASEANIGIF", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _RELEASEANIGIF )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "END", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ERASEGIFDEF", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ERASEGIFDEF )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "_DELNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DELNAMELIST )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "HBOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBOBJECT )}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TGIF_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGIF_NEW )}, NULL },
{ "TGIF_PLAYGIF", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGIF_PLAYGIF )}, NULL },
{ "ADDINLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GIFPLAY", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( GIFPLAY )}, NULL },
{ "TGIF_UPDATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGIF_UPDATE )}, NULL },
{ "GIFSTOP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( GIFSTOP )}, NULL },
{ "TGIF_RESTARTGIF", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGIF_RESTARTGIF )}, NULL },
{ "RESTARTGIF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GIFISRUNNING", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( GIFISRUNNING )}, NULL },
{ "TGIF_END", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGIF_END )}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_CPARENTNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CCONTROLNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CFILENAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NDELAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LOADGIF", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( LOADGIF )}, NULL },
{ "_NTOTALFRAMES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NCURRENTFRAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_APICTDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACLONE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ACLONE )}, NULL },
{ "_AIMAGEDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_GETID", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETID )}, NULL },
{ "_HGIF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "_DEFINEIMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEIMAGE )}, NULL },
{ "NTOTALFRAMES", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CTIMER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_DEFINETIMER", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINETIMER )}, NULL },
{ "CTIMER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADELAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NCURRENTFRAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PLAYGIF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "APICTDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CPARENTNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "CCONTROLNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "CFILENAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "UPDATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DOMETHOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOMETHOD )}, NULL },
{ "_RELEASECONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _RELEASECONTROL )}, NULL },
{ "_ADELAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "READFROMSTREAM", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( READFROMSTREAM )}, NULL },
{ "HB_AT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_AT )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "HB_PS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PS )}, NULL },
{ "CFILENOEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( CFILENOEXT )}, NULL },
{ "STRZERO", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRZERO )}, NULL },
{ "FCREATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCREATE )}, NULL },
{ "FERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERROR )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "FWRITE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FWRITE )}, NULL },
{ "FCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCLOSE )}, NULL },
{ "GETFRAMEDELAY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETFRAMEDELAY )}, NULL },
{ "NDELAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DOEVENTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOEVENTS )}, NULL },
{ "FOPEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOPEN )}, NULL },
{ "FSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( FSEEK )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "FREAD", {HB_FS_PUBLIC}, {HB_FUNCNAME( FREAD )}, NULL },
{ "BIN2W", {HB_FS_PUBLIC}, {HB_FUNCNAME( BIN2W )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "(_INITSTATICS00002)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL },
{ "CLASS_TGIF", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLASS_TGIF )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_GIF89, "h_Gif89.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_GIF89
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_GIF89 )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEANIGIF )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 9 );
	hb_xvmSetLine( 53 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 56 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 57 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00003;
lab00002: ;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00003: ;
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 60 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 61 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 64 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 65 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00006: ;
	hb_xvmSetLine( 68 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 3 );
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
lab00007: ;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " PICTURE Property Invalid Type.", 31 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00008: ;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " PICTURE Can't Be Empty.", 24 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00009: ;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "gif", 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushStringConst( "GIF", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 84 );
	hb_xvmCopyLocals( 3, 16 );
	hb_xvmSetLine( 85 );
	hb_xvmCopyLocals( 15, 3 );
lab00010: ;
	hb_xvmSetLine( 91 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 12 );
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 103 );
	hb_xvmPushStringConst( "ANIGIF", 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 104 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 107 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 110 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 111 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 112 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 113 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 114 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 115 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 116 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 117 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 118 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 119 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 120 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 121 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 122 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 125 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 126 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 128 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 129 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 132 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 134 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 135 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 138 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 139 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 140 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 142 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 9 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 146 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 149 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 150 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 2 ) ) break;
lab00011: ;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 1 ) ) break;
lab00012: ;
	hb_xvmSetLine( 158 );
	hb_xvmPushLocal( 14 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _RELEASEANIGIF )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 196 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 199 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "ANIGIF", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 200 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 201 );
	hb_xvmPushSymbol( symbols + 24 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 196 );
	if( hb_xvmLocalIncPush( 5 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
lab00004: ;
	hb_xvmSetLine( 210 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _ERASEGIFDEF )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 239 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmLocalAdd( 3 );
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 253 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 254 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 255 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 258 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 260 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 261 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 262 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 263 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 264 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 265 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 266 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 267 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 268 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 269 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 270 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 271 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 272 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 273 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 274 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 275 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 276 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 277 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 278 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 280 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 282 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 286 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 288 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 292 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 294 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TGIF )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 102 );
	hb_xvmSetLine( 300 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushStaticByRef( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSeqAlways();
	do {
	hb_xvmLocalSetInt( 1, 1L );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "TGif", 4 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 16 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 303 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hGif", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 304 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cFilename", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 305 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cParentName", 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 306 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cControlName", 12 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 307 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aPictData", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 308 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aImageData", 10 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 309 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nTotalFrames", 12 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 310 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nCurrentFrame", 13 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 311 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nDelay", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 312 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aDelay", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 313 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cTimer", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 316 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 317 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "PlayGif", 7 );
	hb_xvmPushSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 318 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Play", 4 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 176, 36, 0, 95, 1, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 319 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Update", 6 );
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 320 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Stop", 4 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 176, 38, 0, 95, 1, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 321 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "RestartGif", 10 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 322 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Restart", 7 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 48, 40, 0, 95, 1, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 323 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "IsRunning", 9 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 176, 41, 0, 95, 1, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 324 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "End", 3 );
	hb_xvmPushSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 326 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushStaticByRef( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 47 );
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
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TGIF_NEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 9 );
	hb_xvmSetLine( 361 );
	hb_xvmPushSelf();
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 364 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 365 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 368 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 369 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 370 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 371 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 372 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 373 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 376 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 377 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 378 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 379 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 382 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 383 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmArrayGen( 1 );
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 384 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmArrayGen( 1 );
	hb_xvmPopLocal( 13 );
lab00001: ;
	hb_xvmSetLine( 387 );
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 388 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 391 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 392 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 395 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 396 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 400 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 403 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 404 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushStringConst( "tgif_tmr_", 9 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 408 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 14 ] = {
			0, 0, 1, 0, 10, 0, 48, 69, 0, 95, 255, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 411 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Picture", 7 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	hb_xvmSetLine( 414 );
	hb_xvmPushLocal( 10 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TGIF_PLAYGIF )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 444 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 445 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushSelf();
	if( hb_xvmPushObjectVarRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 447 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 451 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Picture", 7 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 452 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Value", 5 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 454 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TGIF_UPDATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 483 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 487 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Row", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Row", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Col", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Col", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Width", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Width", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Height", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Height", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 490 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Row", 3 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Row", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 491 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Col", 3 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Col", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 492 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Width", 5 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Width", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 493 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Height", 6 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Height", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	hb_xvmSetLine( 497 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TGIF_RESTARTGIF )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 525 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 1 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 527 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 530 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 176, 21, 0, 95, 1, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 533 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushSelf();
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 534 );
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 535 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 536 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 537 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 538 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 541 );
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 543 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TGIF_END )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 570 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 571 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 176, 21, 0, 95, 1, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 574 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 575 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Release", 7 );
	if( hb_xvmDo( 3 ) ) break;
lab00001: ;
	hb_xvmSetLine( 579 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 580 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 584 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( GIFPLAY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 614 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 615 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
lab00001: ;
	hb_xvmSetLine( 618 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( GIFSTOP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 644 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 645 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00001: ;
	hb_xvmSetLine( 648 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( GIFISRUNNING )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 674 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 676 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 677 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Enabled", 7 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 680 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( LOADGIF )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 11, 4 );
	hb_xvmSFrame( symbols + 102 );
	hb_xvmSetLine( 732 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 734 );
	hb_xvmPushStringConst( "\x00!\xF9", 3 );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 745 );
	hb_xvmPushStaticByRef( 2 );
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 747 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 748 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 749 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 751 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 752 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 755 );
	hb_xvmLocalSetInt( 12, 0L );
	hb_xvmSetLine( 756 );
	hb_xvmLocalSetInt( 14, 1L );
	hb_xvmSetLine( 757 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 758 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 760 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmAddInt( 2L ) ) break;
	hb_xvmPopLocal( 14 );
lab00002: ;
	hb_xvmSetLine( 766 );
	if( hb_xvmLocalInc( 12 ) ) break;
	hb_xvmSetLine( 768 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmAddInt( 3L ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 770 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 771 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_frame_", 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".gif", 4 );
	hb_xvmLocalAdd( 9 );
	hb_xvmSetLine( 772 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 773 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 774 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 777 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmLocalAdd( 10 );
	hb_xvmSetLine( 778 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 780 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 781 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 784 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 785 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 788 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 789 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 792 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 794 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 797 );
	hb_xvmCopyLocals( 15, 14 );
	goto lab00002;
lab00007: ;
	hb_xvmSetLine( 802 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 804 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_frame_", 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 87 );
	if( hb_xvmLocalIncPush( 12 ) ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".gif", 4 );
	hb_xvmLocalAdd( 9 );
	hb_xvmSetLine( 805 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 806 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 807 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 810 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDec() ) break;
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmLocalAdd( 10 );
	hb_xvmSetLine( 811 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDec() ) break;
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 813 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 814 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmSetLine( 817 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmSetLine( 818 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00010: ;
	hb_xvmSetLine( 821 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 822 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00011: ;
	hb_xvmSetLine( 826 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( READFROMSTREAM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 854 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 856 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 857 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 858 );
	hb_xvmPushFuncSymbol( symbols + 98 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 859 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 860 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 861 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 864 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualInt( 0L ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
lab00002: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETFRAMEDELAY )
{
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 894 );
	hb_xvmPushFuncSymbol( symbols + 100 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmMult() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 102, 2 );
	hb_xvmSFrame( symbols + 102 );
	hb_xvmPushInteger( 0 );
	hb_xvmPopStatic( 2 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CLASS_TGIF )
{
   do {
	/* *** END PROC *** */
   } while( 0 );
}

