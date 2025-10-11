/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "ErrorSys.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( ERRORSYS );
HB_FUNC_EXTERN( ERRORBLOCK );
HB_FUNC_STATIC( DEFERROR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( GETSTARTUPFOLDER );
HB_FUNC_EXTERN( HB_PS );
HB_FUNC_EXTERN( MINIGUIVERSION );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( NETERR );
HB_FUNC_EXTERN( __SETCENTURY );
HB_FUNC( HTML_ERRORLOG );
HB_FUNC_STATIC( ERRORMESSAGE );
HB_FUNC( HTML_RAWTEXT );
HB_FUNC( HTML_LINETEXT );
HB_FUNC_EXTERN( TIME );
HB_FUNC_EXTERN( HB_PROGNAME );
HB_FUNC_EXTERN( NETNAME );
HB_FUNC_EXTERN( HB_USERNAME );
HB_FUNC_EXTERN( TIMEFROMSTART );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC_EXTERN( PADC );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( PROCNAME );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( PROCLINE );
HB_FUNC_EXTERN( PROCFILE );
HB_FUNC( _LSHOWDETAILERROR );
HB_FUNC_STATIC( ERRORLOG );
HB_FUNC( HTML_LINE );
HB_FUNC( HTML_END );
HB_FUNC_STATIC( SHOWERROR );
HB_FUNC_EXTERN( EXITPROCESS );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC( GETOSERRORDESCRIPTION );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( CVALTOCHAR );
HB_FUNC_EXTERN( _SETGETGLOBAL );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_EXTERN( GETCONTROLTYPE );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( _SETMSGALERTCOLORS );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( _DEFINEFONT );
HB_FUNC_EXTERN( HMG_ALERT_MAXLINES );
HB_FUNC_EXTERN( ALERTSTOP );
HB_FUNC_EXTERN( MSGSTOP );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( ERRORLEVEL );
HB_FUNC_EXTERN( RELEASEALLWINDOWS );
HB_FUNC( GETCPUINFO );
HB_FUNC_STATIC( STRVALUE );
HB_FUNC_EXTERN( MEMORYSTATUS );
HB_FUNC_EXTERN( DISKNAME );
HB_FUNC_EXTERN( CURDIR );
HB_FUNC_EXTERN( ROUND );
HB_FUNC_EXTERN( HB_DISKSPACE );
HB_FUNC_EXTERN( HB_DIRBASE );
HB_FUNC_EXTERN( OS );
HB_FUNC_EXTERN( VERSION );
HB_FUNC_EXTERN( HB_BUILDDATE );
HB_FUNC_EXTERN( HB_COMPILER );
HB_FUNC_EXTERN( HB_MTVM );
HB_FUNC_EXTERN( HB_VMMODE );
HB_FUNC_EXTERN( HB_ISFUNCTION );
HB_FUNC_EXTERN( HB_MACROBLOCK );
HB_FUNC_EXTERN( ASC );
HB_FUNC_EXTERN( _GETERRORLOGFILE );
HB_FUNC_EXTERN( DO );
HB_FUNC_EXTERN( HB_WAEVAL );
HB_FUNC_EXTERN( __MVDBGINFO );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( HB_CSTR );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( _ADDNEWGLOBAL );
HB_FUNC_EXTERN( ISERRORLOGACTIVE );
HB_FUNC_EXTERN( HB_VFEXISTS );
HB_FUNC( HTML_INI );
HB_FUNC_EXTERN( HB_VFOPEN );
HB_FUNC_EXTERN( HB_VFSEEK );
HB_FUNC_STATIC( __HTML_INSERT_OFFSET );
HB_FUNC_STATIC( __HTML_BODY_TEMPLATE );
HB_FUNC_EXTERN( HB_VFWRITE );
HB_FUNC_EXTERN( RTRIM );
HB_FUNC_EXTERN( HB_VFCLOSE );
HB_FUNC_EXTERN( HB_BASE64DECODE );
HB_FUNC_EXTERN( GETREGISTRYVALUE );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( TRIM );
HB_FUNC( _SETERRORLOGFILE );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_ERRORSYS )
{ "ERRORSYS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ERRORSYS )}, NULL },
{ "ERRORBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( ERRORBLOCK )}, NULL },
{ "DEFERROR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( DEFERROR )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "GETSTARTUPFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSTARTUPFOLDER )}, NULL },
{ "HB_PS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PS )}, NULL },
{ "MINIGUIVERSION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MINIGUIVERSION )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "GENCODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CANSUBSTITUTE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CANRETRY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OSCODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CANDEFAULT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NETERR", {HB_FS_PUBLIC}, {HB_FUNCNAME( NETERR )}, NULL },
{ "__SETCENTURY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETCENTURY )}, NULL },
{ "HTML_ERRORLOG", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HTML_ERRORLOG )}, NULL },
{ "ERRORMESSAGE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( ERRORMESSAGE )}, NULL },
{ "HTML_RAWTEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HTML_RAWTEXT )}, NULL },
{ "HTML_LINETEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HTML_LINETEXT )}, NULL },
{ "TIME", {HB_FS_PUBLIC}, {HB_FUNCNAME( TIME )}, NULL },
{ "HB_PROGNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PROGNAME )}, NULL },
{ "NETNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( NETNAME )}, NULL },
{ "HB_USERNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_USERNAME )}, NULL },
{ "TIMEFROMSTART", {HB_FS_PUBLIC}, {HB_FUNCNAME( TIMEFROMSTART )}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "PADC", {HB_FS_PUBLIC}, {HB_FUNCNAME( PADC )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "PROCNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCNAME )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "PROCLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCLINE )}, NULL },
{ "PROCFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCFILE )}, NULL },
{ "_LSHOWDETAILERROR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _LSHOWDETAILERROR )}, NULL },
{ "ERRORLOG", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( ERRORLOG )}, NULL },
{ "HTML_LINE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HTML_LINE )}, NULL },
{ "HTML_END", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HTML_END )}, NULL },
{ "SHOWERROR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( SHOWERROR )}, NULL },
{ "EXITPROCESS", {HB_FS_PUBLIC}, {HB_FUNCNAME( EXITPROCESS )}, NULL },
{ "SEVERITY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "SUBSYSTEM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "SUBCODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DESCRIPTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "FILENAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OPERATION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETOSERRORDESCRIPTION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETOSERRORDESCRIPTION )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "ARGS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CVALTOCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CVALTOCHAR )}, NULL },
{ "_SETGETGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETGLOBAL )}, NULL },
{ "_TSB_ACONTROLHWND", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "GETCONTROLTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLTYPE )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "_SETMSGALERTCOLORS", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETMSGALERTCOLORS )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "_DEFINEFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEFONT )}, NULL },
{ "HMG_ALERT_MAXLINES", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_ALERT_MAXLINES )}, NULL },
{ "ALERTSTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALERTSTOP )}, NULL },
{ "MSGSTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGSTOP )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "ERRORLEVEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ERRORLEVEL )}, NULL },
{ "RELEASEALLWINDOWS", {HB_FS_PUBLIC}, {HB_FUNCNAME( RELEASEALLWINDOWS )}, NULL },
{ "GETCPUINFO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETCPUINFO )}, NULL },
{ "STRVALUE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( STRVALUE )}, NULL },
{ "MEMORYSTATUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( MEMORYSTATUS )}, NULL },
{ "DISKNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISKNAME )}, NULL },
{ "CURDIR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CURDIR )}, NULL },
{ "ROUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROUND )}, NULL },
{ "HB_DISKSPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DISKSPACE )}, NULL },
{ "HB_DIRBASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DIRBASE )}, NULL },
{ "OS", {HB_FS_PUBLIC}, {HB_FUNCNAME( OS )}, NULL },
{ "VERSION", {HB_FS_PUBLIC}, {HB_FUNCNAME( VERSION )}, NULL },
{ "HB_BUILDDATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BUILDDATE )}, NULL },
{ "HB_COMPILER", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_COMPILER )}, NULL },
{ "HB_MTVM", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_MTVM )}, NULL },
{ "HB_VMMODE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VMMODE )}, NULL },
{ "HB_ISFUNCTION", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISFUNCTION )}, NULL },
{ "HB_MACROBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_MACROBLOCK )}, NULL },
{ "ASC", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASC )}, NULL },
{ "_GETERRORLOGFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETERRORLOGFILE )}, NULL },
{ "DO", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO )}, NULL },
{ "HB_WAEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_WAEVAL )}, NULL },
{ "__MVDBGINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVDBGINFO )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "HB_CSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_CSTR )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "_ADDNEWGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ADDNEWGLOBAL )}, NULL },
{ "ISERRORLOGACTIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISERRORLOGACTIVE )}, NULL },
{ "HB_VFEXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VFEXISTS )}, NULL },
{ "HTML_INI", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HTML_INI )}, NULL },
{ "HB_VFOPEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VFOPEN )}, NULL },
{ "HB_VFSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VFSEEK )}, NULL },
{ "__HTML_INSERT_OFFSET", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( __HTML_INSERT_OFFSET )}, NULL },
{ "__HTML_BODY_TEMPLATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( __HTML_BODY_TEMPLATE )}, NULL },
{ "HB_VFWRITE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VFWRITE )}, NULL },
{ "RTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( RTRIM )}, NULL },
{ "HB_VFCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VFCLOSE )}, NULL },
{ "HB_BASE64DECODE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BASE64DECODE )}, NULL },
{ "GETREGISTRYVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETREGISTRYVALUE )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "TRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( TRIM )}, NULL },
{ "_SETERRORLOGFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETERRORLOGFILE )}, NULL },
{ "(_INITSTATICS00002)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_ERRORSYS, "ErrorSys.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_ERRORSYS
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_ERRORSYS )
   #include "hbiniseg.h"
#endif

HB_FUNC( ERRORSYS )
{
   do {
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 176, 2, 0, 95, 1, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 112 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "error.log", 9 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 113 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 92 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( DEFERROR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 1 );
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualInt( 10L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 128 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 129 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 134 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 41L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 135 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 141 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 21L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 32L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 142 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 148 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 40L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 150 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 153 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushStringConst( "ON", 2 );
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "<div class=\"record\">", 20 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 161 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "<p class=\"updated\">", 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 162 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "Date: <span class=\"date\">", 25 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "</span> ", 8 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Time: <span class=\"time\">", 25 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "</span>", 7 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 163 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "Application: ", 13 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 164 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "User: ", 6 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " / ", 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 165 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "Time from start: ", 17 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 166 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "<span class=\"error\">", 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "</span>", 7 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "</p>", 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 168 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "<details><summary>", 18 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 171 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushStringConst( " Stack Trace ", 13 );
	hb_xvmPushInteger( 79 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 172 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "<br/></summary><span class=\"stacktrace\">", 40 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 174 );
	hb_xvmLocalSetInt( 6, 1L );
lab00006: ;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 177 );
	hb_xvmPushStringConst( "Called from ", 12 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "(", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmPushStringConst( " in module: ", 12 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	goto lab00008;
lab00007: ;
	hb_xvmPushStringConst( "", 0 );
lab00008: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 5 );
	hb_xvmSetLine( 178 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 179 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00006;
lab00009: ;
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "</span></details>", 17 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
lab00010: ;
	hb_xvmSetLine( 190 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "</div>", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 194 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 198 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( ERRORMESSAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 225 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushStringConst( "Error", 5 );
	goto lab00002;
lab00001: ;
	hb_xvmPushStringConst( "Warning", 7 );
lab00002: ;
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmLocalAdd( 2 );
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 230 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 232 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "\?\?\?", 3 );
	if( hb_xvmPlusEqPop() ) break;
lab00004: ;
	hb_xvmSetLine( 236 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 237 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "/", 1 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 239 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "/\?\?\?", 4 );
	if( hb_xvmPlusEqPop() ) break;
lab00006: ;
	hb_xvmSetLine( 243 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 244 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00007: ;
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 250 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( ": ", 2 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 251 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 252 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( ": ", 2 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00009: ;
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmSetLine( 257 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 258 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "OS Error: ", 10 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00010: ;
	hb_xvmSetLine( 261 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 262 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 263 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "   Args:", 8 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 264 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00016;
lab00011: ;
	hb_xvmSetLine( 269 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "     [", 6 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "] = ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "   ", 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushStringConst( " length: ", 9 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	goto lab00013;
lab00012: ;
	hb_xvmPushStringConst( "", 0 );
lab00013: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00015;
lab00014: ;
	hb_xvmPushStringConst( "", 0 );
lab00015: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 264 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00016: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
lab00017: ;
	hb_xvmSetLine( 273 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( SHOWERROR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 302 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 304 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushStringConst( "_HMG_ShowError", 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 305 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushStringConst( "_HMG_ShowError", 14 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 308 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushStringConst( "_HMG_ShowError", 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 310 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushStringConst( "_HMG_ShowError", 14 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 312 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPopMemvar( symbols + 52 ) ) break;
	hb_xvmSetLine( 314 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	if( hb_xvmPushMemvar( symbols + 54 ) ) break;
	if( hb_xvmArrayItemPush( 456L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 315 );
	hb_xvmPushSymbol( symbols + 55 );
	if( hb_xvmPushMemvar( symbols + 54 ) ) break;
	if( hb_xvmArrayItemPush( 456L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 4 ) ) break;
	hb_xvmPopLocal( 3 );
lab00002: ;
	hb_xvmSetLine( 318 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLocal( 1 );
	goto lab00004;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
lab00004: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 320 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	if( hb_xvmPushMemvar( symbols + 54 ) ) break;
	if( hb_xvmArrayItemPush( 456L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	if( hb_xvmPushMemvar( symbols + 54 ) ) break;
	if( hb_xvmArrayItemPush( 456L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 326 );
	{
		static const HB_BYTE codeblock[ 206 ] = {
			176, 57, 0, 106, 7, 83, 97, 121, 95, 48, 49, 0, 106, 5, 111, 68, 
			108, 103, 0, 12, 2, 106, 5, 69, 68, 73, 84, 0, 8, 28, 6, 100, 
			26, 173, 0, 176, 58, 0, 106, 5, 111, 68, 108, 103, 0, 106, 7, 83, 
			97, 121, 95, 48, 49, 0, 106, 10, 70, 111, 110, 116, 67, 111, 108, 111, 
			114, 0, 93, 255, 0, 93, 255, 0, 121, 4, 3, 0, 20, 4, 176, 58, 
			0, 106, 5, 111, 68, 108, 103, 0, 106, 7, 83, 97, 121, 95, 48, 49, 
			0, 106, 10, 65, 108, 105, 103, 110, 109, 101, 110, 116, 0, 106, 7, 67, 
			69, 78, 84, 69, 82, 0, 20, 4, 176, 58, 0, 106, 5, 111, 68, 108, 
			103, 0, 106, 7, 83, 97, 121, 95, 48, 50, 0, 106, 10, 70, 111, 110, 
			116, 67, 111, 108, 111, 114, 0, 93, 255, 0, 93, 255, 0, 121, 4, 3, 
			0, 20, 4, 176, 58, 0, 106, 5, 111, 68, 108, 103, 0, 106, 7, 83, 
			97, 121, 95, 48, 50, 0, 106, 10, 65, 108, 105, 103, 110, 109, 101, 110, 
			116, 0, 106, 7, 67, 69, 78, 84, 69, 82, 0, 12, 4, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 328 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	if( hb_xvmPushMemvar( symbols + 54 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 329 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 54 ) ) break;
	if( hb_xvmArrayItemPop( 444L ) ) break;
lab00005: ;
	hb_xvmSetLine( 332 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushInteger( 128 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 333 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 335 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushStringConst( "DlgFont", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 336 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushStringConst( "DlgFont", 7 );
	hb_xvmPushStringConst( "Verdana", 7 );
	hb_xvmPushInteger( 14 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	if( hb_xvmDo( 10 ) ) break;
lab00006: ;
	hb_xvmSetLine( 339 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 340 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushInteger( 35 );
	if( hb_xvmDo( 1 ) ) break;
lab00007: ;
	hb_xvmSetLine( 343 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "Program Error", 13 );
	hb_xvmPushStringConst( "ZZZ_B_STOP64", 12 );
	hb_xvmPushInteger( 64 );
	hb_xvmPushInteger( 217 );
	hb_xvmPushInteger( 67 );
	hb_xvmPushInteger( 67 );
	hb_xvmArrayGen( 3 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 7 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 347 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( ";", 1 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushStringConst( "Program Error", 13 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00009: ;
	hb_xvmSetLine( 351 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 353 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	if( hb_xvmPushMemvar( symbols + 54 ) ) break;
	if( hb_xvmArrayItemPush( 456L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 354 );
	hb_xvmPushSymbol( symbols + 55 );
	if( hb_xvmPushMemvar( symbols + 54 ) ) break;
	if( hb_xvmArrayItemPush( 456L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00010: ;
	hb_xvmSetLine( 357 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	if( hb_xvmDo( 0 ) ) break;
lab00011: ;
	hb_xvmSetLine( 361 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( ERRORLOG )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 2 );
	hb_xvmSFrame( symbols + 109 );
	hb_xvmSetLine( 396 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 398 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 1 );
	hb_xvmSetLine( 400 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<details><summary>", 18 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 401 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushStringConst( " System Information ", 20 );
	hb_xvmPushInteger( 79 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 402 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<br/></summary>", 15 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 404 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Workstation name...: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 405 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Active user name...: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 406 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 407 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "CPU type...........: ", 21 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " [~", 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " MHz]", 5 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 408 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Hardware memory....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " MB", 3 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 409 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Available memory...: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " MB", 3 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 410 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Current disk.......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 411 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Current directory..: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 415 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Free disk space....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivideByInt( 1048576L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " MB", 3 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 417 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 418 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Operating system...: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 419 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "MiniGUI version....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 420 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Harbour version....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 424 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Harbour built on...: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 426 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "C/C++ compiler.....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 435 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Multi Threading....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushStringConst( "YES", 3 );
	goto lab00002;
lab00001: ;
	hb_xvmPushStringConst( "NO", 2 );
lab00002: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 436 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "VM Optimization....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushStringConst( "YES", 3 );
	goto lab00004;
lab00003: ;
	hb_xvmPushStringConst( "NO", 2 );
lab00004: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 438 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushStringConst( "Select", 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 439 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 440 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Current Work Area..: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushStringConst( "Select()", 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 444 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "</details>", 10 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 446 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<details><summary>", 18 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 447 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushStringConst( " Environmental Information ", 27 );
	hb_xvmPushInteger( 79 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 448 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<br/></summary>", 15 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 450 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET ALTERNATE......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 451 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET ALTFILE........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 19 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 452 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET AUTOPEN........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 45 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 453 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET AUTORDER.......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 46 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 454 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET AUTOSHARE......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 47 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 460 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET CENTURY........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 461 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET COUNT..........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 47 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 463 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET DATE FORMAT....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 464 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET DBFLOCKSCHEME..: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 108 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 465 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET DEBUG..........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 466 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET DECIMALS.......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 467 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET DEFAULT........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 468 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET DEFEXTENSIONS..: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 109 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 469 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET DELETED........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 470 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET DELIMCHARS.....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 34 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 471 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET DELIMETERS.....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 472 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET DIRCASE........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 103 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 473 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET DIRSEPARATOR...: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 104 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 475 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET EOL............: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 110 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 476 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET EPOCH..........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 477 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET ERRORLOG.......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 481 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET EXACT..........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 482 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET EXCLUSIVE......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 483 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET EXTRA..........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 21 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 484 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET EXTRAFILE......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 486 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET FILECASE.......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 102 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 487 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET FIXED..........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 488 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET FORCEOPT.......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 107 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 490 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET HARDCOMMIT.....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 106 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 492 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET IDLEREPEAT.....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 101 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 494 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET LANGUAGE.......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 496 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET MARGIN.........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 497 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET MBLOCKSIZE.....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 41 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 498 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET MFILEEXT.......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 42 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 500 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET OPTIMIZE.......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 44 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 505 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET PATH...........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 506 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET PRINTER........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 23 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 510 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET PRINTFILE......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 512 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET SOFTSEEK.......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 519 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET TRIMFILENAME...: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 111 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 521 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "SET UNIQUE.........: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 523 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "</details>", 10 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 525 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<details><summary>", 18 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 526 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushStringConst( " Detailed Work Area Items ", 26 );
	hb_xvmPushInteger( 79 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 527 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<br/></summary>", 15 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 569 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	{
		static const HB_BYTE codeblock[ 546 ] = {
			0, 0, 1, 0, 1, 0, 36, 34, 2, 176, 83, 0, 106, 7, 83, 101, 
			108, 101, 99, 116, 0, 12, 1, 28, 56, 36, 35, 2, 176, 20, 0, 95, 
			255, 106, 22, 87, 111, 114, 107, 32, 65, 114, 101, 97, 32, 78, 111, 32, 
			46, 46, 46, 46, 46, 46, 58, 32, 0, 176, 70, 0, 176, 87, 0, 106, 
			7, 83, 101, 108, 101, 99, 116, 0, 12, 1, 12, 1, 72, 20, 2, 36, 
			37, 2, 176, 83, 0, 106, 6, 65, 108, 105, 97, 115, 0, 12, 1, 28, 
			50, 36, 38, 2, 176, 20, 0, 95, 255, 106, 22, 65, 108, 105, 97, 115, 
			32, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 58, 32, 
			0, 176, 87, 0, 106, 6, 65, 108, 105, 97, 115, 0, 12, 1, 72, 20, 
			2, 36, 40, 2, 176, 83, 0, 106, 6, 82, 101, 99, 78, 111, 0, 12, 
			1, 28, 55, 36, 41, 2, 176, 20, 0, 95, 255, 106, 22, 67, 117, 114, 
			114, 101, 110, 116, 32, 82, 101, 99, 110, 111, 32, 46, 46, 46, 46, 46, 
			58, 32, 0, 176, 70, 0, 176, 87, 0, 106, 6, 82, 101, 99, 78, 111, 
			0, 12, 1, 12, 1, 72, 20, 2, 36, 43, 2, 176, 83, 0, 106, 9, 
			100, 98, 70, 105, 108, 116, 101, 114, 0, 12, 1, 28, 53, 36, 44, 2, 
			176, 20, 0, 95, 255, 106, 22, 67, 117, 114, 114, 101, 110, 116, 32, 70, 
			105, 108, 116, 101, 114, 32, 46, 46, 46, 46, 58, 32, 0, 176, 87, 0, 
			106, 9, 100, 98, 70, 105, 108, 116, 101, 114, 0, 12, 1, 72, 20, 2, 
			36, 46, 2, 176, 83, 0, 106, 11, 100, 98, 82, 101, 108, 97, 116, 105, 
			111, 110, 0, 12, 1, 28, 55, 36, 47, 2, 176, 20, 0, 95, 255, 106, 
			22, 82, 101, 108, 97, 116, 105, 111, 110, 32, 69, 120, 112, 46, 32, 46, 
			46, 46, 46, 46, 58, 32, 0, 176, 87, 0, 106, 11, 100, 98, 82, 101, 
			108, 97, 116, 105, 111, 110, 0, 12, 1, 72, 20, 2, 36, 49, 2, 176, 
			83, 0, 106, 9, 73, 110, 100, 101, 120, 79, 114, 100, 0, 12, 1, 28, 
			58, 36, 50, 2, 176, 20, 0, 95, 255, 106, 22, 73, 110, 100, 101, 120, 
			32, 79, 114, 100, 101, 114, 32, 46, 46, 46, 46, 46, 46, 46, 58, 32, 
			0, 176, 70, 0, 176, 87, 0, 106, 9, 73, 110, 100, 101, 120, 79, 114, 
			100, 0, 12, 1, 12, 1, 72, 20, 2, 36, 52, 2, 176, 83, 0, 106, 
			9, 73, 110, 100, 101, 120, 75, 101, 121, 0, 12, 1, 28, 68, 36, 53, 
			2, 176, 20, 0, 95, 255, 106, 22, 65, 99, 116, 105, 118, 101, 32, 75, 
			101, 121, 32, 46, 46, 46, 46, 46, 46, 46, 46, 58, 32, 0, 176, 70, 
			0, 48, 55, 0, 176, 84, 0, 106, 14, 73, 110, 100, 101, 120, 75, 101, 
			121, 40, 32, 48, 32, 41, 0, 12, 1, 112, 0, 12, 1, 72, 20, 2, 
			36, 55, 2, 176, 20, 0, 95, 255, 106, 1, 0, 20, 2, 36, 56, 2, 
			120, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 572 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "</details>", 10 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 574 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<details><summary>", 18 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 575 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushStringConst( " Internal Error Handling Information ", 37 );
	hb_xvmPushInteger( 79 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 576 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<br/></summary>", 15 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 578 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Subsystem Call ....: ", 21 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 579 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "System Code .......: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 580 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Default Status ....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 581 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Description .......: ", 21 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 582 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 583 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Involved File .....: ", 21 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 585 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 586 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Operation .........: ", 21 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 588 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 589 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "OS Error Code .....: ", 21 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00008: ;
	hb_xvmSetLine( 600 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "</details>", 10 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 603 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<details><summary>", 18 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 604 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushStringConst( " Available Memory Variables ", 28 );
	hb_xvmPushInteger( 79 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 605 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<br/></summary>", 15 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 607 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 6 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
lab00009: ;
	hb_xvmSetLine( 609 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 610 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00012;
lab00010: ;
	hb_xvmSetLine( 612 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 613 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "CNDTL", 5 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 614 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "     ", 5 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " TYPE ", 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " [", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 91 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "]", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00011: ;
	hb_xvmSetLine( 610 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00012: ;
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 619 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
lab00013: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 621 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmSetLine( 622 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
lab00014: ;
	hb_xvmSetLine( 625 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "</details>", 10 );
	if( hb_xvmDo( 2 ) ) break;
lab00015: ;
	hb_xvmSetLine( 629 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( STRVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 655 );
	goto lab00009;
lab00001: ;
	hb_xvmSetLine( 657 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 658 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 659 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 660 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushStringConst( "ON", 2 );
	goto lab00008;
lab00005: ;
	hb_xvmPushStringConst( "OFF", 3 );
	goto lab00008;
lab00006: ;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushStringConst( ".T.", 3 );
	goto lab00008;
lab00007: ;
	hb_xvmPushStringConst( ".F.", 3 );
lab00008: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "C", 1 ) )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "M", 1 ) )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "D", 1 ) )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "L", 1 ) )
		{
			hb_stackPop();
			goto lab00004;
		}
		hb_stackPop();
	}
	hb_xvmSetLine( 663 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _LSHOWDETAILERROR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 692 );
	hb_xvmPushStringConst( "_HMG", 4 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 2 );
	hb_xvmSetLine( 693 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 695 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 696 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 699 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HTML_ERRORLOG )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 724 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 726 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 732 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 733 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Harbour MiniGUI Errorlog File", 29 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 734 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 736 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 744 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 745 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 746 );
	hb_xvmPushFuncSymbol( symbols + 98 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 3 ) ) break;
lab00002: ;
	hb_xvmSetLine( 752 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HTML_INI )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 778 );
	hb_xvmLocalSetInt( 3, -1L );
	hb_xvmSetLine( 780 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 785 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 769 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 786 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 788 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushStringConst( "Can`t open errorlog file ", 25 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Error", 5 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 790 );
	hb_xvmPushFuncSymbol( symbols + 100 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 791 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "{{TITLE}}", 9 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 792 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushInteger( 114 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "UTF8", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 793 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "\"windows-1251\"", 14 );
	hb_xvmPushStringConst( "\"utf-8\"", 7 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 798 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 803 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HTML_RAWTEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 829 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 830 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 834 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HTML_LINETEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 860 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 861 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "<BR>", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 865 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HTML_LINE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 888 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 889 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "<HR>", 4 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 893 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HTML_END )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 917 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 94 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 918 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "</BODY></HTML>", 14 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 919 );
	hb_xvmPushFuncSymbol( symbols + 103 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 923 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( __HTML_INSERT_OFFSET )
{
   do {
	hb_xvmSetLine( 945 );
	hb_xvmRetInt( -14L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( __HTML_BODY_TEMPLATE )
{
   do {
	hb_xvmSetLine( 969 );
	hb_xvmPushFuncSymbol( symbols + 104 );
	hb_xvmPushStringConst( "PCFET0NUWVBFIGh0bWw+PGh0bWw+PGhlYWQ+PG1ldGEgY2hhcnNldD0id2luZG93cy0xMjUxIj48dGl0bGU+e3tUSVRMRX19PC90aXRsZT48c3R5bGU+Ym9keXtmb250LWZhbWlseTpzYW5zLXNlcmlmO2JhY2tncm91bmQtY29sb3I6I2ZmZjtmb250LXNpemU6MTAwJTtjb2xvcjojMDAwO3BhZGRpbmc6MTVweH0uc3VtbWFyeSxkZXRhaWxzIHN1bW1hcnl7Y29sb3I6IzA2OTtiYWNrZ3JvdW5kOiNmZmM7Ym9yZGVyOjFweCBzb2xpZCAjOWFmO3BhZGRpbmc6NXB4O21hcmdpbjoxMHB4IDVweDtjdXJzb3I6cG9pbnRlcn0ubGlua3tkaXNwbGF5OmJsb2NrO2JhY2tncm91bmQ6I2NmYzt0ZXh0LWRlY29yYXRpb246bm9uZX1oMXtmb250LWZhbWlseTpzYW5zLXNlcmlmO2ZvbnQtc2l6ZToxNTAlO2NvbG9yOiMwMGM7Zm9udC13ZWlnaHQ6NzAwO2JhY2tncm91bmQtY29sb3I6I2YwZjBmMH0udXBkYXRlZHtmb250LWZhbWlseTpzYW5zLXNlcmlmO2NvbG9yOiNjMDA7Zm9udC1zaXplOjExMCV9Lm5vcm1hbHRleHR7Zm9udC1mYW1pbHk6c2Fucy1zZXJpZjtmb250LXNpemU6MTAwJTtjb2xvcjojMDAwO2ZvbnQtd2VpZ2h0OjQwMDt0ZXh0LXRyYW5zZm9ybTpub25lO3RleHQtZGVjb3JhdGlvbjpub25lfS5sYXJnZS1zZWxlY3R7Zm9udC1zaXplOjEyNSU7cGFkZGluZzo4cHg7bWFyZ2luOjVweDtiYWNrZ3JvdW5kOiNjZGZ9PC9zdHlsZT48c2NyaXB0PmNvbnN0IGZpbHRlckJ5PShyLGUsbCxuPW51bGwpPT57bGV0IHQ9ci5tYXAobCk7dD10LnJlZHVjZSgoZSx0KT0+KHQgaW4gZXx8KGVbdF09MCksZVt0XSsrLGUpLHt9KSx0PU9iamVjdC5lbnRyaWVzKHQpLnJlZHVjZSgoZSxbdCxyXSk9PihlW3RdPVt0LHIsYFske3J9XSAke3R9YF0sZSkse30pO2NvbnN0IGM9ZG9jdW1lbnQucXVlcnlTZWxlY3RvcihlKTtPYmplY3QudmFsdWVzKHQpLnNvcnQoKGUsdCk9PnRbMV0tZVsxXSkuZm9yRWFjaCgoW2UsLHRdKT0+e2NvbnN0IHI9ZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgib3B0aW9uIik7ci52YWx1ZT1lLHIuaW5uZXJUZXh0PXQsYy5hcHBlbmRDaGlsZChyKX0pO2MuYWRkRXZlbnRMaXN0ZW5lcigiY2hhbmdlIixlPT57biYmbihjKTtjb25zdCB0PWUudGFyZ2V0LnZhbHVlO3IuZm9yRWFjaChlPT4oKGUsdCk9Pnt2YXIgcjsibnVsbCIhPT10PyhyPWwoZSksZS5zdHlsZS5kaXNwbGF5PXI9PT10P251bGw6Im5vbmUiLGNvbnNvbGUubG9nKGUsZS5zdHlsZS5kaXNwbGF5KSk6ZS5zdHlsZS5kaXNwbGF5PW51bGx9KShlLHQpKX0pfTtkb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKCJET01Db250ZW50TG9hZGVkIixmdW5jdGlvbihlKXt2YXIgdD1bLi4uZG9jdW1lbnQucXVlcnlTZWxlY3RvckFsbCgiLnJlY29yZCIpXSxyPXQ9PmRvY3VtZW50LnF1ZXJ5U2VsZWN0b3IoIiNleHRyYS1wYW5lbCIpLnF1ZXJ5U2VsZWN0b3JBbGwoInNlbGVjdCIpLmZvckVhY2goZT0+dCE9PWUmJihlLnNlbGVjdGVkSW5kZXg9MCkpO2ZpbHRlckJ5KHQsIiNmaWx0ZXJCeURhdGUiLGU9PmUucXVlcnlTZWxlY3RvcigiLmRhdGUiKS5pbm5lclRleHQsciksZmlsdGVyQnkodCwiI2ZpbHRlckJ5U3RhY2t0cmFjZSIsZT0+ZS5xdWVyeVNlbGVjdG9yKCIuc3RhY2t0cmFjZSIpPy5jaGlsZE5vZGVzWzBdPy5kYXRhLnRyaW0oKSxyKSxmaWx0ZXJCeSh0LCIjZmlsdGVyQnlFcnJvciIsZT0+ZS5xdWVyeVNlbGVjdG9yKCIuZXJyb3IiKS5pbm5lclRleHQucmVwbGFjZSgvKFxyXG58XG58XHIpL2dtLCIiKS5yZXBsYWNlKC9cc3syLH0vZywiICIpLnRyaW0oKSxyKSxkb2N1bWVudC5xdWVyeVNlbGVjdG9yKCIjZXh0cmEtcGFuZWwiKS5zdHlsZS5kaXNwbGF5PW51bGwsY29uc29sZS5sb2coIkRPTUNvbnRlbnRMb2FkZWQiKX0pPC9zY3JpcHQ+PC9oZWFkPjxib2R5PjxoMSBzdHlsZT0idGV4dC1hbGlnbjpjZW50ZXIiPnt7VElUTEV9fTwvaDE+PGRpdiBpZD0iZXh0cmEtcGFuZWwiIHN0eWxlPSJkaXNwbGF5Om5vbmUiPjxzZWxlY3QgYXV0b2NvbXBsZXRlPSJvZmYiIGlkPSJmaWx0ZXJCeURhdGUiIGNsYXNzPSJsYXJnZS1zZWxlY3QiPjxvcHRpb24gc2VsZWN0ZWQ9InNlbGVjdGVkIiBkaXNhYmxlZD0iZGlzYWJsZWQiIHZhbHVlPSJudWxsIj5GaWx0ZXIgYnkgRGF0ZTwvb3B0aW9uPjxvcHRpb24gdmFsdWU9Im51bGwiPkFsbCBkYXRlczwvb3B0aW9uPjwvc2VsZWN0PiA8c2VsZWN0IGF1dG9jb21wbGV0ZT0ib2ZmIiBpZD0iZmlsdGVyQnlTdGFja3RyYWNlIiBjbGFzcz0ibGFyZ2Utc2VsZWN0Ij48b3B0aW9uIHNlbGVjdGVkPSJzZWxlY3RlZCIgZGlzYWJsZWQ9ImRpc2FibGVkIiB2YWx1ZT0ibnVsbCI+RmlsdGVyIGJ5IFN0YWNrVHJhY2U8L29wdGlvbj48b3B0aW9uIHZhbHVlPSJudWxsIj5BbGwgc3RhY2t0cmFjZXM8L29wdGlvbj48L3NlbGVjdD4gPHNlbGVjdCBhdXRvY29tcGxldGU9Im9mZiIgaWQ9ImZpbHRlckJ5RXJyb3IiIGNsYXNzPSJsYXJnZS1zZWxlY3QiPjxvcHRpb24gc2VsZWN0ZWQ9InNlbGVjdGVkIiBkaXNhYmxlZD0iZGlzYWJsZWQiIHZhbHVlPSJudWxsIj5GaWx0ZXIgYnkgRXJyb3I8L29wdGlvbj48b3B0aW9uIHZhbHVlPSJudWxsIj5BbGwgZXJyb3JzPC9vcHRpb24+PC9zZWxlY3Q+PC9kaXY+PC9ib2R5PjwvaHRtbD4=", 3472 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETCPUINFO )
{
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 995 );
	hb_xvmPushInteger( 2 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 996 );
	hb_xvmPushStringConst( "HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0", 46 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 998 );
	hb_xvmPushFuncSymbol( symbols + 105 );
	hb_xvmPushLongLong( HB_LL( 2147483650 ) );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ProcessorNameString", 19 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 999 );
	hb_xvmPushFuncSymbol( symbols + 105 );
	hb_xvmPushLongLong( HB_LL( 2147483650 ) );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "~MHz", 4 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 1001 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETOSERRORDESCRIPTION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSFrame( symbols + 109 );
	hb_xvmSetLine( 1030 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 1031 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushStringConst( "Invalid function number                  ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1032 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushStringConst( "File not found                           ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1033 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushStringConst( "Path not found                           ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1034 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushStringConst( "Too many open files (no handles left)    ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1035 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 5 );
	hb_xvmPushStringConst( "Access denied                            ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1036 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 6 );
	hb_xvmPushStringConst( "Invalid handle                           ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1037 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 7 );
	hb_xvmPushStringConst( "Memory control blocks destroyed          ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1038 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 8 );
	hb_xvmPushStringConst( "Insufficient memory                      ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1039 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 9 );
	hb_xvmPushStringConst( "Invalid memory block address             ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1040 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushStringConst( "Invalid environment                      ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1041 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 11 );
	hb_xvmPushStringConst( "Invalid format                           ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1042 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 12 );
	hb_xvmPushStringConst( "Invalid access code                      ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1043 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 13 );
	hb_xvmPushStringConst( "Invalid data                             ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1044 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 15 );
	hb_xvmPushStringConst( "Invalid drive was specified              ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1045 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 16 );
	hb_xvmPushStringConst( "Attempt to remove the current directory  ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1046 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 17 );
	hb_xvmPushStringConst( "Not same device                          ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1047 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 18 );
	hb_xvmPushStringConst( "No more files                            ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1048 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 19 );
	hb_xvmPushStringConst( "Attempt to write to write-protected media", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1049 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 20 );
	hb_xvmPushStringConst( "Unknown unit                             ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1050 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 21 );
	hb_xvmPushStringConst( "Drive not ready                          ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1051 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 22 );
	hb_xvmPushStringConst( "Unknown command                          ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1052 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 23 );
	hb_xvmPushStringConst( "Data CRC error                           ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1053 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 24 );
	hb_xvmPushStringConst( "Bad request structure length             ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1054 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 25 );
	hb_xvmPushStringConst( "Seek error                               ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1055 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 26 );
	hb_xvmPushStringConst( "Unknown media type                       ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1056 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 27 );
	hb_xvmPushStringConst( "Sector not found                         ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1057 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 28 );
	hb_xvmPushStringConst( "Printer out of paper                     ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1058 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 29 );
	hb_xvmPushStringConst( "Write fault                              ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1059 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 30 );
	hb_xvmPushStringConst( "Read fault                               ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1060 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 31 );
	hb_xvmPushStringConst( "General failure                          ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1061 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 32 );
	hb_xvmPushStringConst( "Sharing violation                        ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1062 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 33 );
	hb_xvmPushStringConst( "Lock violation                           ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1063 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 34 );
	hb_xvmPushStringConst( "Invalid disk change                      ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1064 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 35 );
	hb_xvmPushStringConst( "FCB unavailable                          ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1065 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 36 );
	hb_xvmPushStringConst( "Sharing buffer overflow                  ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1066 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 38 );
	hb_xvmPushStringConst( "Unable to complete the operation         ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1067 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 50 );
	hb_xvmPushStringConst( "Network request not supported            ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1068 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 51 );
	hb_xvmPushStringConst( "Remote computer not listening            ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1069 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 52 );
	hb_xvmPushStringConst( "Duplicate name on network                ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1070 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 53 );
	hb_xvmPushStringConst( "Network path not found                   ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1071 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 54 );
	hb_xvmPushStringConst( "Network busy                             ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1072 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 55 );
	hb_xvmPushStringConst( "Network device no longer exists          ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1073 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 56 );
	hb_xvmPushStringConst( "NETBIOS command limit exceeded           ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1074 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 57 );
	hb_xvmPushStringConst( "System error, NETBIOS error              ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1075 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 58 );
	hb_xvmPushStringConst( "Incorrect response from network          ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1076 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 59 );
	hb_xvmPushStringConst( "Unexpected network error                 ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1077 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 60 );
	hb_xvmPushStringConst( "Incompatible remote adapter              ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1078 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 61 );
	hb_xvmPushStringConst( "Print queue full                         ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1079 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 62 );
	hb_xvmPushStringConst( "Not enough space for print file          ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1080 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 63 );
	hb_xvmPushStringConst( "Print file was cancelled                 ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1081 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 64 );
	hb_xvmPushStringConst( "Network name was denied                  ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1082 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 65 );
	hb_xvmPushStringConst( "Access denied                            ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1083 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 66 );
	hb_xvmPushStringConst( "Network device type incorrect            ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1084 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 67 );
	hb_xvmPushStringConst( "Network name not found                   ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1085 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 68 );
	hb_xvmPushStringConst( "Network name limit exceeded              ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1086 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 69 );
	hb_xvmPushStringConst( "NETBIOS session limit exceeded           ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1087 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 70 );
	hb_xvmPushStringConst( "Sharing temporarily paused               ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1088 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 71 );
	hb_xvmPushStringConst( "Network request not accepted             ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1089 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 72 );
	hb_xvmPushStringConst( "Print or disk redirection is paused      ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1090 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 80 );
	hb_xvmPushStringConst( "File exists                              ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1091 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 82 );
	hb_xvmPushStringConst( "Cannot make directory entry              ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1092 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 83 );
	hb_xvmPushStringConst( "Fail on INT 24                           ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1093 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 84 );
	hb_xvmPushStringConst( "Too many redirections                    ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1094 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 85 );
	hb_xvmPushStringConst( "Duplicate redirection                    ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1095 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 86 );
	hb_xvmPushStringConst( "Invalid password                         ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1096 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 87 );
	hb_xvmPushStringConst( "Invalid parameter                        ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1097 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 88 );
	hb_xvmPushStringConst( "Network data fault                       ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1098 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 89 );
	hb_xvmPushStringConst( "Function not supported by network        ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1099 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushInteger( 90 );
	hb_xvmPushStringConst( "Required system component not installed  ", 41 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 1102 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushStatic( 2 );
	{
		static const HB_BYTE codeblock[ 14 ] = {
			1, 0, 1, 0, 1, 0, 95, 1, 122, 1, 95, 255, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 1103 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "=", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 107 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 1106 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "=Unknown error", 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETERRORLOGFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1112 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "ErrorLog.htm", 12 );
	if( hb_xvmPlus() ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	if( hb_xvmPushMemvar( symbols + 54 ) ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 1114 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 109, 2 );
	hb_xvmSFrame( symbols + 109 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopStatic( 1 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopStatic( 2 );
	/* *** END PROC *** */
   } while( 0 );
}

