/*
 * Harbour 3.2.0dev (r2503200530)
 * Borland C++ 5.8.2 (32-bit)
 * Generated C source from "h_checkres.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( MGADDRESOURCE );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( PROCNAME );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( PROCLINE );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( AADD );
HB_FUNC( MGDELRESOURCE );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( HB_ADEL );
HB_FUNC( CHECKRES );
HB_FUNC_EXTERN( _SETGETLOGFILE );
HB_FUNC_EXTERN( GETSTARTUPFOLDER );
HB_FUNC_EXTERN( HB_PS );
HB_FUNC_EXTERN( FERASE );
HB_FUNC_EXTERN( HB_PROGNAME );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC_EXTERN( _LOGFILE );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_CHECKRES )
{ "MGADDRESOURCE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MGADDRESOURCE )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "PROCNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCNAME )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "PROCLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCLINE )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "MGDELRESOURCE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MGDELRESOURCE )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "HB_ADEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ADEL )}, NULL },
{ "CHECKRES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CHECKRES )}, NULL },
{ "_SETGETLOGFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETLOGFILE )}, NULL },
{ "GETSTARTUPFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSTARTUPFOLDER )}, NULL },
{ "HB_PS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PS )}, NULL },
{ "FERASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERASE )}, NULL },
{ "HB_PROGNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PROGNAME )}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "_LOGFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _LOGFILE )}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_CHECKRES, "h_checkres.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_CHECKRES
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_CHECKRES )
   #include "hbiniseg.h"
#endif

HB_FUNC( MGADDRESOURCE )
{
	static const HB_BYTE pcode[] =
	{
		13,2,2,116,20,0,92,3,80,3,106,1,0,80,
		4,176,1,0,176,2,0,95,3,12,1,12,1,31,
		43,96,4,0,176,2,0,95,3,12,1,106,2,40,
		0,72,176,3,0,176,4,0,95,3,12,1,12,1,
		72,106,4,41,45,62,0,72,135,174,3,0,25,203,
		176,1,0,95,4,12,1,31,22,176,5,0,95,4,
		122,176,6,0,95,4,12,1,92,2,49,12,3,80,
		4,176,7,0,103,1,0,95,2,95,1,95,4,4,
		3,0,20,2,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( MGDELRESOURCE )
{
	static const HB_BYTE pcode[] =
	{
		13,1,1,116,20,0,176,9,0,103,1,0,89,18,
		0,1,0,1,0,1,0,95,1,92,2,1,95,255,
		8,6,12,2,165,80,2,121,69,28,13,176,10,0,
		103,1,0,95,2,120,20,3,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( CHECKRES )
{
	static const HB_BYTE pcode[] =
	{
		13,2,0,116,20,0,106,1,0,80,1,176,12,0,
		176,13,0,12,0,176,14,0,12,0,72,106,13,99,
		104,101,99,107,114,101,115,46,116,120,116,0,72,20,
		1,176,15,0,176,12,0,12,0,20,1,103,1,0,
		96,2,0,129,1,1,28,77,95,2,92,2,1,121,
		69,28,65,96,1,0,176,16,0,12,0,106,5,32,
		45,45,32,0,72,95,2,122,1,72,106,2,44,0,
		72,176,3,0,95,2,92,2,1,12,1,72,106,2,
		44,0,72,95,2,92,3,1,72,176,17,0,12,0,
		72,135,176,18,0,120,95,1,20,2,130,31,183,132,
		176,1,0,95,1,12,1,31,33,176,18,0,120,176,
		16,0,12,0,106,5,32,45,45,32,0,72,176,19,
		0,106,2,61,0,92,99,12,2,72,20,2,100,110,
		7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITSTATICS()
{
	static const HB_BYTE pcode[] =
	{
		117,20,0,1,0,116,20,0,4,0,0,82,1,0,
		7
	};

	hb_vmExecute( pcode, symbols );
}

#line 123 "h_checkres.prg"

#include <windows.h>
#include <hbapiitm.h>
#include <hbvm.h>

/*
 * Function: RegisterResource
 * Purpose: Registers a resource with the Harbour resource tracking system from C code.
 *
 * Parameters:
 *   hRes (HANDLE): The handle of the resource to register.
 *   szType (LPCSTR): A string describing the type of the resource.
 *
 * Return Value:
 *   None (void)
 *
 * Logic:
 *   1.  Prepares to call the Harbour function MGAddResource.
 *   2.  Pushes the necessary parameters onto the Harbour virtual machine stack:
 *       - The function name "MGADDRESOURCE" as a symbol.
 *       - A NIL value (required by Harbour calling convention).
 *       - The resource handle as a numeric integer.
 *       - The resource type as a string.
 *   3.  Calls the MGAddResource function using hb_vmFunction(2) (2 parameters).
 *   4.  Releases the return value item.
 */
void RegisterResource( HANDLE hRes, LPCSTR szType )
{
   PHB_ITEM pRet = hb_itemNew( hb_param( -1, HB_IT_ANY ) ); // Create a new Harbour item for the return value.

   hb_vmPushSymbol( hb_dynsymGetSymbol( "MGADDRESOURCE" ) ); // Push the symbol for the MGADDRESOURCE function.
   hb_vmPushNil(); // Push a NIL value (required by Harbour calling convention).
   hb_vmPushNumInt( ( LONG_PTR ) hRes ); // Push the resource handle as a numeric integer.
   hb_vmPushString( szType, strlen( szType ) ); // Push the resource type as a string.
   hb_vmFunction( 2 ); // Call the MGADDRESOURCE function with 2 parameters.

   hb_itemReturnRelease( pRet ); // Release the return value item.
}

/*
 * Function: DelResource
 * Purpose: Unregisters a resource from the Harbour resource tracking system from C code.
 *
 * Parameters:
 *   hResource (HANDLE): The handle of the resource to unregister.
 *
 * Return Value:
 *   None (void)
 *
 * Logic:
 *   1.  Prepares to call the Harbour function MGDelResource.
 *   2.  Pushes the necessary parameters onto the Harbour virtual machine stack:
 *       - The function name "MGDELRESOURCE" as a symbol.
 *       - A NIL value (required by Harbour calling convention).
 *       - The resource handle as a numeric integer.
 *   3.  Calls the MGDelResource function using hb_vmFunction(1) (1 parameter).
 *   4.  Releases the return value item.
 */
void pascal DelResource( HANDLE hResource )
{
   PHB_ITEM pRet = hb_itemNew( hb_param( -1, HB_IT_ANY ) ); // Create a new Harbour item for the return value.

   hb_vmPushSymbol( hb_dynsymGetSymbol( "MGDELRESOURCE" ) ); // Push the symbol for the MGDELRESOURCE function.
   hb_vmPushNil(); // Push a NIL value (required by Harbour calling convention).
   hb_vmPushNumInt( ( LONG_PTR ) hResource ); // Push the resource handle as a numeric integer.
   hb_vmFunction( 1 ); // Call the MGDELRESOURCE function with 1 parameter.

   hb_itemReturnRelease( pRet ); // Release the return value item.
}

