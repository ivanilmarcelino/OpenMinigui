/*
 *  Resources Control Functions
 *
 *  Purpose: This code provides resource management functions for HMG Extended applications.
 *           It allows tracking of resources (e.g., window handles, GDI objects) allocated by the application
 *           and helps in identifying potential resource leaks.  It provides both Harbour-level functions
 *           and C-level functions to facilitate resource tracking from different parts of the application.
 */

#ifdef __XHARBOUR__
#define __SYSDATA__
#endif
#include "minigui.ch"

STATIC aResources := {} // Array to store resource information. Each element is an array { cType, nHResource, cInfo }.

/*
 *-----------------------------------------------------------------------------*
 * FUNCTION MGAddResource( nHResource, cType )
 *-----------------------------------------------------------------------------*
 * Purpose: Adds a resource to the resource tracking array.
 *
 * Parameters:
 *   nHResource (NUMERIC): The handle of the resource (e.g., HBITMAP for a bitmap).  This is treated as a numeric value representing the memory address of the resource.
 *   cType (CHARACTER): A string describing the type of the resource (e.g., "Icon", "Bitmap").
 *
 * Return Value:
 *   NIL
 *
 * Logic:
 *   1.  Retrieves the call stack information (function names and line numbers) to provide context about where the resource was created.
 *   2.  Adds a new entry to the aResources array containing the resource type, handle, and call stack information.
 *   The call stack information is useful for debugging and identifying the source of resource leaks.
 */
FUNCTION MGAddResource( nHResource, cType )
   LOCAL n := 3, cInfo := "" // Initialize variables: n for call stack depth, cInfo for call stack string.

   WHILE ! Empty( ProcName( n ) ) // Iterate through the call stack until an empty procedure name is encountered.
      cInfo += ProcName( n ) + "(" + hb_ntos( ProcLine( n ) ) + ")->" // Append the procedure name and line number to the call stack information string.
      n++ // Increment the call stack depth.
   END

   IF ! Empty( cInfo ) // If the call stack information is not empty.
      cInfo := SubStr( cInfo, 1, Len( cInfo ) - 2 ) // Remove the trailing "->" from the call stack information string.
   ENDIF

   AAdd( aResources, { cType, nHResource, cInfo } ) // Add the resource information to the aResources array.

RETURN NIL

/*
 *-----------------------------------------------------------------------------*
 * FUNCTION MGDelResource( nHResource )
 *-----------------------------------------------------------------------------*
 * Purpose: Removes a resource from the resource tracking array.
 *
 * Parameters:
 *   nHResource (NUMERIC): The handle of the resource to remove.
 *
 * Return Value:
 *   NIL
 *
 * Logic:
 *   1.  Searches the aResources array for an entry with the matching resource handle.
 *   2.  If found, removes the entry from the array.
 *   The hb_ADel function is used to delete the element at the specified index. The .T. parameter indicates that the array should be re-indexed after the deletion.
 */
FUNCTION MGDelResource( nHResource )
   LOCAL nAt // Variable to store the index of the resource in the array.

   IF ( nAt := AScan( aResources, {| aRes | aRes[ 2 ] == nHResource } ) ) != 0 // Search for the resource in the array.
      hb_ADel( aResources, nAt, .T. ) // If found, delete the resource from the array.
   ENDIF

RETURN NIL

/*
 *-----------------------------------------------------------------------------*
 * FUNCTION CheckRes()
 *-----------------------------------------------------------------------------*
 * Purpose: Checks for potential resource leaks by iterating through the resource tracking array
 *          and logging any resources that have not been released.
 *
 * Parameters:
 *   None
 *
 * Return Value:
 *   NIL
 *
 * Logic:
 *   1.  Sets up a log file named "checkres.txt" in the application's startup folder.
 *   2.  Iterates through the aResources array.
 *   3.  For each resource, if the handle is not 0 (indicating it's still allocated), logs the resource information to the log file.
 *   4.  Adds a separator line to the log file if any unreleased resources were found.
 *   This function is typically called at the end of the application's execution to identify any resources that were not properly released.
 */
FUNCTION CheckRes()
   LOCAL cInfo := "", p // Initialize variables: cInfo for the log information, p for the current resource.

   _SetGetLogFile( GetStartUpFolder() + hb_ps() + "checkres.txt" ) // Set the log file path.
   FErase( _SetGetLogFile() ) // Erase the log file if it exists.

   FOR EACH p IN aResources // Iterate through the aResources array.
      IF p[ 2 ] != 0 // If the resource handle is not 0 (meaning it's still allocated).
         cInfo += GetExeFileName() + " -- " + p[ 1 ] + "," + hb_ntos( p[ 2 ] ) + "," + p[ 3 ] + CRLF // Format the resource information for logging.
         _LogFile( .T., cInfo ) // Write the resource information to the log file.
      ENDIF
   NEXT

   IF ! Empty( cInfo ) // If any unreleased resources were found.
     _LogFile( .T., GetExeFileName() + " -- " + Replicate( "=", 99 ) ) // Write a separator line to the log file.
   ENDIF

RETURN NIL

/*
 * C-level functions for resource management.  These functions provide a way to register and
 * unregister resources directly from C code, which can be useful when working with external libraries
 * or when more fine-grained control over resource management is required.
 */

#pragma BEGINDUMP

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

#pragma ENDDUMP
