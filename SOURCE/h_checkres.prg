/*
 *  Resources Control Functions for HMG Extended
 *
 *  This module implements a centralized resource tracking system. In GUI programming, 
 *  failing to release GDI objects (bitmaps, icons, fonts) or window handles leads 
 *  to memory leaks and system instability. This utility provides a mechanism to 
 *  log every allocation and deallocation, allowing developers to identify 
 *  unreleased resources upon application termination.
 */

#ifdef __XHARBOUR__
   #define __SYSDATA__
#endif

#include "minigui.ch"

/* 
 * Compatibility Layer:
 * This macro ensures the code remains portable across different compiler versions
 * by simulating the behavior of removing characters from the end of a string.
 */
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   #xtranslate hb_StrShrink( <char>, <n> ) => Left( <char>, Len( <char> ) - <n> )
#endif

/* 
 * Thread-local storage for resource tracking.
 * Using THREAD STATIC ensures that in multi-threaded HMG applications, 
 * resource tracking is isolated to the specific thread that created the resource,
 * preventing race conditions and data corruption.
 * Structure: { cType, nHResource, cCallStack }
 */
THREAD STATIC aResources := {} 

/*
 * FUNCTION: MGAddResource
 * -----------------------
 * Registers a new resource handle into the tracking system.
 *
 * Parameters:
 *    nHResource : Numeric - The memory handle (pointer) of the resource.
 *    cType      : String  - A descriptive label (e.g., "BITMAP", "FONT").
 *
 * Implementation Detail:
 *    This function automatically captures the call stack. By starting the 
 *    trace at depth 3, it skips the tracking function itself and its immediate 
 *    wrapper, pointing directly to the developer's code that triggered the allocation.
 */
FUNCTION MGAddResource( nHResource, cType )
   LOCAL n := 3 
   LOCAL cInfo := "" 

   // Traverse the call stack to build a breadcrumb trail for debugging.
   WHILE ! Empty( ProcName( n ) ) 
      // Concatenate function names and line numbers into a readable path.
      cInfo += ProcName( n ) + "(" + hb_ntos( ProcLine( n ) ) + ")->" 
      n++ 
   END

   // Clean up the trailing arrow for a professional log format.
   IF ! Empty( cInfo ) 
      cInfo := hb_StrShrink( cInfo, 2 ) 
   ENDIF

   // Store the resource metadata. The handle is used as the unique identifier for later deletion.
   AAdd( aResources, { cType, nHResource, cInfo } ) 

RETURN NIL

/*
 * FUNCTION: MGDelResource
 * -----------------------
 * Removes a resource from the tracking list, signaling it has been safely released.
 *
 * Parameters:
 *    nHResource : Numeric - The handle of the resource being destroyed.
 *
 * Reasoning:
 *    When an HMG control is destroyed or a GDI object is deleted, this function 
 *    is called to "check out" the resource. If a resource is deleted here, 
 *    it will not appear in the final leak report.
 */
FUNCTION MGDelResource( nHResource )
   LOCAL nAt 

   // Locate the resource by its handle within the tracking array.
   IF ( nAt := AScan( aResources, {| aRes | aRes[ 2 ] == nHResource } ) ) != 0 
      // Use hb_ADel with .T. to physically shrink the array and maintain performance.
      hb_ADel( aResources, nAt, .T. ) 
   ENDIF

RETURN NIL

/*
 * FUNCTION: CheckRes
 * ------------------
 * Analyzes the tracking array and generates a report of unreleased resources.
 *
 * Purpose:
 *    Typically called during the application's shutdown sequence. It creates 
 *    'checkres.txt' in the application folder. If the file contains data, 
 *    it indicates a resource leak that needs developer attention.
 *
 * Side Effects:
 *    - Creates/Overwrites 'checkres.txt'.
 *    - Uses HMG internal logging functions (_SetGetLogFile, _LogFile).
 */
FUNCTION CheckRes()
   // Define the log path relative to the executable location.
   LOCAL cLogFile := GetStartUpFolder() + hb_ps() + "checkres.txt"
   LOCAL cInfo := ""
   LOCAL p 

   // Configure the HMG logging subsystem.
   _SetGetLogFile( cLogFile ) 
   FErase( cLogFile ) 

   // Iterate through all registered resources that were never "deleted".
   FOR EACH p IN aResources 
      // A non-zero handle indicates the resource is still occupying system memory.
      IF p[ 2 ] != 0 
         // Format: ExeName -- Type, Handle, CallStack
         cInfo += GetExeFileName() + " -- " + p[ 1 ] + "," + hb_ntos( p[ 2 ] ) + "," + p[ 3 ] + CRLF 
      ENDIF
   NEXT

   // If leaks are detected, write them to the file with a visual separator.
   IF ! Empty( cInfo ) 
      _LogFile( .T., cInfo ) 
      _LogFile( .T., GetExeFileName() + " -- " + Replicate( "=", 99 ) ) 
   ENDIF

RETURN NIL

/*
 * C-Level Interface
 * -----------------
 * These functions bridge the gap between the Windows API (C) and Harbour.
 * Many HMG resources are created within C wrappers; these functions allow 
 * those low-level routines to participate in the high-level tracking system.
 */

#pragma BEGINDUMP

#include <windows.h>
#include <hbapiitm.h>
#include <hbvm.h>

/*
 * C-FUNCTION: RegisterResource
 * ----------------------------
 * Allows C-level code to invoke the Harbour MGAddResource function.
 *
 * Parameters:
 *    hRes   : The Windows HANDLE (HWND, HBITMAP, etc.)
 *    szType : The string description of the resource.
 *
 * Logic:
 *    Uses the Harbour Virtual Machine (VM) API to push arguments onto the 
 *    eval stack and execute the Harbour-level function by its symbol name.
 */
void RegisterResource( HANDLE hRes, LPCSTR szType )
{
   // Create a placeholder for the Harbour return value.
   PHB_ITEM pRet = hb_itemNew( hb_param( -1, HB_IT_ANY ) );  

   // Prepare the VM to call MGADDRESOURCE.
   hb_vmPushSymbol( hb_dynsymGetSymbol( "MGADDRESOURCE" ) ); 
   hb_vmPushNil();                                           
   // Cast the handle to a pointer-sized integer to ensure 32/64-bit compatibility.
   hb_vmPushNumInt( ( LONG_PTR ) hRes );                     
   hb_vmPushString( szType, strlen( szType ) );              
   // Execute the function with 2 arguments.
   hb_vmFunction( 2 );                                       

   // Clean up the item reference to prevent memory leaks in the C-to-Harbour bridge.
   hb_itemReturnRelease( pRet );                             
}

/*
 * C-FUNCTION: DelResource
 * -----------------------
 * Allows C-level code to invoke the Harbour MGDelResource function.
 *
 * Parameters:
 *    hResource : The Windows HANDLE to be removed from tracking.
 *
 * Logic:
 *    Similar to RegisterResource, this uses the hb_vm API to notify the 
 *    Harbour tracking array that a resource has been freed at the C level.
 */
void pascal DelResource( HANDLE hResource )
{
   PHB_ITEM pRet = hb_itemNew( hb_param( -1, HB_IT_ANY ) );  

   hb_vmPushSymbol( hb_dynsymGetSymbol( "MGDELRESOURCE" ) ); 
   hb_vmPushNil();                                           
   hb_vmPushNumInt( ( LONG_PTR ) hResource );                
   // Execute the function with 1 argument.
   hb_vmFunction( 1 );                                       

   hb_itemReturnRelease( pRet );                             
}

#pragma ENDDUMP