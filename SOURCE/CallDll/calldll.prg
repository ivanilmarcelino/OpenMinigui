/*
 * Harbour Project source code:
 * CALLDLL compatibility library.
 *
 * This file provides functionality to dynamically load and call functions from 
 * external DLLs (Dynamic Link Libraries) within the Harbour environment.
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * https://harbour.github.io/
 *
 * This software is licensed under the GNU General Public License version 2 or 
 * any later version, with special exceptions for linking Harbour libraries 
 * with other files to produce an executable.
 *
 */

#include "hbdyn.ch"  // Include Harbour dynamic library handling constants

// Static variables to store loaded DLLs and a mutex for thread-safe operations
STATIC s_hDLL := { => }             // Stores a map of loaded DLLs
STATIC s_mutex := hb_mutexCreate()  // Mutex for synchronization when loading/unloading DLLs

/*
 * PROCEDURE UnloadAllDll()
 *
 * Description:
 *   Unloads all dynamically loaded DLLs from memory. This is typically called
 *   when the application is closing to release resources held by the DLLs.
 *
 * Purpose:
 *   This procedure ensures that all DLLs loaded during the application's
 *   lifetime are properly unloaded when the application terminates. This
 *   prevents memory leaks and ensures a clean shutdown.
 *
 * Notes:
 *   This procedure relies on the Harbour runtime to properly unload the DLLs
 *   when they are removed from the s_hDLL map. The hb_libFree() function is implicitly
 *   called by Harbour when the DLL handle is removed from the map.
 */
PROCEDURE UnloadAllDll()

   hb_mutexLock( s_mutex )   // Lock the mutex to ensure safe access
   s_hDLL := { => }          // Clear the DLL map (unload all DLLs)
   hb_mutexUnlock( s_mutex ) // Unlock the mutex

RETURN

/*
 * FUNCTION HMG_CallDLL( cLibName, [ nRetType ], cFuncName [, Arg1, ..., ArgN ] )
 *
 * Description:
 *   Dynamically loads a DLL (if not already loaded) and calls a specified function
 *   within that DLL.
 *
 * Parameters:
 *   - cLibName: The name of the DLL file (string). This should be the base name
 *     (e.g., "MyDLL") without the file extension (e.g., ".dll").
 *   - nRetType: (Optional) A numeric code specifying the data type of the return value
 *     from the DLL function. This corresponds to the HB_DYN_CTYPE_* constants
 *     defined in hbdyn.ch. If omitted, the default return type is used.
 *   - cFuncName: The name of the function to call within the DLL (string).
 *   - Arg1, ..., ArgN: (Optional) A variable number of arguments to pass to the DLL function.
 *     The data types of these arguments must be compatible with the expected types
 *     of the DLL function's parameters.
 *
 * Returns:
 *   The return value of the called DLL function. The data type of the return value
 *   depends on the nRetType parameter. If the function fails (e.g., invalid
 *   parameters, DLL not found, function not found), it returns NIL.
 *
 * Purpose:
 *   This function provides a bridge between the HMG Extended environment and
 *   external DLLs. It allows HMG applications to leverage functionality
 *   provided by external libraries, such as operating system APIs, third-party
 *   components, or specialized algorithms.
 *
 *   Example Usage:
 *   // Call a function in "MyDLL.dll" that returns an integer and takes two integer arguments.
 *   LOCAL nResult := HMG_CallDLL( "MyDLL", HB_DYN_CTYPE_INTEGER, "MyFunction", 10, 20 )
 *
 * Notes:
 *   - The DLL must be located in a directory that is in the system's search path
 *     or in the same directory as the HMG Extended executable.
 *   - The nRetType parameter is crucial for correctly interpreting the return value
 *     from the DLL function. Incorrectly specifying the return type can lead to
 *     unexpected results or application crashes.
 *   - This function uses a mutex to ensure thread-safe access to the DLL loading
 *     and unloading mechanisms, which is important in multi-threaded applications.
 *   - The function automatically appends "W" or "A" to the function name if the
 *     current code page is Unicode or ANSI, respectively, and if the corresponding
 *     function exists in the DLL. This allows for seamless handling of Unicode
 *     and ANSI versions of DLL functions.
 */
FUNCTION HMG_CallDLL( cLibName, nRetType, cFuncName, ... )

   LOCAL nEncoding := iif( HMG_IsCurrentCodePageUnicode(), HB_DYN_ENC_UTF16, HB_DYN_ENC_ASCII )  // Set encoding based on current code page
   LOCAL pLibrary

   IF HB_ISSTRING( cFuncName ) .AND. HB_ISSTRING( cLibName )  // Check if library and function names are valid strings
      hb_mutexLock( s_mutex )  // Lock mutex for thread safety

      // Load the DLL if it's not already loaded
      IF !( cLibName $ s_hDLL )
         s_hDLL[ cLibName ] := hb_libLoad( cLibName )  // Load the DLL and store in map
      ENDIF

      pLibrary := s_hDLL[ cLibName ]  // Get reference to the loaded library

      hb_mutexUnlock( s_mutex )  // Unlock mutex after loading

      // Set default return type if not provided
      IF .NOT. HB_ISNUMERIC( nRetType )
         nRetType := HB_DYN_CTYPE_DEFAULT
      ENDIF

      cFuncName := AllTrim( cFuncName )  // Trim whitespace from function name

      // Check if the function is available with the "W" (Unicode) or "A" (ANSI) suffix
      DO CASE
      CASE HMG_IsCurrentCodePageUnicode() == .T. .AND. HMG_IsFuncDLL( pLibrary, cFuncName + "W" )
         cFuncName := cFuncName + "W"  // Use Unicode version of the function if available
      CASE HMG_IsCurrentCodePageUnicode() == .F. .AND. HMG_IsFuncDLL( pLibrary, cFuncName + "A" )
         cFuncName := cFuncName + "A"  // Use ANSI version of the function if available
      ENDCASE

      // Call the DLL function with the specified parameters
      RETURN hb_DynCall( { cFuncName, pLibrary, hb_bitOr( HB_DYN_CALLCONV_STDCALL, nRetType, nEncoding, HB_DYC_OPT_NULLTERM ) }, ... )
   ENDIF

RETURN NIL  // Return NIL if inputs are invalid

/*
 * FUNCTION HMG_IsCurrentCodePageUnicode()
 *
 * Description:
 *   Determines whether the current code page setting in the HMG Extended environment
 *   is Unicode (UTF-8).
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   .T. (TRUE) if the current code page is Unicode (UTF-8), .F. (FALSE) otherwise.
 *
 * Purpose:
 *   This function is used to determine whether to use Unicode or ANSI versions of
 *   DLL functions when calling them using HMG_CallDLL.  Many Windows APIs have
 *   separate versions for ANSI and Unicode strings, and this function ensures
 *   that the correct version is called based on the application's code page.
 */
FUNCTION HMG_IsCurrentCodePageUnicode()
RETURN ( "UTF8" $ Set( _SET_CODEPAGE ) )  // Check if UTF-8 is part of the current code page setting

/*
 * HB_FUNC( HMG_ISFUNCDLL )
 *
 * Description:
 *   Checks if a specific function exists within a given DLL. This function is
 *   implemented in C code for performance reasons and direct access to Windows API.
 *
 * Parameters:
 *   - pLibDLL or cLibName: Either a handle to a loaded DLL (HMODULE) or the name
 *     of the DLL file (string). If a string is provided, the function attempts
 *     to load the DLL.
 *   - cFuncName: The name of the function to check for within the DLL (string).
 *
 * Returns:
 *   .T. (TRUE) if the function exists in the DLL, .F. (FALSE) otherwise.
 *
 * Purpose:
 *   This function is used to verify the existence of a function in a DLL before
 *   attempting to call it using HMG_CallDLL. This can help prevent runtime errors
 *   (e.g., calling a function that doesn't exist) and improve the robustness
 *   of the application.
 *
 * Notes:
 *   - If the DLL is loaded by name (cLibName), it is automatically unloaded after
 *     the function check is complete. This prevents the DLL from remaining loaded
 *     unnecessarily.
 *   - This function uses the Windows API functions LoadLibrary, GetProcAddress,
 *     and FreeLibrary. These functions provide direct access to the operating
 *     system's DLL loading and function lookup mechanisms.
 */
#pragma BEGINDUMP

#include <windows.h>   // Windows API functions
#include "hbapi.h"     // Harbour API functions

HB_FUNC ( HMG_ISFUNCDLL )
{
   HMODULE hModule;    // Handle to the DLL module
   BOOL bRelease;      // Flag to indicate if the DLL should be unloaded
   char * cFuncName;   // Name of the function to search for

   // If the first parameter is a string, load the DLL by name
   if ( HB_ISCHAR( 1 ) )
   {  
      hModule = LoadLibrary( ( char * ) hb_parc( 1 ) );  // Load the DLL
      bRelease = TRUE;  // Mark for release (unload after checking)
   }
   else  // Otherwise, assume it's a DLL handle
   {  
      hModule = ( HMODULE ) hb_libHandle( hb_param( 1, HB_IT_ANY ) );  // Get DLL handle
      bRelease = FALSE;  // No need to release
   }

   cFuncName = ( char * ) hb_parc( 2 );  // Get the function name

   // Check if the function exists in the DLL
   hb_retl( GetProcAddress( hModule, cFuncName ) ? HB_TRUE : HB_FALSE );

   // If DLL was loaded dynamically, free it after the check
   if( bRelease && hModule )
      FreeLibrary( hModule );
}

#pragma ENDDUMP
