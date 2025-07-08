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
STATIC s_hDLL := { => }      // Stores a map of loaded DLLs
STATIC s_mutex := hb_mutexCreate()  // Mutex for synchronization when loading/unloading DLLs

* ===========================================================================
* PROCEDURE UnloadAllDll()
*
* Purpose:
*   Unloads all dynamically loaded DLLs by clearing the `s_hDLL` map, which 
*   holds references to loaded libraries.
*
* Operation:
*   Locks the mutex to ensure thread safety, clears the map of DLLs, and unlocks
*   the mutex.
*
* Returns: 
*   Nothing (procedure).
* ===========================================================================
PROCEDURE UnloadAllDll()

   hb_mutexLock( s_mutex )  // Lock the mutex to ensure safe access
   s_hDLL := { => }         // Clear the DLL map (unload all DLLs)
   hb_mutexUnlock( s_mutex )  // Unlock the mutex

RETURN

* ===========================================================================
* FUNCTION HMG_CallDLL( cLibName, [ nRetType ], cFuncName [, Arg1, ..., ArgN ] )
*
* Purpose:
*   Dynamically loads a DLL and calls a specified function from the DLL.
*
* Parameters:
*   - cLibName: Name of the DLL to load (string).
*   - nRetType: Type of the return value (optional, numeric).
*   - cFuncName: Name of the function to call from the DLL (string).
*   - Arg1, ..., ArgN: Arguments to pass to the DLL function (optional).
*
* Operation:
*   Loads the specified DLL if it hasn't been loaded, finds the function within
*   the DLL, and calls the function with the provided arguments. Handles Unicode 
*   or ASCII function names depending on the current code page.
*
* Returns:
*   The return value of the called DLL function, or NIL if parameters are invalid.
* ===========================================================================
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

* ===========================================================================
* FUNCTION HMG_IsCurrentCodePageUnicode()
*
* Purpose:
*   Checks whether the current code page is Unicode (UTF-8).
*
* Returns:
*   TRUE (.T.) if the current code page is Unicode (UTF-8), FALSE (.F.) otherwise.
* ===========================================================================
FUNCTION HMG_IsCurrentCodePageUnicode()
RETURN ( "UTF8" $ Set( _SET_CODEPAGE ) )  // Check if UTF-8 is part of the current code page setting

* ===========================================================================
* HB_FUNC( HMG_ISFUNCDLL )
*
* Purpose:
*   Checks whether a specified function exists in a given DLL.
*
* Parameters:
*   - pLibDLL or cLibName: A handle to the DLL or the name of the DLL (string).
*   - cFuncName: Name of the function to check within the DLL.
*
* Returns:
*   TRUE if the function exists, FALSE otherwise.
* ===========================================================================
#pragma BEGINDUMP  // Start inline C code

#include <windows.h>  // Windows API functions
#include "hbapi.h"    // Harbour API functions

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

#pragma ENDDUMP  // End inline C code
