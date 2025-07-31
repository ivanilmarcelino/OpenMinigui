/*
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This    program  is  free  software;  you can redistribute it and/or modify
   it under  the  terms  of the GNU General Public License as published by the
   Free  Software   Foundation;  either  version 2 of the License, or (at your
   option) any later version.

   This   program   is   distributed  in  the hope that it will be useful, but
   WITHOUT    ANY    WARRANTY;    without   even   the   implied  warranty  of
   MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You   should  have  received a copy of the GNU General Public License along
   with   this   software;   see  the  file COPYING. If not, write to the Free
   Software   Foundation,   Inc.,   59  Temple  Place,  Suite  330, Boston, MA
   02111-1307 USA (or visit the web site http://www.gnu.org/).

   As   a   special  exception, you have permission for additional uses of the
   text  contained  in  this  release  of  Harbour Minigui.

   The   exception   is that,   if   you  link  the  Harbour  Minigui  library
   with  other    files   to  produce   an   executable,   this  does  not  by
   itself   cause  the   resulting   executable    to   be  covered by the GNU
   General  Public  License.  Your    use  of that   executable   is   in   no
   way  restricted on account of linking the Harbour-Minigui library code into
   it.

   Parts of this project are based upon:

    "Harbour GUI framework for Win32"
    Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
    Copyright 2001 Antonio Linares <alinares@fivetech.com>
    www - https://harbour.github.io/

    "Harbour Project"
    Copyright 1999-2025, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

   Parts of this code are contributed and used here under permission of the
   author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */

// Set Windows version to 4.10, allowing for compatibility with Windows features at this version
#define WINVER 0x0410

#include <mgdefs.h>  // Include MiniGUI definitions
#include "shlwapi.h" // Include Shell Light-weight Utility API (e.g., for file operations)
#include "hbinit.h"  // Harbour initialization definitions
#include "hbvm.h"    // Harbour VM definitions

// Include GDI+ (Graphics Device Interface) functionality for graphics-related operations
#define _HMG_STUB_
#include "hbgdiplus.h"
#undef _HMG_STUB_

// Macro for creating a version number from major and minor version parts
#define PACKVERSION( major, minor ) MAKELONG( minor, major )

// External function prototypes for error handling and GDI+ initialization
extern void       hmg_ErrorExit( LPCTSTR lpMessage, DWORD dwError, BOOL bExit );
extern GpStatus   GdiplusInit( void );

// Function prototypes for system instance and loading libraries
HINSTANCE         GetInstance( void );
HMODULE           hmg_LoadLibrarySystem( LPCTSTR pFileName );

// Auxiliary string handling functions
TCHAR             *hmg_tstrdup( const TCHAR *pszText );  // Duplicate string
TCHAR             *hmg_tstrncat( TCHAR *pDest, const TCHAR *pSource, HB_SIZE nLen );   // Concatenate strings with length limit
HB_SIZE           hmg_tstrlen( const TCHAR *pText );                    // Get string length

// Internal utility functions for DLL versioning and path handling
static DWORD      DllGetVersion( LPCTSTR lpszDllName );                 // Get DLL version
static TCHAR      *hmg_FileNameAtSystemDir( const TCHAR *pFileName );   // Get DLL path in system directory

// Type definition for the DLL version retrieval function
typedef HRESULT ( CALLBACK *_DLLGETVERSIONPROC ) ( DLLVERSIONINFO2 * );

// Global variables to store instance handle and DLL version
static HINSTANCE  g_hInstance = NULL;
static DWORD      g_dwComCtl32Ver = 0;

/*
 * FUNCTION: hmg_init( [void *cargo] )
 *
 * Initializes core components of the HMG (Harbour MiniGUI Extended) runtime,
 * including COM, application instance handle, and GDI+.
 *
 * Parameters:
 *   cargo : Optional. Used by the Harbour runtime for initialization callbacks.
 *           This parameter is unused and ignored in this context.
 *
 * Returns:
 *   void. This function has side effects: it initializes global variables and
 *   system-level components required for GUI functionality.
 *
 * Purpose:
 *   This startup routine ensures that critical subsystems like COM and GDI+ are
 *   initialized before any GUI operations occur. It's designed to run automatically
 *   when the application starts.
 */
#ifdef __XHARBOUR__
static void hmg_init ( void )
#else
static void hmg_init ( void *cargo )
#endif
{
   LPCTSTR  lpszDllName = TEXT( "ComCtl32.dll" );  // Common Controls Library
#ifndef __XHARBOUR__
   HB_SYMBOL_UNUSED( cargo ); // Unused parameter, only required for specific Harbour versions
#endif

   // Initialize COM library for OLE (Object Linking and Embedding)
   if( S_FALSE == CoInitializeEx( NULL, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE | COINIT_SPEED_OVER_MEMORY ) )
   {
      hmg_ErrorExit( TEXT( "hmg_init( void )" ), S_FALSE, TRUE ); // Handle initialization error
   }

   // Get the version of ComCtl32.dll
   g_dwComCtl32Ver = DllGetVersion( lpszDllName );

   // Get application instance
   GetInstance();

   // Initialize GDI+ for graphics support, handle errors if initialization fails
   if( Ok != GdiplusInit() )
   {
      hmg_ErrorExit( TEXT( "GdiplusInit( void )" ), 0, TRUE );
   }
}

// Define startup routine for HMG, adjusting for different Harbour variants
#ifdef __XHARBOUR__
HB_CALL_ON_STARTUP_BEGIN( _hmg_init_ )
hmg_init();
HB_CALL_ON_STARTUP_END( _hmg_init_ )
#else
HB_CALL_ON_STARTUP_BEGIN( _hmg_init_ )
hb_vmAtInit( hmg_init, NULL );
HB_CALL_ON_STARTUP_END( _hmg_init_ )
#endif

// Compiler-specific setup for startup routines
#if defined( HB_PRAGMA_STARTUP )
#pragma startup _hmg_init_
#elif defined( HB_DATASEG_STARTUP )
#define HB_DATASEG_BODY HB_DATASEG_FUNC( _hmg_init_ )
#include "hbiniseg.h"
#endif

/*
 * FUNCTION: GetInstance()
 *
 * Retrieves the application's instance handle, caching it globally.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   HINSTANCE - Handle to the current application module.
 *
 * Purpose:
 *   Used by GUI functions and control creation APIs to associate window classes
 *   with the running application.
 *
 * Notes:
 *   - Uses lazy initialization pattern.
 *   - Value is stored in g_hInstance for reuse.
 */
HINSTANCE GetInstance( void )
{
   if( !g_hInstance )
   {
      g_hInstance = GetModuleHandle( 0 );
   }

   return g_hInstance;
}

/*
 * FUNCTION: DllGetVersion( LPCTSTR lpszDllName )
 *
 * Queries the version of a specified DLL using its DllGetVersion export.
 *
 * Parameters:
 *   lpszDllName : Name of the DLL (e.g., "comctl32.dll")
 *
 * Returns:
 *   DWORD - Packed version number using MAKELONG(minor, major).
 *           Returns 0 if version information is unavailable.
 *
 * Purpose:
 *   Provides version detection for system DLLs to enable or disable features
 *   based on their availability.
 *
 * Notes:
 *   - Safely handles missing DLLs or missing version exports.
 *   - Frees DLL handle after use.
 */
static DWORD DllGetVersion( LPCTSTR lpszDllName )
{
   HINSTANCE   hinstDll;
   DWORD       dwVersion = 0;

   hinstDll = hmg_LoadLibrarySystem( lpszDllName );               // Load the specified DLL
   if( hinstDll )
   {
      _DLLGETVERSIONPROC   pDllGetVersion;
      pDllGetVersion = ( _DLLGETVERSIONPROC ) wapi_GetProcAddress( hinstDll, "DllGetVersion" ); // Get DllGetVersion function address
      if( pDllGetVersion )
      {
         DLLVERSIONINFO2   dvi;
         HRESULT           hr;

         ZeroMemory( &dvi, sizeof( DLLVERSIONINFO2 ) );  // Initialize version info structure to zero
         dvi.info1.cbSize = sizeof( dvi );               // Set structure size
         hr = ( *pDllGetVersion ) ( &dvi );              // Call DllGetVersion
         if( S_OK == hr )
         {
            dwVersion = PACKVERSION( dvi.info1.dwMajorVersion, dvi.info1.dwMinorVersion );   // Create packed version number
         }
      }

      FreeLibrary( hinstDll );   // Free the loaded DLL
   }

   return dwVersion;
}

/*
 * FUNCTION: GETINSTANCE()
 *
 * Harbour wrapper for GetInstance(), returns the HINSTANCE.
 */
HB_FUNC( GETINSTANCE )
{
   hmg_ret_raw_HANDLE( g_hInstance );
}

/*
 * FUNCTION: GETCOMCTL32DLLVER()
 *
 * Returns the version of ComCtl32.dll (common controls).
 */
HB_FUNC( GETCOMCTL32DLLVER )
{
   hmg_ret_DWORD( g_dwComCtl32Ver );
}

/*
 * FUNCTION: OLEDATARELEASE()
 *
 * Uninitializes the COM library.
 * Should be called during application shutdown.
 */
HB_FUNC( OLEDATARELEASE )
{
   CoUninitialize();
}

/*
 * FUNCTION: win_has_search_system32()
 *
 * Checks if the current system supports LOAD_LIBRARY_SEARCH_SYSTEM32 security flag.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   HB_BOOL - TRUE if supported, FALSE otherwise.
 *
 * Purpose:
 *   Enables secure DLL loading when available on Windows 7+ with KB2533623.
 */
#ifndef LOAD_LIBRARY_SEARCH_SYSTEM32
#define LOAD_LIBRARY_SEARCH_SYSTEM32   0x00000800
#endif
static HB_BOOL win_has_search_system32( void )
{
   HMODULE  hKernel32 = GetModuleHandle( TEXT( "kernel32.dll" ) );

   if( hKernel32 )
   {
      return GetProcAddress( hKernel32, "AddDllDirectory" ) != NULL; // Detects KB2533623 availability
   }

   return HB_FALSE;
}

/*
 * FUNCTION: hmg_LoadLibrarySystem( LPCTSTR pFileName )
 *
 * Loads a DLL from the system directory with the safest possible search strategy.
 *
 * Parameters:
 *   pFileName : Name of the DLL file (e.g., "comctl32.dll")
 *
 * Returns:
 *   HMODULE - Module handle if successful, NULL otherwise.
 *
 * Purpose:
 *   Ensures system libraries are loaded from the correct directory, avoiding
 *   DLL preloading attacks.
 */
HMODULE hmg_LoadLibrarySystem( LPCTSTR pFileName )
{
   TCHAR    *pLibPath = hmg_FileNameAtSystemDir( pFileName );        // Get full path for system directory
   HMODULE  h = LoadLibraryEx( pLibPath, NULL, win_has_search_system32() ? LOAD_LIBRARY_SEARCH_SYSTEM32 : LOAD_WITH_ALTERED_SEARCH_PATH );

   hb_xfree( pLibPath );   // Free allocated memory
   return h;
}

/*
 * FUNCTION: hmg_FileNameAtSystemDir( const TCHAR *pFileName )
 *
 * Builds a fully qualified path to a file in the system directory.
 *
 * Parameters:
 *   pFileName : Name of the file to locate within the system directory.
 *
 * Returns:
 *   TCHAR* - Dynamically allocated string containing full path.
 *            Caller is responsible for freeing memory with hb_xfree().
 *
 * Purpose:
 *   Ensures DLLs and executables are referenced from the secure system location.
 *
 * Notes:
 *   - If GetSystemDirectory fails, duplicates and returns the original file name.
 */
static TCHAR *hmg_FileNameAtSystemDir( const TCHAR *pFileName )
{
   UINT  nLen = GetSystemDirectory( NULL, 0 );

   if( nLen )
   {
      LPTSTR   buffer;

      if( pFileName )
      {
         nLen += ( UINT ) hmg_tstrlen( pFileName ) + 1;        // Calculate total path length
      }

      buffer = ( LPTSTR ) hb_xgrab( nLen * sizeof( TCHAR ) );  // Allocate buffer
      GetSystemDirectory( buffer, nLen );                // Retrieve system directory
      if( pFileName )
      {
         hmg_tstrncat( buffer, TEXT( "\\" ), nLen - 1 ); // Append backslash
         hmg_tstrncat( buffer, pFileName, nLen - 1 );    // Append file name
      }

      return buffer;
   }
   else
   {
      return hmg_tstrdup( pFileName );                   // Return duplicate if system directory retrieval fails
   }
}

/*
 * FUNCTION: hmg_tstrdup()
 *
 * Duplicates a TCHAR string using dynamic allocation.
 */
TCHAR *hmg_tstrdup( const TCHAR *pszText )
{
   TCHAR    *pszDup;
   HB_SIZE  nLen;

   nLen = ( hmg_tstrlen( pszText ) + 1 ) * sizeof( TCHAR );

   pszDup = ( TCHAR * ) hb_xgrab( nLen );
   memcpy( pszDup, pszText, nLen );

   return pszDup;
}

/*
 * FUNCTION: hmg_tstrncat()
 *
 * Concatenates a source string to a destination buffer, up to a maximum length.
 */
TCHAR *hmg_tstrncat( TCHAR *pDest, const TCHAR *pSource, HB_SIZE nLen )
{
   TCHAR *pBuf = pDest;

   pDest[nLen] = TEXT( '\0' );   // Ensure null termination
   while( nLen && *pDest )
   {
      pDest++;
      nLen--;
   }

   while( nLen && ( *pDest++ = *pSource++ ) != TEXT( '\0' ) )
   {
      nLen--;
   }

   return pBuf;
}

// Computes the length of a string
HB_SIZE hmg_tstrlen( const TCHAR *pText )
{
   HB_SIZE  nLen = 0;

   while( pText[nLen] != TEXT( '\0' ) )
   {
      ++nLen;
   }

   return nLen;
}
