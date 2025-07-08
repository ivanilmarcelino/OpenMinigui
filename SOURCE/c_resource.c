/* MINIGUI - Harbour Win32 GUI library source code

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

   Parts  of  this  code  is contributed and used here under permission of his
   author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */
#include <mgdefs.h>
#include "hbapifs.h"                 // Include Harbour API for file system functions.

// Function prototype to get the application instance handle.
HINSTANCE         GetInstance( void );

// Prototype for function to convert ANSI to wide character strings (Unicode).
#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );
#endif

// Global variables
static HINSTANCE  hResources = 0;                     // Handle to the currently loaded resources DLL.
static HINSTANCE  HMG_DllStore[256];

// Array to store handles of up to 256 loaded DLLs.

// Function to load a DLL and manage it in HMG_DllStore
static HINSTANCE HMG_LoadDll( char *DllName )
{
   static int  DllCnt;                                // Counter to track the index for storing DLL handles in HMG_DllStore.
#ifndef UNICODE
   LPCSTR      lpLibFileName = DllName;               // If not Unicode, DLL name is in ANSI.
#else
   LPCWSTR     lpLibFileName = AnsiToWide( DllName ); // Convert DLL name to wide characters for Unicode.
#endif
   DllCnt = ( DllCnt + 1 ) & 255;                     // Update DllCnt in a circular manner within 256 entries.
   FreeLibrary( HMG_DllStore[DllCnt] );               // Free any previously loaded DLL at this index.

   // Load the specified DLL and store its handle in the current slot in HMG_DllStore.
   return HMG_DllStore[DllCnt] = LoadLibraryEx( lpLibFileName, NULL, 0 );
}

// Function to unload all DLLs stored in HMG_DllStore
static void HMG_UnloadDll( void )
{
   register int   i;

   // Iterate through HMG_DllStore and free each loaded DLL.
   for( i = 255; i >= 0; i-- )
   {
      FreeLibrary( HMG_DllStore[i] );
   }
}

// Function to get the current resources handle, either hResources or the main instance
HINSTANCE GetResources( void )
{
   return( hResources ) ? ( hResources ) : ( GetInstance() );
}

// Harbour function to return the handle to the current resources (DLL)
HB_FUNC( GETRESOURCES )
{
   hmg_ret_raw_HANDLE( GetResources() );              // Return the resources handle.
}

// Harbour function to set the current resources handle
HB_FUNC( SETRESOURCES )
{
   if( HB_ISCHAR( 1 ) ) // If parameter 1 is a string, treat it as a DLL name.
   {
      hResources = HMG_LoadDll( ( char * ) hb_parc( 1 ) );  // Load DLL and set hResources.
   }
   else if( HB_ISNUM( 1 ) )                           // If parameter 1 is numeric, treat it as a direct handle.
   {
      hResources = hmg_par_raw_HINSTANCE( 1 );
   }

   hmg_ret_raw_HANDLE( hResources );                  // Return the newly set resources handle.
}

// Harbour function to free all loaded resources and reset hResources to zero
HB_FUNC( FREERESOURCES )
{
   HMG_UnloadDll();                                   // Free all DLLs in HMG_DllStore.
   if( hResources )                                   // Reset hResources if it’s not zero.
   {
      hResources = 0;
   }
}

// Compatibility macros for older versions of Harbour or xHarbour
#if defined( __XHARBOUR__ ) || ( __HARBOUR__ - 0 < 0x030200 )

// Harbour function to save binary resource data to a file
HB_FUNC( RCDATATOFILE )
{
   HMODULE  hModule = GetResources();                 // Get the handle to the current resources.
   LPTSTR   lpType = ( hb_parclen( 3 ) > 0 ? ( LPTSTR ) hb_parc( 3 ) : MAKEINTRESOURCE( RT_RCDATA ) );
   HRSRC    hResInfo;
   HGLOBAL  hResData;
   LPVOID   lpData;
   DWORD    dwSize, dwRet;
   HANDLE   hFile;

   // Locate resource by name or ID, based on the length of the first parameter.
   if( hb_parclen( 1 ) > 0 )
   {
      hResInfo = FindResourceA( hModule, hb_parc( 1 ), lpType );
   }
   else
   {
      hResInfo = FindResource( hModule, MAKEINTRESOURCE( hb_parni( 1 ) ), lpType );
   }

   // Check if the resource was found; return -1 if not.
   if( NULL == hResInfo )
   {
      hb_retni( -1 );
      return;
   }

   hResData = LoadResource( hModule, hResInfo );      // Load the located resource.

   // Check if resource data was loaded; return -2 if not.
   if( NULL == hResData )
   {
      hb_retni( -2 );
      return;
   }

   lpData = LockResource( hResData );                 // Lock the resource to get a data pointer.

   // Check if data pointer is valid; return -3 if not.
   if( NULL == lpData )
   {
      FreeResource( hResData );
      hb_retni( -3 );
      return;
   }

   dwSize = SizeofResource( hModule, hResInfo );      // Get the size of the resource.

   // Create a new file to write the resource data.
   hFile = CreateFile( hb_parc( 2 ), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, ( DWORD ) 0, NULL );

   // Check if the file was created; return -4 if not.
   if( INVALID_HANDLE_VALUE == hFile )
   {
      FreeResource( hResData );
      hb_retni( -4 );
      return;
   }

   WriteFile( hFile, lpData, dwSize, &dwRet, NULL );  // Write resource data to the file.
   FreeResource( hResData );           // Free the resource data after use.

   // Check if write was successful; return -5 if not.
   if( dwRet != dwSize )
   {
      CloseHandle( hFile );
      hb_retni( -5 );
      return;
   }

   CloseHandle( hFile );               // Close the file.
   hb_retnl( dwRet );                  // Return the number of bytes written.
}

// Harbour function to return binary resource data in memory
HB_FUNC( RCDATATOMEM )
{
   HMODULE  hModule = GetResources();  // Get the handle to the current resources.
   LPTSTR   lpType = ( hb_parclen( 2 ) > 0 ? ( LPTSTR ) hb_parc( 2 ) : MAKEINTRESOURCE( RT_RCDATA ) );
   HRSRC    hResInfo;
   HGLOBAL  hResData;
   LPVOID   lpData;
   DWORD    dwSize;

   // Locate the resource by name or ID.
   if( hb_parclen( 1 ) > 0 )
   {
      hResInfo = FindResourceA( hModule, hb_parc( 1 ), lpType );
   }
   else
   {
      hResInfo = FindResource( hModule, MAKEINTRESOURCE( hb_parni( 1 ) ), lpType );
   }

   // Return empty string if the resource was not found.
   if( NULL == hResInfo )
   {
      hb_retc( "" );
      return;
   }

   hResData = LoadResource( hModule, hResInfo );   // Load the located resource.

   // Return empty string if resource data could not be loaded.
   if( NULL == hResData )
   {
      hb_retc( "" );
      return;
   }

   lpData = LockResource( hResData );              // Lock the resource data for access.

   // Return empty string if resource data could not be locked.
   if( NULL == lpData )
   {
      FreeResource( hResData );
      hb_retc( "" );
      return;
   }

   dwSize = SizeofResource( hModule, hResInfo );   // Get the size of the resource.
   if( dwSize )               // If the resource has data, return it as a binary string.
   {
      hb_retclen( lpData, dwSize );
   }

   FreeResource( hResData );  // Free the resource data after use.
}

#else
#if defined( __WATCOMC__ )
extern HB_EXPORT HB_SIZE   hb_fileWrite( PHB_FILE pFile, const void *buffer, HB_SIZE nSize, HB_MAXINT nTimeout );
#endif
HB_FUNC( RCDATATOFILE )
{
   HMODULE  hModule = ( HMODULE ) ( HB_ISNIL( 4 ) ? GetResources() : hmg_par_raw_HINSTANCE( 4 ) );

   /* lpType is RT_RCDATA by default */
#ifndef UNICODE
   LPCSTR   lpName = hb_parc( 1 );
   LPCSTR   lpType = hb_parclen( 3 ) > 0 ? ( LPCSTR ) hb_parc( 3 ) : MAKEINTRESOURCE( hb_parnidef( 3, 10 ) );
#else
   LPCWSTR  lpName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPCWSTR  lpType = HB_ISCHAR( 3 ) ? AnsiToWide( ( char * ) hb_parc( 3 ) ) : ( LPCWSTR ) MAKEINTRESOURCE( hb_parnidef( 3, 10 ) );
#endif
   HRSRC    hResInfo;
   HGLOBAL  hResData = NULL;
   HB_SIZE  dwResult = 0;

   if( HB_ISCHAR( 1 ) )
   {
      hResInfo = FindResource( hModule, lpName, lpType );
   }
   else
   {
      hResInfo = FindResource( hModule, MAKEINTRESOURCE( hb_parni( 1 ) ), lpType );
   }

   if( NULL != hResInfo )
   {
      hResData = LoadResource( hModule, hResInfo );

      if( NULL == hResData )
      {
         dwResult = ( HB_SIZE ) -2;         // can't load
      }
   }
   else
   {
      dwResult = ( HB_SIZE ) -1;            // can't find
   }

   if( 0 == dwResult )
   {
      LPVOID   lpData = LockResource( hResData );

      if( NULL != lpData )
      {
         DWORD    dwSize = SizeofResource( hModule, hResInfo );
         PHB_FILE pFile;

         pFile = hb_fileExtOpen( hb_parcx( 2 ), NULL, FO_CREAT | FO_WRITE | FO_EXCLUSIVE | FO_PRIVATE, NULL, NULL );

         if( NULL != pFile )
         {
            dwResult = hb_fileWrite( pFile, ( const void * ) lpData, ( HB_SIZE ) dwSize, -1 );

            if( dwResult != dwSize )
            {
               dwResult = ( HB_SIZE ) -5;   // can't write
            }

            hb_fileClose( pFile );
         }
         else
         {
            dwResult = ( HB_SIZE ) -4;      // can't open
         }
      }
      else
      {
         dwResult = ( HB_SIZE ) -3;         // can't lock
      }

      FreeResource( hResData );
   }

   hb_retnl( ( LONG ) dwResult );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpName );
   if( HB_ISCHAR( 3 ) )
   {
      hb_xfree( ( TCHAR * ) lpType );
   }
#endif
}

HB_FUNC( RCDATATOMEM )
{
   HMODULE  hModule = ( HMODULE ) ( HB_ISNIL( 3 ) ? GetResources() : hmg_par_raw_HINSTANCE( 3 ) );

#ifndef UNICODE
   LPCSTR   lpName = hb_parc( 1 );
   LPCSTR   lpType = hb_parclen( 2 ) > 0 ? ( LPCSTR ) hb_parc( 2 ) : MAKEINTRESOURCE( hb_parnidef( 2, 10 ) );
#else
   LPCWSTR  lpName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPCWSTR  lpType = HB_ISCHAR( 2 ) ? AnsiToWide( ( char * ) hb_parc( 2 ) ) : ( LPCWSTR ) MAKEINTRESOURCE( hb_parnidef( 2, 10 ) );
#endif
   HRSRC    hResInfo;
   HGLOBAL  hResData;
   LPVOID   lpData;
   DWORD    dwSize;

   if( hb_parclen( 1 ) > 0 )
   {
      hResInfo = FindResource( hModule, lpName, lpType );
   }
   else
   {
      hResInfo = FindResource( hModule, MAKEINTRESOURCE( hb_parni( 1 ) ), lpType );
   }

   if( NULL == hResInfo )
   {
      hb_retc( "" );
      return;
   }

   hResData = LoadResource( hModule, hResInfo );

   if( NULL == hResData )
   {
      hb_retc( "" );
      return;
   }

   lpData = LockResource( hResData );

   if( NULL == lpData )
   {
      FreeResource( hResData );
      hb_retc( "" );
      return;
   }

   dwSize = SizeofResource( hModule, hResInfo );

   if( dwSize )
   {
      hb_retclen( lpData, dwSize );
   }

   FreeResource( hResData );
}
#endif /* __XHARBOUR__ */
