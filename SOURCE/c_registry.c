/*----------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This program is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this software; see the file COPYING. If not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
   visit the web site http://www.gnu.org/).

   As a special exception, you have permission for additional uses of the text
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   Harbour-Minigui library code into it.

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

 ---------------------------------------------------------------------------*/
#include <mgdefs.h>
#include <commctrl.h>

// Convert pointer to long on Win64 platforms for compatibility.
#if ( defined( __BORLANDC__ ) || defined( __POCC__ ) ) && defined( _WIN64 )
#define PtrToLong( p )  ( ( LONG ) ( LONG_PTR ) ( p ) )
#endif

// Function to close a registry key handle.
HB_FUNC( REGCLOSEKEY )
{
   HKEY  hwHandle = ( HKEY ) HB_PARNL( 1 );  // Get the registry key handle from the first parameter.

   // Close the registry key and return success or error code.
   hb_retnl( ( RegCloseKey( hwHandle ) == ERROR_SUCCESS ) ? ERROR_SUCCESS : -1 );
}

// Function to open a registry key using `RegOpenKeyExA`.
HB_FUNC( REGOPENKEYEXA )
{
   HKEY     hwKey = ( HKEY ) HB_PARNL( 1 );  // Parent registry key handle.
   LPCTSTR  lpValue = hb_parc( 2 );          // Subkey name to open.
   long     lError;     // Variable to store the error code.
   HKEY     phwHandle;  // Handle for the opened subkey.

   // Attempt to open the registry key.
   lError = RegOpenKeyExA( ( HKEY ) hwKey, lpValue, 0, ( REGSAM ) hb_parnl( 4 ), &phwHandle );

   // Check if the operation was successful.
   if( lError != ERROR_SUCCESS )
   {
      hb_retnl( -1 );   // Return -1 on failure.
   }
   else
   {
      HB_STORNL( PtrToLong( phwHandle ), 5 );   // Store the new key handle in the fifth parameter.
      hb_retnl( 0 );             // Return success.
   }
}

// Function to query a registry value, returning its type, data, and length.
HB_FUNC( REGQUERYVALUEEXA )
{
   long  lError;                 // Variable to store error code.
   DWORD lpType = hb_parnl( 4 ); // Variable type identifier.
   DWORD lpcbData = 0;           // Size of the data buffer.

   // Query registry value size and type.
   lError = RegQueryValueExA( ( HKEY ) HB_PARNL( 1 ), ( LPTSTR ) hb_parc( 2 ), NULL, &lpType, NULL, &lpcbData );

   if( lError == ERROR_SUCCESS )
   {
      BYTE  *lpData;

      // Allocate memory to hold the registry data and query the actual value.
      lpData = ( BYTE * ) hb_xgrab( lpcbData + 1 );
      lError = RegQueryValueExA( ( HKEY ) HB_PARNL( 1 ), ( LPTSTR ) hb_parc( 2 ), NULL, &lpType, ( BYTE * ) lpData, &lpcbData );

      if( lError != ERROR_SUCCESS )
      {
         hb_retnl( -1 );         // Return -1 on failure.
      }
      else
      {
         HB_STORNL( ( long ) lpType, 4 );    // Store the data type.
         hb_storc( ( char * ) lpData, 5 );   // Store the data.
         HB_STORNL( ( long ) lpcbData, 6 );  // Store the data size.
         hb_retnl( 0 );                      // Return success.
      }

      hb_xfree( lpData );                    // Free allocated memory for data buffer.
   }
   else
   {
      hb_retnl( -1 );                        // Return -1 if the value query fails.
   }
}

// Function to enumerate subkeys in a registry key.
HB_FUNC( REGENUMKEYEXA )
{
   FILETIME ft;                              // Filetime variable for last write time.
   long     bErr;
   TCHAR    Buffer[255];                     // Buffer for subkey name.
   DWORD    dwBuffSize = 255;                // Size of subkey buffer.
   TCHAR    Class[255];                      // Buffer for class name.
   DWORD    dwClass = 255;                   // Size of class buffer.

   // Enumerate subkeys in the registry key.
   bErr = RegEnumKeyEx( ( HKEY ) HB_PARNL( 1 ), hb_parnl( 2 ), Buffer, &dwBuffSize, NULL, Class, &dwClass, &ft );

   if( bErr != ERROR_SUCCESS )
   {
      hb_retnl( -1 );                        // Return -1 on failure.
   }
   else
   {
      hb_storc( Buffer, 3 );                 // Store the subkey name.
      HB_STORNL( ( long ) dwBuffSize, 4 );   // Store the size of the subkey name.
      hb_storc( Class, 6 );                  // Store the class name.
      HB_STORNL( ( long ) dwClass, 7 );      // Store the class name size.
      hb_retnl( 0 );                // Return success.
   }
}

// Function to set a value in the registry.
HB_FUNC( REGSETVALUEEXA )
{
   DWORD nType = hb_parnl( 4 );     // Data type of the value.
   if( nType != REG_DWORD )
   {
      // Set non-DWORD data (e.g., strings) in the registry.
      if( RegSetValueExA( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ), 0, nType, ( BYTE * ) hb_parc( 5 ), ( DWORD ) hb_parclen( 5 ) + 1 ) == ERROR_SUCCESS )
      {
         hb_retnl( 0 );             // Return success.
      }
      else
      {
         hb_retnl( -1 );            // Return -1 on failure.
      }
   }
   else
   {
      DWORD nSpace = hb_parnl( 5 ); // For DWORD type, value is passed directly.
      if( RegSetValueExA( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ), 0, nType, ( BYTE * ) &nSpace, sizeof( REG_DWORD ) ) == ERROR_SUCCESS )
      {
         hb_retnl( 0 );             // Return success.
      }
      else
      {
         hb_retnl( -1 );            // Return -1 on failure.
      }
   }
}

// Function to create a registry key.
HB_FUNC( REGCREATEKEY )
{
   HKEY  hKey; // Handle for the new registry key.

   // Attempt to create the registry key.
   if( RegCreateKey( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ), &hKey ) == ERROR_SUCCESS )
   {
      HB_STORNL( PtrToLong( hKey ), 3 );  // Store the new key handle.
      hb_retnl( 0 );          // Return success.
   }
   else
   {
      hb_retnl( -1 );         // Return -1 on failure.
   }
}

// Function to enumerate a registry key's values.
HB_FUNC( REGENUMVALUEA )
{
   DWORD lpType = 1;          // Variable to store the type of the registry value.
   TCHAR Buffer[255];         // Buffer for the value name.
   DWORD dwBuffSize = 255;    // Size of the value name buffer.
   DWORD dwClass = 255;       // Size of the class buffer.
   long  lError;

   // Enumerate registry values.
   lError = RegEnumValueA( ( HKEY ) HB_PARNL( 1 ), hb_parnl( 2 ), Buffer, &dwBuffSize, NULL, &lpType, NULL, &dwClass );

   if( lError != ERROR_SUCCESS )
   {
      hb_retnl( -1 );         // Return -1 on failure.
   }
   else
   {
      hb_storc( Buffer, 3 );  // Store the value name.
      HB_STORNL( ( long ) dwBuffSize, 4 );   // Store the size of the value name.
      HB_STORNL( ( long ) lpType, 6 );       // Store the value type.
      HB_STORNL( ( long ) dwClass, 8 );      // Store the data size.
      hb_retnl( lError );                    // Return success or error code.
   }
}

// Function to delete a registry key.
HB_FUNC( REGDELETEKEY )
{
   // Delete the specified registry key and return the result.
   hb_retnl( RegDeleteKey( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ) ) );
}

// Function to delete a registry value.
HB_FUNC( REGDELETEVALUEA )
{
   // Delete the specified value within a registry key.
   if( RegDeleteValueA( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ) ) == ERROR_SUCCESS )
   {
      hb_retnl( 0 );                // Return success.
   }
   else
   {
      hb_retnl( -1 );               // Return -1 on failure.
   }
}

// Function to connect to a registry on a remote computer.
HB_FUNC( REGCONNECTREGISTRY )
{
   LPCTSTR  lpValue = hb_parc( 1 ); // Remote computer name.
   HKEY     hwKey = ( ( HKEY ) HB_PARNL( 2 ) ); // Predefined key to connect.
   long     lError;
   HKEY     phwHandle;

   // Attempt to connect to the registry on the specified computer.
   lError = RegConnectRegistry( lpValue, ( HKEY ) hwKey, &phwHandle );

   if( lError != ERROR_SUCCESS )
   {
      hb_retnl( -1 );   // Return -1 on failure.
   }
   else
   {
      HB_STORNL( PtrToLong( phwHandle ), 3 );   // Store the new registry handle.
      hb_retnl( lError );  // Return success or error code.
   }
}
