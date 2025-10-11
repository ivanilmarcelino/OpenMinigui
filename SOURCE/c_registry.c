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

#if ( defined( __BORLANDC__ ) || defined( __POCC__ ) ) && defined( _WIN64 )
#define PtrToLong( p )  ( ( LONG ) ( LONG_PTR ) ( p ) )
#endif
#define MAX_REG_BUFFER  255

// Helper function that wraps RegSetValueExA
// Sets a registry value using the provided parameters
static LONG _setRegValue( HKEY hKey, LPCSTR lpValueName, DWORD dwType, const void *pData, DWORD cbData )
{
   return RegSetValueExA( hKey, lpValueName, 0, dwType, ( const BYTE * ) pData, cbData );
}

/*
 * REGCLOSEKEY
 *
 * Closes a handle to an open registry key.
 *
 * Parameters:
 *   1: (NUMERIC) Handle to the registry key to close.
 *
 * Return Value:
 *   Returns 0 on success, -1 on failure.
 */
HB_FUNC( REGCLOSEKEY )
{
   HKEY  hKey = ( HKEY ) HB_PARNL( 1 );
   hb_retnl( ( RegCloseKey( hKey ) == ERROR_SUCCESS ) ? 0 : -1 );
}

/*
 * REGOPENKEYEXA
 *
 * Opens the specified subkey of an open registry key.
 *
 * Parameters:
 *   1: (NUMERIC) Handle to an open parent registry key.
 *   2: (STRING)  Subkey name to open.
 *   4: (NUMERIC) Access rights mask (e.g., KEY_READ, KEY_WRITE).
 *   5: (NUMERIC, BY REF) Variable to receive the opened key handle.
 *
 * Return Value:
 *   Returns 0 on success, -1 on failure.
 */
HB_FUNC( REGOPENKEYEXA )
{
   HKEY     hParentKey = ( HKEY ) HB_PARNL( 1 );
   LPCSTR   cSubKey = hb_parc( 2 );
   HKEY     hOpenedKey;
   LONG     lResult;

   if( !cSubKey )
   {
      hb_retnl( -1 );
      return;
   }

   lResult = RegOpenKeyExA( hParentKey, cSubKey, 0, ( REGSAM ) hb_parnl( 4 ), &hOpenedKey );

   if( lResult == ERROR_SUCCESS )
   {
      HB_STORNL( PtrToLong( hOpenedKey ), 5 );
      hb_retnl( 0 );
   }
   else
   {
      hb_retnl( -1 );
   }
}

/*
 * REGQUERYVALUEEXA
 *
 * Retrieves the data for a specified value name associated with an open registry key.
 *
 * Parameters:
 *   1: (NUMERIC) Registry key handle.
 *   2: (STRING)  Name of the value to query.
 *   3: (unused)
 *   4: (NUMERIC, BY REF) Variable to receive the value type (e.g., REG_SZ, REG_DWORD).
 *   5: (STRING, BY REF)  Buffer to receive the value data (as a string).
 *   6: (NUMERIC, BY REF) Variable to receive the size of the data (in bytes).
 *
 * Return Value:
 *   Returns 0 on success, -1 on failure.
 *
 * Note:
 *   This function only returns the value as a string in parameter 5.
 */
HB_FUNC( REGQUERYVALUEEXA )
{
   DWORD dwType = hmg_par_DWORD( 4 );
   DWORD dwSize = 0;
   LONG  lError;

   // First call to get required buffer size
   lError = RegQueryValueExA( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ), NULL, &dwType, NULL, &dwSize );

   if( lError == ERROR_SUCCESS )
   {
      BYTE  *lpData = ( BYTE * ) hb_xgrab( dwSize );

      // Second call to retrieve actual data
      lError = RegQueryValueExA( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ), NULL, &dwType, lpData, &dwSize );

      if( lError == ERROR_SUCCESS )
      {
         HB_STORNL( ( LONG ) dwType, 4 );
         hb_storc( ( char * ) lpData, 5 );
         HB_STORNL( ( LONG ) dwSize, 6 );
         hb_retnl( 0 );
      }
      else
      {
         hb_retnl( -1 );
      }

      hb_xfree( lpData );
   }
   else
   {
      hb_retnl( -1 );
   }
}

/*
 * REGENUMKEYEXA
 *
 * Enumerates subkeys of the specified open registry key.
 *
 * Parameters:
 *   1: (NUMERIC) Registry key handle.
 *   2: (NUMERIC) Index of the subkey to retrieve.
 *   3: (STRING, BY REF) Buffer to receive the subkey name.
 *   4: (NUMERIC, BY REF) Variable to receive subkey name length.
 *   5: (unused)
 *   6: (STRING, BY REF) Buffer to receive the class string.
 *   7: (NUMERIC, BY REF) Variable to receive class string length.
 *
 * Return Value:
 *   Returns 0 on success, -1 on failure.
 */
HB_FUNC( REGENUMKEYEXA )
{
   FILETIME ft;
   CHAR     cName[MAX_REG_BUFFER];
   CHAR     cClass[MAX_REG_BUFFER];
   DWORD    dwNameSize = MAX_REG_BUFFER;
   DWORD    dwClassSize = MAX_REG_BUFFER;
   LONG     lError;

   lError = RegEnumKeyExA( ( HKEY ) HB_PARNL( 1 ), hb_parnl( 2 ), cName, &dwNameSize, NULL, cClass, &dwClassSize, &ft );

   if( lError == ERROR_SUCCESS )
   {
      hb_storc( cName, 3 );
      HB_STORNL( ( LONG ) dwNameSize, 4 );
      hb_storc( cClass, 6 );
      HB_STORNL( ( LONG ) dwClassSize, 7 );
      hb_retnl( 0 );
   }
   else
   {
      hb_retnl( -1 );
   }
}

/*
 * REGSETVALUEEXA
 *
 * Sets the value for a named entry under an open registry key.
 *
 * Parameters:
 *   1: (NUMERIC) Registry key handle.
 *   2: (STRING)  Name of the value to set.
 *   3: (unused)
 *   4: (NUMERIC) Type of the data (e.g., REG_SZ, REG_DWORD).
 *   5: (STRING or NUMERIC) Data to write.
 *
 * Return Value:
 *   Returns 0 on success, -1 on failure.
 *
 * Notes:
 *   REG_DWORD expects a numeric input; strings are null-terminated.
 */
HB_FUNC( REGSETVALUEEXA )
{
   DWORD dwType = hmg_par_DWORD( 4 );
   LONG  lError;

   if( dwType == REG_DWORD )
   {
      DWORD nValue = hmg_par_DWORD( 5 );
      lError = _setRegValue( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ), dwType, &nValue, sizeof( DWORD ) );
   }
   else
   {
      LPCSTR   cValue = hb_parc( 5 );
      if( !cValue )
      {
         hb_retnl( -1 );
         return;
      }

      lError = _setRegValue( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ), dwType, cValue, ( DWORD ) hb_parclen( 5 ) + 1 );
   }

   hb_retnl( ( lError == ERROR_SUCCESS ) ? 0 : -1 );
}

/*
 * REGCREATEKEY
 *
 * Creates or opens a registry key under a specified parent.
 *
 * Parameters:
 *   1: (NUMERIC) Handle to an open parent key.
 *   2: (STRING)  Subkey name to create or open.
 *   3: (NUMERIC, BY REF) Variable to receive the key handle.
 *   4: (NUMERIC, BY REF) Receives disposition (e.g., REG_CREATED_NEW_KEY).
 *
 * Return Value:
 *   Returns 0 on success (and closes the key), -1 on failure.
 */
HB_FUNC( REGCREATEKEY )
{
   HKEY  hKey;
   DWORD dwDisposition;

   if( RegCreateKeyExA( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ), 0, NULL, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, &hKey, &dwDisposition ) == ERROR_SUCCESS )
   {
      HB_STORNL( PtrToLong( hKey ), 3 );
      hb_stornl( dwDisposition, 4 );
      hb_retnl( ( RegCloseKey( hKey ) == ERROR_SUCCESS ) ? 0 : -1 );
   }
   else
   {
      hb_retnl( -1 );
   }
}

/*
 * REGENUMVALUEA
 *
 * Enumerates the values under an open registry key.
 *
 * Parameters:
 *   1: (NUMERIC) Registry key handle.
 *   2: (NUMERIC) Index of the value to retrieve.
 *   3: (STRING, BY REF) Buffer to receive the value name.
 *   4: (NUMERIC, BY REF) Variable to receive the size of the value name.
 *   5: (unused)
 *   6: (NUMERIC, BY REF) Variable to receive the value type.
 *   7: (unused)
 *   8: (NUMERIC, BY REF) Variable to receive the size of the value data.
 *
 * Return Value:
 *   Returns 0 on success, -1 on failure.
 */
HB_FUNC( REGENUMVALUEA )
{
#define MAX_VAL_BUFFER  255
   CHAR  cName[MAX_VAL_BUFFER];
   DWORD dwNameSize = MAX_VAL_BUFFER;
   DWORD dwType = 0;
   DWORD dwDataSize = MAX_VAL_BUFFER;
   LONG  lError;

   lError = RegEnumValueA( ( HKEY ) HB_PARNL( 1 ), hb_parnl( 2 ), cName, &dwNameSize, NULL, &dwType, NULL, &dwDataSize );

   if( lError == ERROR_SUCCESS )
   {
      hb_storc( cName, 3 );
      HB_STORNL( ( LONG ) dwNameSize, 4 );
      HB_STORNL( ( LONG ) dwType, 6 );
      HB_STORNL( ( LONG ) dwDataSize, 8 );
   }

   hb_retnl( ( lError == ERROR_SUCCESS ) ? 0 : -1 );
}

/*
 * REGDELETEKEY
 *
 * Deletes a specified subkey from a parent key.
 *
 * Parameters:
 *   1: (NUMERIC) Handle to the parent key.
 *   2: (STRING)  Name of the subkey to delete.
 *
 * Return Value:
 *   Returns 0 on success, or an error code.
 */
HB_FUNC( REGDELETEKEY )
{
   hb_retnl( RegDeleteKeyA( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ) ) );
}

/*
 * REGDELETEVALUEA
 *
 * Deletes a named value from an open registry key.
 *
 * Parameters:
 *   1: (NUMERIC) Registry key handle.
 *   2: (STRING)  Name of the value to delete.
 *
 * Return Value:
 *   Returns 0 on success, -1 on failure.
 */
HB_FUNC( REGDELETEVALUEA )
{
   hb_retnl( ( RegDeleteValueA( ( HKEY ) HB_PARNL( 1 ), hb_parc( 2 ) ) == ERROR_SUCCESS ) ? 0 : -1 );
}

/*
 * REGCONNECTREGISTRY
 *
 * Connects to a predefined key on a remote machine.
 *
 * Parameters:
 *   1: (STRING)  Remote computer name (e.g., "\\MyPC").
 *   2: (NUMERIC) Predefined key (e.g., HKEY_LOCAL_MACHINE).
 *   3: (NUMERIC, BY REF) Receives a handle to the connected registry.
 *
 * Return Value:
 *   Returns 0 on success, -1 on failure.
 *
 * Notes:
 *   Requires network access and appropriate permissions.
 */
HB_FUNC( REGCONNECTREGISTRY )
{
   LPCSTR   cRemoteName = hb_parc( 1 );
   HKEY     hPredefKey = ( HKEY ) HB_PARNL( 2 );
   HKEY     hConnected;
   LONG     lResult;

   if( !cRemoteName )
   {
      hb_retnl( -1 );
      return;
   }

   lResult = RegConnectRegistryA( cRemoteName, hPredefKey, &hConnected );

   if( lResult == ERROR_SUCCESS )
   {
      HB_STORNL( PtrToLong( hConnected ), 3 );
   }

   hb_retnl( ( lResult == ERROR_SUCCESS ) ? 0 : -1 );
}
