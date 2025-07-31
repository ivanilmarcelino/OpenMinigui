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

#include "hbclass.ch"
#include "common.ch"

#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
#xtranslate hb_defaultValue( <v>, <x> ) => iif( StrTran( ValType( <v> ), "M", "C" ) == StrTran( ValType( <x> ), "M", "C" ), <v>, <x> )
#endif

/*
   Registry access permission constants.
*/
#define KEY_READ        25      // Read access to registry key
#define KEY_WRITE       6       // Write access to registry key
#define KEY_ALL_ACCESS  63      // Full access to registry key

/*
   Registry value type constants.
*/
#define REG_SZ          1       // String value
#define REG_DWORD       4       // 32-bit integer value

#define ERROR_SUCCESS   0       // Operation successful
#define KEY_WOW64_64KEY 0x0100  // Access 64-bit registry view (for WOW64)

/*
 * TReg32: Class for Win32 registry access.
 * Encapsulates registry key operations (open, create, get, set, delete).
 */
CREATE CLASS TReg32

   EXPORTED:
   VAR cRegKey     // Registry key path (string)
   VAR nHandle     // Registry key handle (integer)
   VAR nError      // Last error code (integer)
   VAR lError      // Error flag (logical)

   METHOD New( nKey, cRegKey, lShowError )    // Open existing key
   METHOD Create( nKey, cRegKey, lShowError ) // Create new key
   METHOD Get( cRegVar, uVar )                // Retrieve value
   METHOD Set( cRegVar, uVar )                // Set value
   METHOD Delete( cRegVar )                   // Delete value
   METHOD Close() BLOCK {|Self| iif( ::lError, , RegCloseKey( ::nHandle ) ) } // Close handle if no error

ENDCLASS

/*
 * METHOD New( nKey, cRegKey, lShowError ) CLASS TReg32
 *
 * Opens an existing registry key.
 *
 * Parameters:
 *   nKey       : Integer.  The root key to open (e.g., HKEY_LOCAL_MACHINE).
 *   cRegKey    : String. The subkey path to open under the root key.  Optional, defaults to "".
 *   lShowError : Logical.  If .T., displays an error message if the key cannot be opened. Optional, defaults to .T..
 *
 * Returns:
 *   Self: Returns the TReg32 object itself, allowing for method chaining.
 *
 * Purpose:
 *   This method allows access to an existing registry key. It first attempts to open the key with full access,
 *   including access to the 64-bit registry view if the application is running on a 64-bit version of Windows (WOW64).
 *   If full access fails, it falls back to read-only access. This ensures that the application can still read
 *   registry values even if it doesn't have write permissions. The lShowError parameter allows controlling
 *   whether an error message is displayed to the user if the key cannot be opened.
 *
 * Notes:
 *   If the key does not exist, the method will set the lError property to .T..
 *   The IsWow64() function is used to determine if the application is running on a 64-bit version of Windows.
 */
METHOD New( nKey, cRegKey, lShowError ) CLASS TReg32

   LOCAL nHandle := 0
   LOCAL nReturn

   DEFAULT cRegKey TO ""

   // Try to open key with full access, including 64-bit view if on WOW64.
   nReturn := RegOpenKeyExA( nKey, cRegKey, , ;
      iif( IsWow64(), hb_BitOr( KEY_ALL_ACCESS, KEY_WOW64_64KEY ), KEY_ALL_ACCESS ), @nHandle )

   IF nReturn != ERROR_SUCCESS
      // Fallback: try read-only access if full access fails.
      nReturn := RegOpenKeyExA( nKey, cRegKey, , KEY_READ, @nHandle )
   ENDIF

   ::lError := ( nReturn != ERROR_SUCCESS )
   IF ::lError
      // Show error if requested.
      IF hb_defaultValue( lShowError, .T. )
         MsgStop( "Error opening TReg32 object (" + hb_ntos( nReturn ) + ")" )
      ENDIF
   ELSE
      ::cRegKey := cRegKey
      ::nHandle := nHandle
   ENDIF

RETURN Self

/*
 * METHOD Create( nKey, cRegKey, lShowError ) CLASS TReg32
 *
 * Creates a new registry key.
 *
 * Parameters:
 *   nKey       : Integer. The root key under which to create the new key (e.g., HKEY_LOCAL_MACHINE).
 *   cRegKey    : String. The subkey path to create under the root key. Optional, defaults to "".
 *   lShowError : Logical. If .T., displays an error message if the key cannot be created. Optional, defaults to .T..
 *
 * Returns:
 *   Self: Returns the TReg32 object itself, allowing for method chaining.
 *
 * Purpose:
 *   This method creates a new registry key. If the key already exists, it does *not* overwrite it.
 *   After creating the key, it opens the key with full access, allowing subsequent operations to be performed on it.
 *   The lShowError parameter allows controlling whether an error message is displayed to the user if the key cannot be created.
 *
 * Notes:
 *   If the key cannot be created, the method will set the lError property to .T..
 */
METHOD Create( nKey, cRegKey, lShowError ) CLASS TReg32

   LOCAL nHandle := 0
   LOCAL nReturn

   DEFAULT cRegKey TO ""

   nReturn := RegCreateKey( nKey, cRegKey, @nHandle )

   ::lError := ( nReturn != ERROR_SUCCESS )
   IF ::lError
      // Show error if requested.
      IF hb_defaultValue( lShowError, .T. )
         MsgStop( "Error creating TReg32 object (" + hb_ntos( nReturn ) + ")" )
      ENDIF
   ELSE
      // Open the newly created key for full access.
      ::nError := RegOpenKeyExA( nKey, cRegKey, , ;
         iif( IsWow64(), hb_BitOr( KEY_ALL_ACCESS, KEY_WOW64_64KEY ), KEY_ALL_ACCESS ), @nHandle )
      ::cRegKey := cRegKey
      ::nHandle := nHandle
   ENDIF

RETURN Self

/*
 * METHOD Get( cRegVar, uVar ) CLASS TReg32
 *
 * Retrieves a value from the registry key.
 *
 * Parameters:
 *   cRegVar : String. The name of the registry value to retrieve. Optional, defaults to "".
 *   uVar    : Mixed. A variable of the expected data type to receive the retrieved value.  The data type of this variable
 *             determines the type of conversion performed on the retrieved value.
 *
 * Returns:
 *   uVar: The retrieved value, converted to the data type of the input uVar variable. If an error occurs, the original value of uVar is returned.
 *
 * Purpose:
 *   This method retrieves a value from the registry and converts it to the appropriate Harbour data type (Numeric, Date, Logical, or String).
 *   The type conversion is based on the initial data type of the uVar parameter. This allows the caller to easily retrieve
 *   registry values in the desired format.
 *
 * Notes:
 *   If the registry value does not exist or an error occurs, the uVar parameter will retain its original value.
 *   The nError property will be set to the error code returned by the RegQueryValueExA function.
 */
METHOD Get( cRegVar, uVar ) CLASS TReg32

   LOCAL cValue := ""
   LOCAL nType := 0
   LOCAL nLen := 0
   LOCAL cType

   IF ! ::lError

      DEFAULT cRegVar TO ''
      cType := ValType( uVar )

      // Query value from registry.
      ::nError := RegQueryValueExA( ::nHandle, cRegVar, 0, @nType, @cValue, @nLen )

      IF Empty( ::nError )
         uVar := cValue
         // Convert value to the expected type.
         SWITCH cType
         CASE "N"
            uVar := Bin2U( uVar )    // Convert binary to unsigned integer
            EXIT
         CASE "D"
            uVar := CToD( uVar )     // Convert string to date
            EXIT
         CASE "L"
            uVar := ( Upper( uVar ) == ".T." ) // Convert to logical
         ENDSWITCH
      ENDIF

   ENDIF

RETURN uVar

/*
 * METHOD Set( cRegVar, uVar ) CLASS TReg32
 *
 * Writes a value to the registry key.
 *
 * Parameters:
 *   cRegVar : String. The name of the registry value to set. Optional, defaults to "".
 *   uVar    : Mixed. The value to write to the registry.  The data type of this variable determines the registry value type.
 *
 * Returns:
 *   NIL: This method does not return a value.
 *
 * Purpose:
 *   This method writes a value to the registry. It automatically handles type conversion for Date and Logical values,
 *   converting them to their string representations before writing them to the registry. Numeric values are stored as REG_DWORD,
 *   while all other types are stored as REG_SZ (string).
 *
 * Notes:
 *   The nError property will be set to the error code returned by the RegSetValueExA function.
 */
METHOD Set( cRegVar, uVar ) CLASS TReg32

   LOCAL cType
   LOCAL nType

   IF ! ::lError

      DEFAULT cRegVar TO ''
      cType := ValType( uVar )

      IF cType == 'N'
         nType := REG_DWORD
      ELSE
         nType := REG_SZ
         // Convert date and logical types to string representation
         SWITCH cType
         CASE "D"
            uVar := DToC( uVar )
            EXIT
         CASE "L"
            uVar := iif( uVar, ".T.", ".F." )
         ENDSWITCH
      ENDIF

      // Write value to registry.
      ::nError := RegSetValueExA( ::nHandle, cRegVar, 0, nType, @uVar )

   ENDIF

RETURN NIL

/*
 * METHOD Delete( cRegVar ) CLASS TReg32
 *
 * Deletes a value from the registry key.
 *
 * Parameters:
 *   cRegVar : String. The name of the registry value to delete.
 *
 * Returns:
 *   NIL: This method does not return a value.
 *
 * Purpose:
 *   This method deletes a specific value from the currently open registry key.
 *
 * Notes:
 *   The nError property will be set to the error code returned by the RegDeleteValueA function.
 */
METHOD Delete( cRegVar ) CLASS TReg32

   IF ! ::lError
      ::nError := RegDeleteValueA( ::nHandle, cRegVar )
   ENDIF

RETURN NIL

/*
 * FUNCTION Bin2U( c )
 *
 * Converts a binary string to an unsigned integer.
 *
 * Parameters:
 *   c : String. The binary string to convert.
 *
 * Returns:
 *   Numeric. The unsigned integer representation of the binary string.
 *
 * Purpose:
 *   This function converts a binary string (typically read from the registry) to an unsigned integer.
 *   It handles negative values by wrapping them to the 32-bit unsigned range. This is necessary because
 *   the Bin2L() function returns a signed integer, and registry values are often stored as unsigned integers.
 *
 * Notes:
 *   This function assumes that the binary string represents a 32-bit integer.
 */
STATIC FUNCTION Bin2U( c )

   LOCAL l := Bin2L( c )

RETURN iif( l < 0, l + 4294967296, l )

/*
 * Registry Access Helper Functions
 */

/*
 * FUNCTION IsRegistryKey( nKey, cRegKey )
 *
 * Checks if a registry key exists.
 *
 * Parameters:
 *   nKey    : Integer. The root key to check (e.g., HKEY_LOCAL_MACHINE).
 *   cRegKey : String. The subkey path to check.
 *
 * Returns:
 *   Logical. .T. if the key exists, .F. otherwise.
 *
 * Purpose:
 *   This function provides a convenient way to determine if a registry key exists before attempting to open or create it.
 *   This can prevent errors and improve the robustness of the application.
 *
 * Notes:
 *   The function creates a temporary TReg32 object to check for the key's existence.
 */
FUNCTION IsRegistryKey( nKey, cRegKey )

   LOCAL oReg
   LOCAL lExist

   oReg   := TReg32():New( nKey, cRegKey, .F. )
   lExist := ( oReg:lError == .F. )

   oReg:Close()

RETURN lExist

/*
 * FUNCTION CreateRegistryKey( nKey, cRegKey )
 *
 * Creates a registry key if it does not exist.
 *
 * Parameters:
 *   nKey    : Integer. The root key under which to create the new key (e.g., HKEY_LOCAL_MACHINE).
 *   cRegKey : String. The subkey path to create.
 *
 * Returns:
 *   Logical. .T. on success, .F. on failure.
 *
 * Purpose:
 *   This function simplifies the creation of registry keys. It creates the key only if it doesn't already exist.
 *   This is useful for ensuring that the necessary registry keys are in place before writing values to them.
 *
 * Notes:
 *   The function creates a temporary TReg32 object to create the key.
 */
FUNCTION CreateRegistryKey( nKey, cRegKey )

   LOCAL oReg
   LOCAL lSuccess

   oReg     := TReg32():Create( nKey, cRegKey, .F. )
   lSuccess := ( oReg:lError == .F. )

   oReg:Close()

RETURN lSuccess

/*
 * FUNCTION GetRegistryValue( nKey, cRegKey, cRegVar, cType )
 *
 * Retrieves a value from the registry.
 *
 * Parameters:
 *   nKey    : Integer. The root key to read from (e.g., HKEY_LOCAL_MACHINE).
 *   cRegKey : String. The subkey path to read from.
 *   cRegVar : String. The name of the registry value to retrieve. Optional, defaults to "".
 *   cType   : Character. The expected data type of the registry value ('N' for Numeric, 'D' for Date, 'L' for Logical, 'C' for Character). Optional, defaults to 'C'.
 *
 * Returns:
 *   Mixed. The value retrieved from the registry, converted to the specified data type. Returns NIL on error.
 *
 * Purpose:
 *   This function provides a high-level interface for retrieving values from the registry. It handles the creation of the TReg32 object,
 *   retrieval of the value, type conversion, and closing of the registry key. The cType parameter allows the caller to specify the
 *   expected data type of the registry value, ensuring that it is converted to the correct Harbour type.
 *
 * Notes:
 *   If the registry value does not exist or an error occurs, the function returns NIL.
 */
FUNCTION GetRegistryValue( nKey, cRegKey, cRegVar, cType )

   LOCAL oReg
   LOCAL uVal

   DEFAULT cRegVar TO '', cType TO 'C'

   oReg := TReg32():New( nKey, cRegKey, .F. )

   IF ! oReg:lError
      // Initialize uVal to the expected type.
      DEFAULT uVal TO iif( cType == 'N', 0, iif( cType == 'D', CToD(''), iif( cType == 'L', .F., '' ) ) )
      uVal := oReg:Get( cRegVar, uVal )
      IF oReg:nError != ERROR_SUCCESS
         uVal := NIL
      ENDIF
   ENDIF

   oReg:Close()

RETURN uVal

/*
 * FUNCTION SetRegistryValue( nKey, cRegKey, cRegVar, uVal )
 *
 * Sets a value in the registry.
 *
 * Parameters:
 *   nKey    : Integer. The root key to write to (e.g., HKEY_LOCAL_MACHINE).
 *   cRegKey : String. The subkey path to write to.
 *   cRegVar : String. The name of the registry value to set. Optional, defaults to "".
 *   uVal    : Mixed. The value to write to the registry.
 *
 * Returns:
 *   Logical. .T. on success, .F. on failure.
 *
 * Purpose:
 *   This function provides a high-level interface for setting values in the registry. It handles the creation of the TReg32 object,
 *   setting of the value, and closing of the registry key.
 *
 * Notes:
 *   If an error occurs, the function returns .F..
 */
FUNCTION SetRegistryValue( nKey, cRegKey, cRegVar, uVal )

   LOCAL oReg
   LOCAL lSuccess := .F.

   DEFAULT cRegVar TO ''

   oReg := TReg32():New( nKey, cRegKey, .F. )

   IF ! oReg:lError
      oReg:Set( cRegVar, uVal )
      lSuccess := ( oReg:nError == ERROR_SUCCESS )
   ENDIF

   oReg:Close()

RETURN lSuccess

/*
 * FUNCTION DeleteRegistryVar( nKey, cRegKey, cRegVar )
 *
 * Deletes a value from a registry key.
 *
 * Parameters:
 *   nKey    : Integer. The root key to delete from (e.g., HKEY_LOCAL_MACHINE).
 *   cRegKey : String. The subkey path to delete from.
 *   cRegVar : String. The name of the registry value to delete. Optional, defaults to "".
 *
 * Returns:
 *   Logical. .T. on success, .F. on failure.
 *
 * Purpose:
 *   This function provides a high-level interface for deleting values from the registry. It handles the creation of the TReg32 object,
 *   deletion of the value, and closing of the registry key.
 *
 * Notes:
 *   If an error occurs, the function returns .F..
 */
FUNCTION DeleteRegistryVar( nKey, cRegKey, cRegVar )

   LOCAL oReg
   LOCAL lSuccess := .F.

   DEFAULT cRegVar TO ''

   oReg := TReg32():New( nKey, cRegKey, .F. )

   IF ! oReg:lError
      oReg:Delete( cRegVar )
      lSuccess := ( oReg:nError == ERROR_SUCCESS )
   ENDIF

   oReg:Close()

RETURN lSuccess

/*
 * FUNCTION DeleteRegistryKey( nKey, cRegKey )
 *
 * Deletes an entire registry key.
 *
 * Parameters:
 *   nKey    : Integer. The root key to delete (e.g., HKEY_LOCAL_MACHINE).
 *   cRegKey : String. The subkey path to delete.
 *
 * Returns:
 *   Logical. .T. on success, .F. on failure.
 *
 * Purpose:
 *   This function deletes an entire registry key and all its subkeys and values.
 *   Use with caution, as this operation is irreversible.
 *
 * Notes:
 *   This function uses the native RegDeleteKey function directly.
 *   The key must be empty (no subkeys) for the deletion to succeed on some older Windows versions.
 */
FUNCTION DeleteRegistryKey( nKey, cRegKey )

   LOCAL lSuccess

   lSuccess := ( RegDeleteKey( nKey, cRegKey ) == ERROR_SUCCESS )

RETURN lSuccess

/*
 * C-level: Native code for detecting WOW64 (Windows-on-Windows 64-bit) environment.
 * Used to determine if registry access should target 64-bit view.
 */
#pragma BEGINDUMP

#include <mgdefs.h>

extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );

// Type definition for IsWow64Process API.
typedef BOOL ( WINAPI *LPFN_ISWOW64PROCESS ) ( HANDLE, PBOOL );

/*
 * HB_FUNC_STATIC( ISWOW64 )
 *
 * Determines if the current process is running under WOW64 (32-bit process on 64-bit Windows).
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Logical. .T. if running under WOW64, .F. otherwise.
 *
 * Purpose:
 *   This function is used to determine whether the application is running in a 32-bit environment on a 64-bit operating system.
 *   This information is crucial for accessing the correct registry view (32-bit or 64-bit) when the application needs to read or write
 *   registry values that are specific to the architecture.
 *
 * Notes:
 *   This function uses the IsWow64Process API, which is only available on 64-bit versions of Windows.
 *   It dynamically loads the IsWow64Process function from kernel32.dll using GetProcAddress.
 */
HB_FUNC_STATIC( ISWOW64 )
{
   BOOL bIsWow64 = FALSE;

   LPFN_ISWOW64PROCESS fnIsWow64Process;

   fnIsWow64Process = ( LPFN_ISWOW64PROCESS ) wapi_GetProcAddress( GetModuleHandle( "kernel32" ), "IsWow64Process" );
   if( NULL != fnIsWow64Process )
   {
      fnIsWow64Process( GetCurrentProcess(), &bIsWow64 );
   }

   hb_retl( bIsWow64 );
}

#pragma ENDDUMP
