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
   Copyright 1999-2026, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

---------------------------------------------------------------------------*/

#include "hbclass.ch"

// Compatibility macro for older Harbour/xHarbour versions to ensure hb_defaultValue behavior.
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   #xtranslate hb_defaultValue( <v>, <x> ) => ;
      iif( StrTran( ValType( <v> ), "M", "C" ) == ;
           StrTran( ValType( <x> ), "M", "C" ), <v>, <x> )
#endif

// Standard Windows Registry Access Rights
#define KEY_READ           25
#define KEY_WRITE          6
#define KEY_ALL_ACCESS     63

// Standard Windows Registry Value Types
#define REG_SZ             1  // Null-terminated string
#define REG_DWORD          4  // 32-bit number

// Windows API Constants for Registry Operations
#define ERROR_SUCCESS      0
#define KEY_WOW64_64KEY    0x0100      // Access the 64-bit registry view from a 32-bit application
#define UINT32_MAX         4294967296  // Used for unsigned integer conversion

/*
   TReg32 Class
   Provides an object-oriented interface for managing Windows Registry keys and values.
   Encapsulates low-level API calls into manageable methods for HMG Extended applications.
*/
CLASS TReg32
   EXPORTED:
   VAR cRegKey       // Stores the string path of the current registry key
   VAR nHandle       // The Windows API handle to the opened registry key
   VAR nDisposition  // Indicates if the key was created or opened (used in Create method)
   VAR nError        // Stores the last error code returned by a registry operation
   VAR lError        // Boolean flag indicating if the last operation failed

   METHOD New( nKey, cRegKey, lShowError )      // Constructor to open an existing key
   METHOD Create( nKey, cRegKey, lShowError )   // Constructor to create or open a key
   METHOD Get( cRegVar, uVar )                  // Retrieves a value from the registry
   METHOD Set( cRegVar, uVar )                  // Writes a value to the registry
   METHOD Delete( cRegVar )                     // Deletes a specific value from the key
   METHOD ShowErrorIf( nError, lShowError, cContext ) // Internal UI helper for error reporting
   METHOD Close()                               // Closes the registry handle and releases resources
ENDCLASS

/*
   METHOD New
   Purpose: Opens an existing registry key.
   Parameters:
      nKey: Root key handle (e.g., HKEY_CURRENT_USER).
      cRegKey: Subkey path string.
      lShowError: If .T., displays a message box on failure.
   Logic: Attempts to open with full access (including WOW64 redirection if applicable).
          If full access fails, it falls back to read-only access.
*/
METHOD New( nKey, cRegKey, lShowError ) CLASS TReg32
   LOCAL nHandle := 0, nReturn
   hb_default( @cRegKey, "" )
   
   // Attempt to open with full access. IsWow64() check ensures we target the correct registry view on 64-bit OS.
   nReturn := RegOpenKeyExA( nKey, cRegKey, 0, iif( IsWow64(), hb_bitOr( KEY_ALL_ACCESS, KEY_WOW64_64KEY ), KEY_ALL_ACCESS ), @nHandle )
   
   // Fallback: If full access is denied (common for HKLM without admin rights), try read-only.
   IF nReturn != ERROR_SUCCESS
      nReturn := RegOpenKeyExA( nKey, cRegKey, 0, KEY_READ, @nHandle )
   ENDIF
   
   ::lError := ( nReturn != ERROR_SUCCESS )
   IF ::lError
      ::ShowErrorIf( nReturn, lShowError, "opening" )
   ELSE
      ::cRegKey := cRegKey
      ::nHandle := nHandle
   ENDIF
RETURN Self

/*
   METHOD Create
   Purpose: Creates a new registry key or opens it if it already exists.
   Parameters:
      nKey: Root key handle.
      cRegKey: Subkey path string.
      lShowError: If .T., displays a message box on failure.
*/
METHOD Create( nKey, cRegKey, lShowError ) CLASS TReg32
   LOCAL nHandle := 0, nDisposition, nReturn
   hb_default( @cRegKey, "" )
   
   // Create the key. nDisposition will tell us if it was newly created or already existed.
   nReturn := RegCreateKey( nKey, cRegKey, @nHandle, @nDisposition )
   ::lError := ( nReturn != ERROR_SUCCESS )
   
   IF ::lError
      ::ShowErrorIf( nReturn, lShowError, "creating" )
   ELSE
      // Re-open with specific access flags to ensure handle is ready for read/write operations.
      ::nError := RegOpenKeyExA( nKey, cRegKey, 0, iif( IsWow64(), hb_bitOr( KEY_ALL_ACCESS, KEY_WOW64_64KEY ), KEY_ALL_ACCESS ), @nHandle )
      ::cRegKey := cRegKey
      ::nHandle := nHandle
      ::nDisposition := nDisposition
   ENDIF
RETURN Self

/*
   METHOD ShowErrorIf
   Purpose: Displays a standard HMG error dialog if a registry operation fails.
   Parameters:
      nError: The numeric error code from the API.
      lShowError: User preference to show/hide dialogs.
      cContext: Description of the action being performed (e.g., "opening").
*/
METHOD ShowErrorIf( nError, lShowError, cContext ) CLASS TReg32
   hb_default( @lShowError, .T. )
   IF lShowError .AND. nError != ERROR_SUCCESS
      MsgStop( "Error " + cContext + " " + ::ClassName() + " object (" + hb_ntos( nError ) + ")" )
   ENDIF
RETURN NIL

/*
   METHOD Get
   Purpose: Reads a value from the currently opened registry key.
   Parameters:
      cRegVar: The name of the registry value.
      uVar: A variable whose type determines how the registry data is converted back to Harbour.
   Returns: The value read from the registry, converted to the type of uVar.
*/
METHOD Get( cRegVar, uVar ) CLASS TReg32
   LOCAL cRegValue := "", nValueType := 0, nLen := 0, cExpectedType
   IF !::lError
      hb_default( @cRegVar, "" )
      cExpectedType := ValType( uVar )
      
      // Query the value. The API populates nValueType and cRegValue.
      ::nError := RegQueryValueExA( ::nHandle, cRegVar, 0, @nValueType, @cRegValue, @nLen )
      
      IF ::nError == ERROR_SUCCESS
         // Convert the raw binary/string data from the registry into Harbour types (Date, Logic, etc.)
         uVar := _ConvertValueFromReg( cExpectedType, cRegValue )
      ENDIF
   ENDIF
RETURN uVar

/*
   METHOD Set
   Purpose: Writes a value to the currently opened registry key.
   Parameters:
      cRegVar: The name of the registry value.
      uVar: The data to write.
   Logic: Automatically maps Harbour types (Numeric, Date, Logic, String) to Registry types (DWORD or SZ).
*/
METHOD Set( cRegVar, uVar ) CLASS TReg32
   LOCAL cType := ValType( uVar ), nType
   IF !::lError
      hb_default( @cRegVar, "" )
      
      // Map Harbour Numeric to REG_DWORD, everything else to REG_SZ (string).
      nType := iif( cType == "N", REG_DWORD, REG_SZ )
      
      // Convert non-numeric types to their string representation for registry storage.
      IF cType != "N"
         uVar := _ConvertValueToReg( cType, uVar )
      ENDIF
      
      ::nError := RegSetValueExA( ::nHandle, cRegVar, 0, nType, @uVar )
   ENDIF
RETURN NIL

/*
   METHOD Delete
   Purpose: Removes a specific value entry from the registry key.
*/
METHOD Delete( cRegVar ) CLASS TReg32
   IF !::lError
      ::nError := RegDeleteValueA( ::nHandle, hb_defaultValue( cRegVar, "" ) )
   ENDIF
RETURN NIL

/*
   METHOD Close
   Purpose: Close valid handle with error handling.
   Note: Essential to prevent resource leaks.
*/
METHOD Close() CLASS TReg32
   IF HB_ISNUMERIC( ::nHandle ) .AND. ::nHandle != 0
      ::nError := RegCloseKey( ::nHandle )
      ::nHandle := 0
   ENDIF
RETURN NIL

/*
   STATIC FUNCTION Bin2U
   Purpose: Converts a 4-byte binary string to an unsigned 32-bit integer.
   Reasoning: Harbour's Bin2L returns signed integers; this handles the overflow for registry DWORDs.
*/
STATIC FUNCTION Bin2U( cBinaryString )
   LOCAL nLong := Bin2L( cBinaryString )
RETURN iif( nLong < 0, nLong + UINT32_MAX, nLong )

/*
   STATIC FUNCTION _ConvertValueFromReg
   Purpose: Internal helper to cast raw registry strings back to Harbour types.
*/
STATIC FUNCTION _ConvertValueFromReg( cType, uVal )
   SWITCH cType
   CASE "N" ; RETURN Bin2U( uVal )           // Binary to Unsigned Numeric
   CASE "D" ; RETURN CToD( uVal )           // String to Date
   CASE "L" ; RETURN Upper( uVal ) == ".T." // String to Logical
   ENDSWITCH
RETURN uVal

/*
   STATIC FUNCTION _ConvertValueToReg
   Purpose: Internal helper to format Harbour types as strings for registry storage.
*/
STATIC FUNCTION _ConvertValueToReg( cType, uVal )
   SWITCH cType
   CASE "D" ; RETURN DToC( uVal )           // Date to String
   CASE "L" ; RETURN iif( uVal, ".T.", ".F." ) // Logical to String
   ENDSWITCH
RETURN uVal

/*
   STATIC FUNCTION _InitValueByType
   Purpose: Returns an empty/default value for a given type code.
   Used to initialize variables before calling RegQueryValue.
*/
STATIC FUNCTION _InitValueByType( cType )
RETURN iif( cType == "N", 0, iif( cType == "D", CToD( "" ), iif( cType == "L", .F., "" ) ) )

/*
   STATIC FUNCTION WithRegistry
   Purpose: A functional wrapper that handles the lifecycle of a TReg32 object.
   Parameters:
      nKey, cRegKey: Registry identifiers.
      bAction: A codeblock to execute while the key is open.
      uDefault: Value to return if the key cannot be opened.
   Side Effects: Automatically closes the registry handle after execution.
*/
STATIC FUNCTION WithRegistry( nKey, cRegKey, bAction, uDefault )
   LOCAL oReg, uResult := uDefault
   oReg := TReg32():New( nKey, cRegKey, .F. )
   IF !oReg:lError
      uResult := Eval( bAction, oReg )
   ENDIF
   oReg:Close()
RETURN uResult

/*
   FUNCTION IsRegistryKey
   Purpose: Checks if a specific registry key exists.
   Returns: .T. if exists, .F. otherwise.
*/
FUNCTION IsRegistryKey( nKey, cRegKey )
RETURN WithRegistry( nKey, cRegKey, {|oReg| !oReg:lError }, .F. )

/*
   FUNCTION CreateRegistryKey
   Purpose: Creates a registry key path.
   Returns: .T. on success.
*/
FUNCTION CreateRegistryKey( nKey, cRegKey )
   LOCAL oReg := TReg32():Create( nKey, cRegKey, .F. )
   LOCAL lSuccess := !oReg:lError
   oReg:Close()
RETURN lSuccess

/*
   FUNCTION GetRegistryValue
   Purpose: High-level function to read a value in one call.
   Parameters:
      cType: The expected Harbour type ("C", "N", "D", "L").
*/
FUNCTION GetRegistryValue( nKey, cRegKey, cRegVar, cType )
   hb_default( @cRegVar, "" )
   hb_default( @cType, "C" )
RETURN WithRegistry( nKey, cRegKey, {|oReg| oReg:Get( cRegVar, _InitValueByType( cType ) ) } )

/*
   FUNCTION SetRegistryValue
   Purpose: High-level function to write a value in one call.
*/
FUNCTION SetRegistryValue( nKey, cRegKey, cRegVar, uVal )
   hb_default( @cRegVar, "" )
RETURN WithRegistry( nKey, cRegKey, {|oReg| oReg:Set( cRegVar, uVal ), oReg:nError == ERROR_SUCCESS } )

/*
   FUNCTION DeleteRegistryVar
   Purpose: High-level function to delete a value entry.
*/
FUNCTION DeleteRegistryVar( nKey, cRegKey, cRegVar )
   hb_default( @cRegVar, "" )
RETURN WithRegistry( nKey, cRegKey, {|oReg| oReg:Delete( cRegVar ), oReg:nError == ERROR_SUCCESS } )

/*
   FUNCTION DeleteRegistryKey
   Purpose: Deletes an entire registry key (and subkeys depending on OS version).
   Directly calls the Windows API.
*/
FUNCTION DeleteRegistryKey( nKey, cRegKey )
RETURN RegDeleteKey( nKey, cRegKey ) == ERROR_SUCCESS

#pragma BEGINDUMP

#include <mgdefs.h>

// Function pointer definition for IsWow64Process to support dynamic linking.
// This ensures compatibility with older Windows versions that might not have this API.
extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );
typedef BOOL ( WINAPI *LPFN_ISWOW64PROCESS ) ( HANDLE, PBOOL );

/*
   ISWOW64 (C-level)
   Purpose: Detects if the current 32-bit process is running on a 64-bit Windows OS.
   Reasoning: Required to correctly set the KEY_WOW64_64KEY flag for registry access,
              preventing the OS from redirecting requests to the Wow6432Node.
*/
HB_FUNC_STATIC( ISWOW64 )
{
   BOOL bIsWow64 = FALSE;
   // Dynamically locate the function in kernel32.dll
   LPFN_ISWOW64PROCESS fnIsWow64Process = (LPFN_ISWOW64PROCESS) wapi_GetProcAddress( GetModuleHandle( "kernel32" ), "IsWow64Process" );
   
   if( fnIsWow64Process )
   {
      fnIsWow64Process( GetCurrentProcess(), &bIsWow64 );
   }
   hb_retl( bIsWow64 );
}

#pragma ENDDUMP