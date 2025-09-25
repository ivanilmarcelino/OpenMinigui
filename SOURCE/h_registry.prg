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

// Registry access permission constants.
#define KEY_READ        25
#define KEY_WRITE       6
#define KEY_ALL_ACCESS  63

// Registry value type constants.
#define REG_SZ          1
#define REG_DWORD       4

#define ERROR_SUCCESS   0
#define KEY_WOW64_64KEY 0x0100

/*
 * CLASS TReg32
 *
 * Provides an object-oriented interface for interacting with the 32-bit Windows Registry.
 *
 * Purpose:
 *   This class encapsulates the Windows Registry API, simplifying common registry operations
 *   such as opening, creating, reading, writing, and deleting registry keys and values.
 *   It provides a more structured and manageable way to access the registry compared to
 *   directly using the Windows API functions.  It also handles potential errors and provides
 *   a mechanism for displaying error messages.
 *
 * Notes:
 *   This class is designed for 32-bit registry access.  For 64-bit registry access on 64-bit
 *   systems, the KEY_WOW64_64KEY flag is used when opening keys.
 */
CREATE CLASS TReg32

   EXPORTED:
   VAR cRegKey       // Stores the registry key path (string).
   VAR nHandle       // Stores the handle to the opened registry key (numeric).
   VAR nDisposition  // Stores the disposition of the created registry key (numeric).
   VAR nError        // Stores the last error code returned by a registry API function (numeric).
   VAR lError        // Indicates whether an error occurred during registry operations (logical).

   METHOD New( nKey, cRegKey, lShowError )
   METHOD Create( nKey, cRegKey, lShowError )
   METHOD Get( cRegVar, uVar )
   METHOD Set( cRegVar, uVar )
   METHOD Delete( cRegVar )
   METHOD ShowErrorIf( nError, lShowError, cContext )
   METHOD Close() BLOCK {| Self | iif( ::lError, , RegCloseKey( ::nHandle ) ) }

ENDCLASS

/*
 * METHOD New( nKey, cRegKey, lShowError )
 *
 * Opens an existing registry key.
 *
 * Parameters:
 *   nKey       (numeric):  The handle of a predefined registry key (e.g., HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER).
 *   cRegKey    (string):   The path to the registry key to open.  Optional, defaults to "".
 *   lShowError (logical):  A flag indicating whether to display an error message if the key cannot be opened. Optional, defaults to .T.
 *
 * Return Value:
 *   Self (object): Returns the TReg32 object itself, allowing for method chaining.
 *
 * Purpose:
 *   This method attempts to open the specified registry key. It first tries to open the key with
 *   full access (KEY_ALL_ACCESS). If that fails, it attempts to open the key with read-only access (KEY_READ).
 *   If both attempts fail, the lError flag is set to .T., and an error message is displayed (if lShowError is .T.).
 *   The method stores the registry key path and handle in the object's properties for subsequent operations.
 *
 * Notes:
 *   If the key does not exist, the method will set the lError flag to .T.
 *   The IsWow64() function is used to determine if the process is running in a 32-bit environment on a 64-bit OS,
 *   and if so, the KEY_WOW64_64KEY flag is added to the access mask to access the 64-bit registry view.
 */
METHOD New( nKey, cRegKey, lShowError ) CLASS TReg32

   LOCAL nHandle := 0
   LOCAL nReturn

   cRegKey := hb_defaultValue( cRegKey, "" )

   nReturn := RegOpenKeyExA( nKey, cRegKey, , ;
      iif( IsWow64(), hb_bitOr( KEY_ALL_ACCESS, KEY_WOW64_64KEY ), KEY_ALL_ACCESS ), @nHandle )

   IF nReturn != ERROR_SUCCESS
      nReturn := RegOpenKeyExA( nKey, cRegKey, , KEY_READ, @nHandle )
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
 * METHOD Create( nKey, cRegKey, lShowError )
 *
 * Creates a new registry key.
 *
 * Parameters:
 *   nKey       (numeric):  The handle of a predefined registry key (e.g., HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER).
 *   cRegKey    (string):   The path to the registry key to create. Optional, defaults to "".
 *   lShowError (logical):  A flag indicating whether to display an error message if the key cannot be created. Optional, defaults to .T.
 *
 * Return Value:
 *   Self (object): Returns the TReg32 object itself, allowing for method chaining.
 *
 * Purpose:
 *   This method creates a new registry key under the specified parent key. If the key already exists,
 *   it will be opened. The method stores the registry key path, handle, and disposition in the object's
 *   properties for subsequent operations. If an error occurs during key creation, the lError flag is set to .T.,
 *   and an error message is displayed (if lShowError is .T.).
 *
 * Notes:
 *   The RegCreateKey function creates the key if it does not exist.
 *   The IsWow64() function is used to determine if the process is running in a 32-bit environment on a 64-bit OS,
 *   and if so, the KEY_WOW64_64KEY flag is added to the access mask to access the 64-bit registry view.
 */
METHOD Create( nKey, cRegKey, lShowError ) CLASS TReg32

   LOCAL nHandle := 0
   LOCAL nDisposition
   LOCAL nReturn

   cRegKey := hb_defaultValue( cRegKey, "" )

   nReturn := RegCreateKey( nKey, cRegKey, @nHandle, @nDisposition )
   ::lError := ( nReturn != ERROR_SUCCESS )

   IF ::lError
      ::ShowErrorIf( nReturn, lShowError, "creating" )
   ELSE
      ::nError := RegOpenKeyExA( nKey, cRegKey, , ;
         iif( IsWow64(), hb_bitOr( KEY_ALL_ACCESS, KEY_WOW64_64KEY ), KEY_ALL_ACCESS ), @nHandle )
      ::cRegKey := cRegKey
      ::nHandle := nHandle
      ::nDisposition := nDisposition
   ENDIF

RETURN Self

/*
 * METHOD ShowErrorIf( nError, lShowError, cContext )
 *
 * Displays an error message if an error occurred during a registry operation.
 *
 * Parameters:
 *   nError     (numeric):  The error code returned by the registry API function.
 *   lShowError (logical):  A flag indicating whether to display the error message. Optional, defaults to .T.
 *   cContext   (string):   A string describing the context in which the error occurred (e.g., "opening", "creating").
 *
 * Return Value:
 *   NIL
 *
 * Purpose:
 *   This method provides a centralized way to display error messages related to registry operations.
 *   It checks if the lShow flag is .T. and if the error code is not ERROR_SUCCESS. If both conditions are met,
 *   it displays an error message using MsgStop, indicating the context of the error and the error code.
 *
 * Notes:
 *   The MsgStop function displays a modal message box with an error icon.
 */
METHOD ShowErrorIf( nError, lShowError, cContext ) CLASS TReg32

   IF hb_defaultValue( lShowError, .T. ) .AND. nError != ERROR_SUCCESS
      MsgStop( "Error " + cContext + " " + ::ClassName + " object (" + hb_ntos( nError ) + ")" )
   ENDIF

RETURN NIL

/*
 * METHOD Get( cRegVar, uVar )
 *
 * Retrieves the value of a registry variable.
 *
 * Parameters:
 *   cRegVar (string):  The name of the registry variable to retrieve. Optional, defaults to "".
 *   uVar    (variant): A variable to store the retrieved value. The data type of this variable
 *                      determines the expected data type of the registry value.
 *
 * Return Value:
 *   uVar (variant): The retrieved value from the registry, converted to the expected data type.
 *                   If an error occurs or the variable does not exist, the original value of uVar is returned.
 *
 * Purpose:
 *   This method retrieves the value of the specified registry variable from the currently opened registry key.
 *   It determines the expected data type based on the data type of the uVar parameter and converts the retrieved
 *   value to that type. If the registry operation is successful, the retrieved value is assigned to uVar.
 *   If an error occurs, the nError property is set, and the original value of uVar is returned.
 *
 * Notes:
 *   The _ConvertValueFromReg function is used to convert the retrieved value from the registry to the expected data type.
 *   The ValType function is used to determine the expected data type of the registry value.
 */
METHOD Get( cRegVar, uVar ) CLASS TReg32

   LOCAL cRegValue := ""
   LOCAL nValueType := 0
   LOCAL nLen := 0
   LOCAL cExpectedType

   IF ! ::lError
      cRegVar := hb_defaultValue( cRegVar, '' )
      cExpectedType := ValType( uVar )

      ::nError := RegQueryValueExA( ::nHandle, cRegVar, 0, @nValueType, @cRegValue, @nLen )

      IF Empty( ::nError )
         uVar := _ConvertValueFromReg( cExpectedType, cRegValue )
      ENDIF
   ENDIF

RETURN uVar

/*
 * METHOD Set( cRegVar, uVar )
 *
 * Sets the value of a registry variable.
 *
 * Parameters:
 *   cRegVar (string):  The name of the registry variable to set. Optional, defaults to "".
 *   uVar    (variant): The value to set for the registry variable. The data type of this variable
 *                      determines the data type of the registry value.
 *
 * Return Value:
 *   NIL
 *
 * Purpose:
 *   This method sets the value of the specified registry variable in the currently opened registry key.
 *   It determines the data type of the value based on the data type of the uVar parameter and converts the value
 *   to a string representation if necessary. If the registry operation is successful, the nError property is set to ERROR_SUCCESS.
 *
 * Notes:
 *   The _ConvertValueToReg function is used to convert the value to a string representation if necessary.
 *   The ValType function is used to determine the data type of the value.
 *   Only numeric and string values are supported.
 */
METHOD Set( cRegVar, uVar ) CLASS TReg32

   LOCAL cType := ValType( uVar )
   LOCAL nType

   IF ! ::lError

      cRegVar := hb_defaultValue( cRegVar, '' )

      IF cType == 'N'
         nType := REG_DWORD
      ELSE
         nType := REG_SZ
         uVar := _ConvertValueToReg( cType, uVar )
      ENDIF

      ::nError := RegSetValueExA( ::nHandle, cRegVar, 0, nType, @uVar )
   ENDIF

RETURN NIL

/*
 * METHOD Delete( cRegVar )
 *
 * Deletes a registry variable.
 *
 * Parameters:
 *   cRegVar (string): The name of the registry variable to delete. Optional, defaults to "".
 *
 * Return Value:
 *   NIL
 *
 * Purpose:
 *   This method deletes the specified registry variable from the currently opened registry key.
 *   If the registry operation is successful, the nError property is set to ERROR_SUCCESS.
 */
METHOD Delete( cRegVar ) CLASS TReg32

   IF ! ::lError
      ::nError := RegDeleteValueA( ::nHandle, hb_defaultValue( cRegVar, '' ) )
   ENDIF

RETURN NIL


/*
 * STATIC FUNCTION Bin2U( c )
 *
 * Converts a binary string to an unsigned integer.
 *
 * Parameters:
 *   cBinaryString (string): The binary string to convert.
 *
 * Return Value:
 *   (numeric): The unsigned integer representation of the binary string.
 *
 * Purpose:
 *   This function converts a binary string (typically read from the registry) into an unsigned integer.
 *   It handles the case where the binary string represents a negative number by adding 4294967296 to it.
 *
 * Notes:
 *   This function is used to convert REG_DWORD values from the registry to Harbour numeric values.
 */
#define UINT32_MAX 4294967296

STATIC FUNCTION Bin2U( cBinaryString )

   LOCAL nLong := Bin2L( cBinaryString )

RETURN iif( nLong < 0, nLong + UINT32_MAX, nLong )

/*
 * STATIC FUNCTION _ConvertValueFromReg( cType, uVal )
 *
 * Converts a registry value to a Harbour data type.
 *
 * Parameters:
 *   cType (string):  The Harbour data type to convert to (e.g., "N", "D", "L").
 *   uVal  (variant): The registry value to convert.
 *
 * Return Value:
 *   (variant): The converted value.
 *
 * Purpose:
 *   This function converts a registry value (which is always stored as a string) to the appropriate
 *   Harbour data type based on the cType parameter.
 *
 * Notes:
 *   This function supports converting to numeric, date, and logical data types.
 */
STATIC FUNCTION _ConvertValueFromReg( cType, uVal )
   SWITCH cType
   CASE "N" ; RETURN Bin2U( uVal )
   CASE "D" ; RETURN CToD( uVal )
   CASE "L" ; RETURN ( Upper( uVal ) == ".T." )
   END

RETURN uVal

/*
 * STATIC FUNCTION _ConvertValueToReg( cType, uVal )
 *
 * Converts a Harbour value to a string representation for storing in the registry.
 *
 * Parameters:
 *   cType (string):  The Harbour data type of the value (e.g., "D", "L").
 *   uVal  (variant): The value to convert.
 *
 * Return Value:
 *   (string): The string representation of the value.
 *
 * Purpose:
 *   This function converts a Harbour value to a string representation that can be stored in the registry.
 *   It handles date and logical data types.
 *
 * Notes:
 *   This function supports converting from date and logical data types.
 */
STATIC FUNCTION _ConvertValueToReg( cType, uVal )
   SWITCH cType
   CASE "D" ; RETURN DToC( uVal )
   CASE "L" ; RETURN iif( uVal, ".T.", ".F." )
   END

RETURN uVal

/*
 * STATIC FUNCTION _InitValueByType( cType )
 *
 * Returns an initial value for a given data type.
 *
 * Parameters:
 *   cType (string): The Harbour data type (e.g., "N", "D", "L").
 *
 * Return Value:
 *   (variant): An initial value for the given data type (0 for numeric, blank date for date, .F. for logical, "" for others).
 *
 * Purpose:
 *   This function returns an appropriate initial value for a given data type. This is used when retrieving registry values
 *   to provide a default value if the registry variable does not exist.
 */
STATIC FUNCTION _InitValueByType( cType )
RETURN iif( cType == 'N', 0, ;
      iif( cType == 'D', CToD( '' ), ;
      iif( cType == 'L', .F., '' ) ) )

/*
 * STATIC FUNCTION WithRegistry( nKey, cRegKey, bAction )
 *
 * Executes a code block with a TReg32 object.
 *
 * Parameters:
 *   nKey    (numeric): The handle of a predefined registry key (e.g., HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER).
 *   cRegKey (string):  The path to the registry key to operate on.
 *   bAction (codeblock): The code block to execute with the TReg32 object as a parameter.
 *
 * Return Value:
 *   (variant): The result of the code block execution.
 *
 * Purpose:
 *   This function simplifies registry operations by creating a TReg32 object, executing a code block with the object,
 *   and then closing the object. It handles potential errors during object creation and ensures that the registry key is always closed.
 *
 * Notes:
 *   The TReg32 object is created with lShowError set to .F. to suppress error messages during object creation.
 */
STATIC FUNCTION WithRegistry( nKey, cRegKey, bAction )

   LOCAL oReg := TReg32():New( nKey, cRegKey, .F. ), uResult
   IF ! oReg:lError
      uResult := Eval( bAction, oReg )
   ENDIF
   oReg:Close()

RETURN uResult


/*
 * FUNCTION IsRegistryKey( nKey, cRegKey )
 *
 * Checks if a registry key exists.
 *
 * Parameters:
 *   nRegistryKey (numeric): The handle of a predefined registry key (e.g., HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER).
 *   cRegistryKey (string):  The path to the registry key to check.
 *
 * Return Value:
 *   (logical): .T. if the key exists, .F. otherwise.
 *
 * Purpose:
 *   This function checks if the specified registry key exists. It creates a TReg32 object to open the key,
 *   and if the object is successfully created (i.e., no error occurred), it means the key exists.
 *
 * Notes:
 *   The TReg32 object is created with lShowError set to .F. to suppress error messages.
 */
FUNCTION IsRegistryKey( nRegistryKey, cRegistryKey )
RETURN WithRegistry( nRegistryKey, cRegistryKey, {| oReg | ! oReg:lError } )

/*
 * FUNCTION CreateRegistryKey( nKey, cRegKey )
 *
 * Creates a registry key.
 *
 * Parameters:
 *   nKey    (numeric): The handle of a predefined registry key (e.g., HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER).
 *   cRegKey (string):  The path to the registry key to create.
 *
 * Return Value:
 *   (logical): .T. if the key was successfully created, .F. otherwise.
 *
 * Purpose:
 *   This function creates the specified registry key. It creates a TReg32 object to create the key,
 *   and if the object is successfully created (i.e., no error occurred), it means the key was created.
 *
 * Notes:
 *   The TReg32 object is created with lShowError set to .F. to suppress error messages.
 */
FUNCTION CreateRegistryKey( nKey, cRegKey )

   LOCAL oReg := TReg32():Create( nKey, cRegKey, .F. )
   LOCAL lSuccess := ( oReg:lError == .F. )
   oReg:Close()

RETURN lSuccess

/*
 * FUNCTION GetRegistryValue( nKey, cRegKey, cRegVar, cType )
 *
 * Retrieves a value from the registry.
 *
 * Parameters:
 *   nKey    (numeric): The handle of a predefined registry key (e.g., HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER).
 *   cRegKey (string):  The path to the registry key containing the value.
 *   cRegVar (string):  The name of the registry variable to retrieve. Optional, defaults to "".
 *   cType   (string):  The expected data type of the registry value (e.g., "C", "N", "D", "L"). Optional, defaults to "C".
 *
 * Return Value:
 *   (variant): The retrieved value, converted to the specified data type. Returns NIL if an error occurs or the value does not exist.
 *
 * Purpose:
 *   This function retrieves the value of the specified registry variable from the specified registry key.
 *   It creates a TReg32 object to access the registry, retrieves the value using the Get method, and converts
 *   the value to the specified data type. If an error occurs, it returns NIL.
 *
 * Notes:
 *   The _InitValueByType function is used to provide a default value for the Get method.
 */
FUNCTION GetRegistryValue( nKey, cRegKey, cRegVar, cType )
   cRegVar := hb_defaultValue( cRegVar, '' )
   cType := hb_defaultValue( cType, 'C' )

RETURN WithRegistry( nKey, cRegKey, {| oReg | ;
      oReg:Get( cRegVar, _InitValueByType( cType ) ) } )

/*
 * FUNCTION SetRegistryValue( nKey, cRegKey, cRegVar, uVal )
 *
 * Sets a value in the registry.
 *
 * Parameters:
 *   nKey    (numeric): The handle of a predefined registry key (e.g., HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER).
 *   cRegKey (string):  The path to the registry key containing the value.
 *   cRegVar (string):  The name of the registry variable to set. Optional, defaults to "".
 *   uVal    (variant): The value to set for the registry variable.
 *
 * Return Value:
 *   (logical): .T. if the value was successfully set, .F. otherwise.
 *
 * Purpose:
 *   This function sets the value of the specified registry variable in the specified registry key.
 *   It creates a TReg32 object to access the registry, sets the value using the Set method, and returns
 *   .T. if the operation was successful.
 */
FUNCTION SetRegistryValue( nKey, cRegKey, cRegVar, uVal )
   cRegVar := hb_defaultValue( cRegVar, '' )

RETURN WithRegistry( nKey, cRegKey, {| oReg | ;
      oReg:Set( cRegVar, uVal ), oReg:nError == ERROR_SUCCESS } )

/*
 * FUNCTION DeleteRegistryVar( nKey, cRegKey, cRegVar )
 *
 * Deletes a variable from the registry.
 *
 * Parameters:
 *   nRegistryKey (numeric): The handle of a predefined registry key (e.g., HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER).
 *   cRegistryKey (string):  The path to the registry key containing the variable.
 *   cRegistryVar (string):  The name of the registry variable to delete. Optional, defaults to "".
 *
 * Return Value:
 *   (logical): .T. if the variable was successfully deleted, .F. otherwise.
 *
 * Purpose:
 *   This function deletes the specified registry variable from the specified registry key.
 *   It creates a TReg32 object to access the registry, deletes the variable using the Delete method, and returns
 *   .T. if the operation was successful.
 */
FUNCTION DeleteRegistryVar( nRegistryKey, cRegistryKey, cRegistryVar )
   cRegistryVar := hb_defaultValue( cRegistryVar, '' )

RETURN WithRegistry( nRegistryKey, cRegistryKey, {| oReg | ;
      oReg:Delete( cRegistryVar ), oReg:nError == ERROR_SUCCESS } )

/*
 * FUNCTION DeleteRegistryKey( nKey, cRegKey )
 *
 * Deletes a registry key.
 *
 * Parameters:
 *   nKey    (numeric): The handle of a predefined registry key (e.g., HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER).
 *   cRegKey (string):  The path to the registry key to delete.
 *
 * Return Value:
 *   (logical): .T. if the key was successfully deleted, .F. otherwise.
 *
 * Purpose:
 *   This function deletes the specified registry key.
 *
 * Notes:
 *   This function directly calls the RegDeleteKey API function.
 */
FUNCTION DeleteRegistryKey( nKey, cRegKey )
RETURN ( RegDeleteKey( nKey, cRegKey ) == ERROR_SUCCESS )


#pragma BEGINDUMP

#include <mgdefs.h>

extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );
typedef BOOL ( WINAPI *LPFN_ISWOW64PROCESS ) ( HANDLE, PBOOL );

/*
 * HB_FUNC_STATIC( ISWOW64 )
 *
 * Determines if the current process is running in a WOW64 environment (32-bit process on a 64-bit OS).
 *
 * Parameters:
 *   None
 *
 * Return Value:
 *   (logical): .T. if the process is running in a WOW64 environment, .F. otherwise.
 *
 * Purpose:
 *   This function checks if the current process is running in a WOW64 (Windows 32-bit on Windows 64-bit) environment.
 *   It uses the IsWow64Process API function to determine this.
 *
 * Notes:
 *   The IsWow64Process API function is only available on Windows XP SP2 and later.
 *   The wapi_GetProcAddress function is used to dynamically load the IsWow64Process API function.
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
