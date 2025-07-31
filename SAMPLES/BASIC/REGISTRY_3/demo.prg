/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Writing Registry Value as Binary
 * Demo was contributed to HMG forum by Edward 18/May/2017
 *
 * Adapted by Grigory Filatov for MiniGUI Extended Edition
 */

#include "minigui.ch"
#include "hbwin.ch"

/*
 * PROCEDURE Main()
 *
 * Defines the main window and its menu, and activates the application.
 *
 * Purpose:
 *   This is the entry point of the demo application. It creates the main window,
 *   defines a menu with options to write and delete a test registry value, and
 *   then activates the window, making it visible to the user.  The main window
 *   serves as the user interface for demonstrating the registry write functionality.
 *
 * Notes:
 *   The menu items call the WriteRegistryTest() and DeleteRegistryTest() procedures
 *   to perform the actual registry operations.
 */
PROCEDURE Main()

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 350 ;
         HEIGHT 300 ;
         TITLE 'Registry BINARY Value Test' ;
         MAIN

      DEFINE MAIN MENU

         DEFINE POPUP "Test"
            MENUITEM 'Write TEST Value' ACTION WriteRegistryTest()
            MENUITEM 'Delete TEST Value' ACTION DeleteRegistryTest()
            SEPARATOR
            ITEM 'Exit' ACTION Form_1.RELEASE
         END POPUP

      END MENU

   END WINDOW

   Form_1.CENTER
   Form_1.ACTIVATE

RETURN

/*
 * PROCEDURE WriteRegistryTest()
 *
 * Writes binary data to the Windows Registry under the HKCU\_TEST key.
 *
 * Purpose:
 *   This procedure demonstrates how to write binary data to the Windows Registry
 *   using the win_regWriteEx() function. It first prompts the user for confirmation,
 *   then defines a binary array and a binary string.  It then writes both the array
 *   and the string to the registry under different value names ("a" and "c" respectively).
 *   This allows comparison of writing binary data using different methods.
 *
 * Notes:
 *   The procedure uses the MsgYesNo() function to get user confirmation before
 *   writing to the registry. The registry key "HKCU\_TEST" will be created if it
 *   doesn't already exist. The binary data represents a path to notepad.exe and VBSedit.
 */
PROCEDURE WriteRegistryTest()

   LOCAL hKey := "HKCU\"
   LOCAL cKey := "_TEST\"
   LOCAL aReg, cReg := "", nByte

   IF MsgYesNo( 'This will add a binary TEST value into the Windows Registry.', 'Are you sure?' )

      // binary array
      aReg := { 0x6e, 0x00, 0x6f, 0x00, 0x74, 0x00, 0x65, 0x00, 0x70, 0x00, 0x61, 0x00, 0x64, 0x00, 0x2e, 0x00, 0x65, 0x00, 0x78, 0x00, 0x65, 0x00, 0x00, ;
         0x00, 0x44, 0x00, 0x3a, 0x00, 0x5c, 0x00, 0x44, 0x00, 0x45, 0x00, 0x56, 0x00, 0x5c, 0x00, 0x53, 0x00, 0x63, 0x00, 0x72, 0x00, 0x69, 0x00, 0x70, 0x00, ;
         0x74, 0x00, 0x45, 0x00, 0x64, 0x00, 0x69, 0x00, 0x74, 0x00, 0x5c, 0x00, 0x56, 0x00, 0x42, 0x00, 0x53, 0x00, 0x45, 0x00, 0x64, 0x00, 0x69, 0x00, 0x74, ;
         0x00, 0x5c, 0x00, 0x56, 0x00, 0x42, 0x00, 0x53, 0x00, 0x00, 0x00 }

      // binary string
      FOR EACH nByte IN aReg
         cReg += Chr( nByte )
      NEXT

      // writing registry value as binary array
      MsgInfo( win_regWriteEx( hKey + cKey + "a", aReg, WIN_REG_BINARY ), 'Binary Array' )

      // writing registry value as binary string
      MsgInfo( win_regWriteEx( hKey + cKey + "c", cReg, WIN_REG_BINARY ), 'Binary String' )

   ENDIF

RETURN

/*
 * PROCEDURE DeleteRegistryTest()
 *
 * Deletes the test registry values and the test key created by WriteRegistryTest().
 *
 * Purpose:
 *   This procedure demonstrates how to delete registry values and keys using the
 *   win_regDelete() function. It deletes the "a" and "c" values under the
 *   "HKCU\_TEST" key, and then attempts to delete the "HKCU\_TEST" key itself.
 *   This cleans up the registry after the WriteRegistryTest() procedure has been run.
 *
 * Notes:
 *   The win_regDelete() function returns .T. if the deletion is successful, and .F.
 *   otherwise. The procedure checks the return values to determine if the deletions
 *   were successful and displays a message to the user.  If the key contains other values,
 *   the key deletion will fail.
 */
PROCEDURE DeleteRegistryTest()

   LOCAL lSuccess

   lSuccess := win_regDelete( "HKCU\_TEST\a" )
   lSuccess := lSuccess .AND. win_regDelete( "HKCU\_TEST\c" )
   lSuccess := lSuccess .AND. win_regDelete( "HKCU\_TEST\" )

   IF lSuccess
      MsgInfo( "Registry TEST value was deleted successfully." )
   ENDIF

RETURN

/*
 * FUNCTION win_regWriteEx( cRegPath, xValue, nType, nRegSam )
 *
 * Writes a value to the Windows Registry, automatically splitting the registry path.
 *
 * Parameters:
 *   cRegPath (STRING): The full registry path, including the HKEY (e.g., "HKCU\Software\MyKey\MyValue").
 *   xValue (MIXED): The value to write to the registry.  Can be a string, number, logical, or array.
 *   nType (NUMERIC): The registry data type (e.g., WIN_REG_SZ, WIN_REG_DWORD, WIN_REG_BINARY).
 *   nRegSam (NUMERIC): Optional. The security access mask for the registry key. Defaults to 0.
 *
 * Return Value:
 *   LOGICAL: .T. if the write operation was successful, .F. otherwise.
 *
 * Purpose:
 *   This function simplifies writing to the registry by automatically splitting the
 *   provided registry path into its component parts (HKEY, key name, and entry name)
 *   and then calling the win_regSetEx() function to perform the actual write operation.
 *   This avoids the need for the caller to manually parse the registry path.
 *
 * Notes:
 *   This function is a wrapper around win_regSetEx().  It uses win_regPathSplit() to
 *   parse the registry path.
 */
FUNCTION win_regWriteEx( cRegPath, xValue, nType, nRegSam )

   LOCAL nHKEY, cKey, cEntry

   win_regPathSplit( cRegPath, @nHKEY, @cKEY, @cEntry )

RETURN win_regSetEx( nHKEY, cKey, cEntry, xValue, nType, nRegSam )

/*
 * FUNCTION win_regSetEx( nHKEY, cKeyName, cEntryName, xValue, nValueType, nRegSam )
 *
 * Sets a value in the Windows Registry under the specified key.
 *
 * Parameters:
 *   nHKEY (NUMERIC): The handle to the HKEY (e.g., HKEY_CURRENT_USER).
 *   cKeyName (STRING): The name of the registry key.
 *   cEntryName (STRING): The name of the registry entry (value).
 *   xValue (MIXED): The value to write to the registry.  The data type depends on nValueType.
 *   nValueType (NUMERIC): The registry data type (e.g., WIN_REG_SZ, WIN_REG_DWORD, WIN_REG_BINARY).
 *   nRegSam (NUMERIC): Optional. The security access mask for the registry key. Defaults to 0.
 *
 * Return Value:
 *   LOGICAL: .T. if the write operation was successful, .F. otherwise.
 *
 * Purpose:
 *   This function is the core function for writing values to the Windows Registry.
 *   It creates the specified registry key (if it doesn't already exist) and then
 *   sets the value of the specified entry under that key.  It handles different
 *   data types by converting the xValue to the appropriate format before writing it
 *   to the registry.
 *
 * Notes:
 *   The function uses win_regCreateKeyEx() to create the registry key.
 *   The function uses win_regSetValueEx() to set the registry value.
 *   The function closes the registry key handle using win_regCloseKey() after the
 *   write operation is complete.
 *   The function supports writing strings, numbers, logicals, and binary data (as strings or arrays) to the registry.
 *   The nRegSam parameter allows specifying the security access mask for the registry key.
 */
FUNCTION win_regSetEx( nHKEY, cKeyName, cEntryName, xValue, nValueType, nRegSam )

   LOCAL cName
   LOCAL lRetVal := .F.
   LOCAL pKeyHandle
   LOCAL nByte

   hb_default( @nRegSam, 0 )

   IF win_regCreateKeyEx( nHKEY, cKeyName, 0, 0, 0, hb_bitOr( KEY_SET_VALUE, nRegSam ), 0, @pKeyHandle )
      /* no support for Arrays, Codeblock ... */
      SWITCH ValType( xValue )
      CASE "A" // added support for array of binary
         IF HB_ISNUMERIC( nValueType ) .AND. nValueType == WIN_REG_BINARY
            cName := ''
            FOR EACH nByte IN xValue
               cName += Chr( nByte )
            NEXT
         ELSE
            cName := NIL
         ENDIF
         EXIT
      CASE "L"
         nValueType := WIN_REG_DWORD
         cName := iif( xValue, 1, 0 )
         EXIT
      CASE "D"
         nValueType := WIN_REG_SZ
         cName := DToS( xValue )
         EXIT
      CASE "N"
         IF ! HB_ISNUMERIC( nValueType ) .OR. ;
               !( nValueType == WIN_REG_DWORD .OR. ;
               nValueType == WIN_REG_DWORD_LITTLE_ENDIAN .OR. ;
               nValueType == WIN_REG_DWORD_BIG_ENDIAN .OR. ;
               nValueType == WIN_REG_QWORD .OR. ;
               nValueType == WIN_REG_QWORD_LITTLE_ENDIAN )
            nValueType := WIN_REG_DWORD
         ENDIF
         cName := xValue
         EXIT
      CASE "C"
      CASE "M"
         // added support for string of binary
         IF ! HB_ISNUMERIC( nValueType ) .OR. ;
               !( nValueType == WIN_REG_SZ .OR. ;
               nValueType == WIN_REG_EXPAND_SZ .OR. ;
               nValueType == WIN_REG_BINARY .OR. ;
               nValueType == WIN_REG_MULTI_SZ )
            nValueType := WIN_REG_SZ
         ENDIF
         cName := xValue
         EXIT
      ENDSWITCH

      IF cName != NIL
         lRetVal := win_regSetValueEx( pKeyHandle, cEntryName, 0, nValueType, cName )
      ENDIF

      win_regCloseKey( pKeyHandle )
   ENDIF

RETURN lRetVal
