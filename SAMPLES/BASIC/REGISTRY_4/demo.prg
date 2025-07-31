/*
 * GUI Test Harness for Registry Wrapper
 * Author: Adapted by G.Filatov for HMG Extended
 */

#include "hmg.ch"

/*
 * PROCEDURE Main()
 *
 * Initializes the GUI application for testing the registry wrapper functions.
 *
 * Purpose:
 *   This function serves as the entry point for the registry wrapper test GUI.
 *   It defines the main window (Form_1) with its associated controls (labels, combo box, edit box, and buttons).
 *   The controls are used to interact with the registry wrapper functions, allowing the user to create, read, write, and delete registry keys and values.
 *   The function also centers and activates the main window, making it visible to the user.
 *
 * Notes:
 *   The GUI provides a simple interface for testing the functionality of the registry wrapper functions.
 *   The user can select the registry value type from the combo box, enter the value in the edit box, and click the buttons to perform the corresponding registry operations.
 */
PROCEDURE Main()

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 500 ;
         HEIGHT 540 ;
         TITLE "Registry Wrapper Test" ;
         MAIN

      DEFINE LABEL lblResult
         ROW 20
         COL 20
         WIDTH 550
         VALUE "Click a button to perform registry test"
      END LABEL

      DEFINE COMBOBOX cmbType
         ROW 60
         COL 20
         WIDTH 250
         ITEMS { "REG_SZ", "REG_DWORD", "REG_BINARY", "REG_MULTI_SZ" }
         VALUE 1
      END COMBOBOX

      DEFINE EDITBOX txtValue
         ROW 100
         COL 20
         WIDTH 400
         HEIGHT 60
         VALUE "Success!"
         VSCROLLBAR .F.
         HSCROLLBAR .F.
      END EDITBOX

      DEFINE BUTTON btnCreateKey
         ROW 180
         COL 20
         WIDTH 200
         CAPTION "Create and Write"
         ACTION CreateAndWriteKey()
      END BUTTON

      DEFINE BUTTON btnReadKey
         ROW 220
         COL 20
         WIDTH 200
         CAPTION "Read Value"
         ACTION ReadRegistryValue()
      END BUTTON

      DEFINE BUTTON btnSaveToReg
         ROW 260
         COL 20
         WIDTH 200
         CAPTION "Save to .reg File"
         ACTION SaveToRegFile()
      END BUTTON

      DEFINE BUTTON btnDeleteKey
         ROW 300
         COL 20
         WIDTH 200
         CAPTION "Delete Key"
         ACTION DeleteRegistryKey()
      END BUTTON

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN

/*
 * FUNCTION CreateAndWriteKey()
 *
 * Creates a registry key and writes a value to it.
 *
 * Purpose:
 *   This function creates a registry key under HKEY_CURRENT_USER and writes a value to it.
 *   The key is "Software\\HMG_TestKeyGUI", and the value name is "Test".
 *   The value type and data are obtained from the GUI controls (combo box and edit box).
 *   The function displays the result of the operation in the lblResult label.
 *
 * Notes:
 *   The function uses the REGCREATEKEY, REGOPENKEYEXA, REGSETVALUEEXA, and REGCLOSEKEY Windows API functions.
 *   Error handling is implemented to display appropriate messages in case of failure.
 *   The function converts the value data to the appropriate type based on the selected value type.
 */
FUNCTION CreateAndWriteKey()

   LOCAL hKey := 0
   LOCAL nDisposition := 0
   LOCAL cSubKey := "Software\\HMG_TestKeyGUI"
   LOCAL cValName := "Test"
   LOCAL xValData
   LOCAL nType := GetSelectedType()

   IF nType == REG_DWORD
      xValData := Val( AllTrim( Form_1.txtValue.Value ) )
   ELSEIF nType == REG_BINARY .OR. nType == REG_MULTI_SZ
      xValData := StringToBinary( Form_1.txtValue.Value )
   ELSE
      xValData := Form_1.txtValue.VALUE
   ENDIF

   IF REGCREATEKEY( HKEY_CURRENT_USER, cSubKey, @hKEY, @nDisposition ) == 0
      IF REGOPENKEYEXA( HKEY_CURRENT_USER, cSubKey, 0, KEY_ALL_ACCESS, @hKey ) == 0
         IF REGSETVALUEEXA( hKey, cValName, 0, nType, xValData ) == 0
            Form_1.lblResult.VALUE := "Value written as type: " + TypeToText( nType )
         ELSE
            Form_1.lblResult.VALUE := "Failed to write value."
         ENDIF
         REGCLOSEKEY( hKey )
      ELSE
         Form_1.lblResult.VALUE := "Failed to open key."
      ENDIF
   ELSE
      Form_1.lblResult.VALUE := "Failed to create key."
   ENDIF

RETURN NIL

/*
 * FUNCTION ReadRegistryValue()
 *
 * Reads a value from a registry key.
 *
 * Purpose:
 *   This function reads a value from the registry key "Software\\HMG_TestKeyGUI" under HKEY_CURRENT_USER.
 *   The value name is "Test".
 *   The function determines the value type and displays the value in the lblResult label.
 *
 * Notes:
 *   The function uses the REGOPENKEYEXA, REGQUERYVALUEEXA, and REGCLOSEKEY Windows API functions.
 *   Error handling is implemented to display appropriate messages in case of failure.
 *   The function converts the value data to a string representation based on the value type.
 */
FUNCTION ReadRegistryValue()

   LOCAL hKey := 0
   LOCAL cSubKey := "Software\\HMG_TestKeyGUI"
   LOCAL cValName := "Test"
   LOCAL cBuffer := Space( 1024 )
   LOCAL nType := GetSelectedType()
   LOCAL nSize := 0
   LOCAL cOutput

   IF REGOPENKEYEXA( HKEY_CURRENT_USER, cSubKey, 0, KEY_ALL_ACCESS, @hKey ) == 0
      IF REGQUERYVALUEEXA( hKey, cValName, 0, @nType, @cBuffer, @nSize ) == 0
         DO CASE
         CASE nType == REG_DWORD
            cOutput := "DWORD: " + LTrim( Str( Bin2L( Left(cBuffer,4 ) ) ) )
         CASE nType == REG_BINARY .OR. nType == REG_MULTI_SZ
            cOutput := "BIN: " + BinaryToString( Left( cBuffer, nSize ) )
         OTHERWISE
            cOutput := "STR: " + Left( cBuffer, nSize )
         ENDCASE
         Form_1.lblResult.VALUE := "Read " + TypeToText( nType ) + ": " + cOutput
      ELSE
         Form_1.lblResult.VALUE := "Failed to read value."
      ENDIF
      REGCLOSEKEY( hKey )
   ELSE
      Form_1.lblResult.VALUE := "Failed to open key."
   ENDIF

RETURN NIL

/*
 * FUNCTION SaveToRegFile()
 *
 * Saves a registry key and its value to a .reg file.
 *
 * Purpose:
 *   This function reads a value from the registry key "Software\\HMG_TestKeyGUI" under HKEY_CURRENT_USER and saves it to a .reg file named "test_output.reg".
 *   The function formats the registry data according to the .reg file format.
 *   The function displays the result of the operation in the lblResult label.
 *
 * Notes:
 *   The function uses the REGOPENKEYEXA, REGQUERYVALUEEXA, and REGCLOSEKEY Windows API functions.
 *   Error handling is implemented to display appropriate messages in case of failure.
 *   The function converts the value data to a string representation based on the value type.
 *   The MemoWrit function is used to write the registry data to the .reg file.
 */
FUNCTION SaveToRegFile()

   LOCAL cPath := "test_output.reg"
   LOCAL cSubKey := "Software\\HMG_TestKeyGUI"
   LOCAL cValName := "Test"
   LOCAL cBuffer := Space( 1024 )
   LOCAL nType := GetSelectedType()
   LOCAL nSize := 0
   LOCAL hKey := 0
   LOCAL cRegData := ""
   LOCAL cLine := ""
   LOCAL i, cByte

   IF REGOPENKEYEXA( HKEY_CURRENT_USER, cSubKey, 0, KEY_ALL_ACCESS, @hKey ) == 0
      IF REGQUERYVALUEEXA( hKey, cValName, 0, @nType, @cBuffer, @nSize ) == 0
         cRegData += "Windows Registry Editor Version 5.00" + CRLF + CRLF
         cRegData += "[HKEY_CURRENT_USER\\" + cSubKey + "]" + CRLF

         DO CASE
         CASE nType == REG_SZ
            cRegData += ["] + cValName + ["="] + Left( cBuffer, nSize - 1 ) + ["] + CRLF

         CASE nType == REG_DWORD
            cRegData += ["] + cValName + ["=dword:] + Hex( Bin2L( Left( cBuffer, 4 ) ), 8 ) + CRLF

         CASE nType == REG_BINARY .OR. nType == REG_MULTI_SZ
            cRegData += ["] + cValName + ["=hex:]
            FOR i := 1 TO nSize
               cByte := Hex( Asc( SubStr( cBuffer, i, 1 ) ), 2 )
               cLine += cByte
               IF i < nSize
                  cLine += iif( Mod( i, 16 ) == 0, "," + CRLF + "  ", "," )
               ENDIF
            NEXT
            cRegData += cLine + CRLF
         ENDCASE

         IF hb_MemoWrit( cPath, cRegData )
            Form_1.lblResult.VALUE := "Saved to " + cPath
         ELSE
            Form_1.lblResult.VALUE := "Failed to write file."
         ENDIF
      ELSE
         Form_1.lblResult.VALUE := "Failed to read value."
      ENDIF
      REGCLOSEKEY( hKey )
   ELSE
      Form_1.lblResult.VALUE := "Failed to open key."
   ENDIF

RETURN NIL

/*
 * FUNCTION Hex( nValue, nDigits )
 *
 * Converts a numeric value to its hexadecimal representation.
 *
 * Parameters:
 *   nValue (Numeric): The numeric value to convert.
 *   nDigits (Numeric): The desired number of digits in the hexadecimal representation.
 *
 * Return Value:
 *   Character: The hexadecimal representation of the numeric value, padded with leading zeros if necessary.
 *
 * Purpose:
 *   This function converts a numeric value to its hexadecimal representation and pads it with leading zeros to ensure a specific number of digits.
 *   It is used to format the DWORD value in the .reg file when saving the registry key.
 *
 * Notes:
 *   The function uses the NumToHex function to convert the numeric value to its hexadecimal representation.
 *   The PadL function is used to pad the hexadecimal representation with leading zeros.
 */
FUNCTION Hex( nValue, nDigits )
RETURN PadL( Lower( NumToHex( nValue ) ), nDigits, "0" )

/*
 * FUNCTION DeleteRegistryKey()
 *
 * Deletes a registry key.
 *
 * Purpose:
 *   This function deletes the registry key "Software\\HMG_TestKeyGUI" under HKEY_CURRENT_USER.
 *   The function displays the result of the operation in the lblResult label.
 *
 * Notes:
 *   The function uses the REGDELETEKEY Windows API function.
 *   Error handling is implemented to display appropriate messages in case of failure.
 */
FUNCTION DeleteRegistryKey()

   LOCAL cSubKey := "Software\\HMG_TestKeyGUI"

   IF REGDELETEKEY( HKEY_CURRENT_USER, cSubKey ) == 0
      Form_1.lblResult.VALUE := "Key deleted successfully."
   ELSE
      Form_1.lblResult.VALUE := "Failed to delete key."
   ENDIF

RETURN NIL

/*
 * FUNCTION GetSelectedType()
 *
 * Gets the selected registry value type from the combo box.
 *
 * Parameters:
 *   None
 *
 * Return Value:
 *   Numeric: The numeric representation of the selected registry value type (REG_SZ, REG_DWORD, REG_BINARY, or REG_MULTI_SZ).
 *
 * Purpose:
 *   This function retrieves the selected registry value type from the cmbType combo box and returns its corresponding numeric value.
 *   This value is used when creating or reading registry values to specify the data type.
 */
FUNCTION GetSelectedType()
   SWITCH Form_1.cmbType.VALUE
   CASE 2 ; RETURN REG_DWORD
   CASE 3 ; RETURN REG_BINARY
   CASE 4 ; RETURN REG_MULTI_SZ
   OTHERWISE ; RETURN REG_SZ
   ENDSWITCH

RETURN 0

/*
 * FUNCTION TypeToText(nType)
 *
 * Converts a registry value type code to its text representation.
 *
 * Parameters:
 *   nType (Numeric): The numeric code representing the registry value type.
 *
 * Return Value:
 *   Character: The text representation of the registry value type (e.g., "REG_SZ", "REG_DWORD").
 *
 * Purpose:
 *   This function converts a numeric registry value type code to its corresponding text representation.
 *   This is used to display the value type in the lblResult label.
 */
FUNCTION TypeToText( nType )
   DO CASE
   CASE nType == REG_SZ ; RETURN "REG_SZ"
   CASE nType == REG_DWORD ; RETURN "REG_DWORD"
   CASE nType == REG_BINARY ; RETURN "REG_BINARY"
   CASE nType == REG_MULTI_SZ ; RETURN "REG_MULTI_SZ"
   ENDCASE

RETURN "UNKNOWN"

/*
 * FUNCTION StringToBinary(cStr)
 *
 * Converts a string to a binary string.
 *
 * Parameters:
 *   cStr (Character): The string to convert.
 *
 * Return Value:
 *   Character: The binary representation of the string.
 *
 * Purpose:
 *   This function converts a string to a binary string by converting each character in the string to its ASCII code and then to a character.
 *   This is used to convert the value data to a binary string when the selected value type is REG_BINARY or REG_MULTI_SZ.
 *
 * Notes:
 *   The function iterates over each character in the string and converts it to its ASCII code using the ASC function.
 *   The CHR function is then used to convert the ASCII code back to a character.
 */
FUNCTION StringToBinary( cStr )

   LOCAL i, cOut := ""
   FOR i := 1 TO Len( cStr )
      cOut += Chr( Asc( SubStr( cStr, i, 1 ) ) )
   NEXT

RETURN cOut

/*
 * FUNCTION BinaryToString(cBin)
 *
 * Converts a binary string to a regular string.
 *
 * Parameters:
 *   cBin (Character): The binary string to convert.
 *
 * Return Value:
 *   Character: The string representation of the binary string.
 *
 * Purpose:
 *   This function converts a binary string to a regular string by converting each character in the binary string to its ASCII code and then to a character.
 *   This is used to convert the value data from a binary string to a regular string when reading a REG_BINARY or REG_MULTI_SZ value from the registry.
 *
 * Notes:
 *   The function iterates over each character in the binary string and converts it to its ASCII code using the ASC function.
 *   The CHR function is then used to convert the ASCII code back to a character.
 */
FUNCTION BinaryToString( cBin )

   LOCAL i, cOut := ""
   FOR i := 1 TO Len( cBin )
      cOut += Chr( Asc( SubStr( cBin, i, 1 ) ) )
   NEXT

RETURN cOut
