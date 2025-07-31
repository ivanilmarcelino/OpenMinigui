/*
   HotKeys Demo - HMG internal hotkeys processing
   Author: Pablo César Arrascaeta
   Date: March 28, 2017
   Version: 1.0

   Updated: March 02, 2021
*/

#include "hmg.ch"

MEMVAR aOriginalKeys

/*
 * FUNCTION Main()
 *
 * Initializes the application, loads the demo window, defines hotkeys, and activates the window.
 *
 * Purpose:
 *   This is the main entry point of the application. It demonstrates the use of HMG Extended's internal hotkey processing.
 *   It loads a pre-defined window named "Demo", assigns actions to specific hotkey combinations (F2, ALT+C, ALT+D),
 *   stores the original hotkey actions, centers the window on the screen, and activates it to make it visible.
 *   The storing of original hotkey actions is important to be able to restore them later.
 */
FUNCTION Main()

   Load Window Demo

   ON KEY F2    OF Demo ACTION MsgInfo ( 'F2 pressed' )
   ON KEY ALT+C OF Demo ACTION MsgInfo ( 'ALT+C pressed' )
   ON KEY ALT+D OF Demo ACTION MsgInfo ( 'ALT+D pressed' )

   PRIVATE aOriginalKeys := GetAllHotKeysActions()

   Demo.Center
   Demo.Activate

RETURN NIL

/*
 * FUNCTION ShowAllActiveHotKeys( cParentForm )
 *
 * Retrieves and formats a list of all active hotkeys associated with a given form.
 *
 * Parameters:
 *   cParentForm (STRING): The name of the form for which to retrieve the hotkeys.
 *
 * Returns:
 *   aRet (ARRAY): An array of strings, where each string represents a formatted hotkey combination (e.g., "Alt+C", "F2").
 *                 Returns an empty array if no hotkeys are found for the specified form.
 *
 * Purpose:
 *   This function is used to display the currently active hotkeys for a specific form. It iterates through the internal
 *   HMG Extended control handles, identifies HOTKEY controls associated with the given form, and formats the hotkey
 *   combinations into human-readable strings. This is useful for debugging or displaying available hotkeys to the user.
 *
 * Notes:
 *   The aMods and acKeys arrays provide mappings between modifier keys and key codes to their string representations.
 *   If a key code is not found in the anKeys array, an error message is displayed.
 */
FUNCTION ShowAllActiveHotKeys( cParentForm )

   LOCAL i, n, nParentFormHandle, nControlCount, aRet := {}, cTemp := ""
   LOCAL aMods := { "Alt", "Control", "", "Shift", "", "", "", "Win" }
   LOCAL anKeys := {  8,  9, 13, 27, 35, 36, 37, 38, 39, 40, 45, 46, 33, 34, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, ;
      65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, ;
      89, 90, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123 }
   LOCAL acKeys := { "BACK", "TAB", "RETURN", "ESCAPE", "END", "HOME", "LEFT", "UP", "RIGHT", "DOWN", "INSERT", "DELETE", "PRIOR", "NEXT", ;
      "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", ;
      "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12" }

   nParentFormHandle := GetFormhandle ( cParentForm )
   nControlCount := Len ( _HMG_aControlHandles )

   FOR i := 1 TO nControlCount

      IF _HMG_aControlType[ i ] == 'HOTKEY' .AND. _HMG_aControlParentHandles[ i ] == nParentFormHandle

         IF !Empty( _HMG_aControlPageMap[ i ] )
            cTemp := aMods[ _HMG_aControlPageMap[ i ] ]
         ELSE
            cTemp := ""
         ENDIF

         IF !Empty( cTemp )
            cTemp += "+"
         ENDIF

         n := AScan( anKeys, _HMG_aControlValue[ i ] )
         IF n == 0
            MsgStop( "Key number not found!", "Identify & add it" )
            LOOP
         ENDIF

         cTemp += acKeys[ n ]
         AAdd( aRet, cTemp )

      ENDIF

   NEXT i

RETURN aRet

/*
 * FUNCTION ReleaseAllActiveHotKeys( cParentForm )
 *
 * Releases (erases) all active hotkeys associated with a given form.
 *
 * Parameters:
 *   cParentForm (STRING): The name of the form for which to release the hotkeys.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function is used to remove all hotkey definitions from a specific form. It iterates through the internal
 *   HMG Extended control handles, identifies HOTKEY controls associated with the given form, and calls the _EraseControl
 *   function to remove them. This is useful for dynamically changing hotkey assignments or cleaning up hotkeys when a form is closed.
 */
FUNCTION ReleaseAllActiveHotKeys( cParentForm )

   LOCAL i, nParentFormHandle, nControlCount, z

   nParentFormHandle := GetFormhandle ( cParentForm )
   nControlCount := Len ( _HMG_aControlHandles )
   z := GetFormIndex ( cParentForm )

   FOR i := 1 TO nControlCount

      IF _HMG_aControlType[ i ] == 'HOTKEY' .AND. _HMG_aControlParentHandles[ i ] == nParentFormHandle
         _EraseControl( i, z )
      ENDIF

   NEXT i

RETURN NIL

/*
 * FUNCTION GetAllHotKeysActions()
 *
 * Retrieves information about all defined hotkeys in the application.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   aRet (ARRAY): An array of arrays. Each inner array contains information about a single hotkey, including:
 *     - Parent Form Handle (NUMERIC): The handle of the form to which the hotkey is assigned.
 *     - Modifier Key (NUMERIC): A numeric code representing the modifier key (e.g., ALT, CTRL, SHIFT).
 *     - Key Code (NUMERIC): The numeric code of the key associated with the hotkey.
 *     - Action Procedure (CODEBLOCK): The codeblock to execute when the hotkey is pressed.
 *
 * Purpose:
 *   This function is used to create a snapshot of all currently defined hotkeys. This is useful for saving the hotkey
 *   configuration, restoring it later, or performing other operations on the hotkey definitions.  This function is used to save the original hotkey settings before defining new ones.
 */
FUNCTION GetAllHotKeysActions()

   LOCAL i, aRet := {}
   LOCAL nControlCount := Len ( _HMG_aControlHandles )

   FOR i := 1 TO nControlCount

      IF _HMG_aControlType[ i ] == 'HOTKEY'
         AAdd( aRet, { _HMG_aControlParentHandles[ i ], _HMG_aControlPageMap[ i ], _HMG_aControlValue[ i ], _HMG_aControlProcedures[ i ] } )
      ENDIF

   NEXT i

RETURN aRet

/*
 * FUNCTION RestoreAllHotKeysActions()
 *
 * Restores hotkey definitions based on a previously saved configuration.
 *
 * Purpose:
 *   This function is used to restore hotkey definitions from a previously saved configuration, typically stored in the aOriginalKeys variable.
 *   It iterates through the saved hotkey information and re-defines the hotkeys using the _DefineHotKey function. This is useful for restoring
 *   the original hotkey configuration after temporary modifications or when the application is restarted.
 *
 * Notes:
 *   The function relies on the aOriginalKeys MEMVAR, which should contain an array of hotkey information as returned by the GetAllHotKeysActions function.
 *   The GetFormIndexByHandle and GetFormNameByIndex functions are used to retrieve the form name based on its handle.
 */
FUNCTION RestoreAllHotKeysActions()

   LOCAL i, cFName, nIndexForm
   LOCAL nLen := Len( aOriginalKeys )

   FOR i := 1 TO nLen

      nIndexForm := GetFormIndexByHandle( aOriginalKeys[ i, 1 ] )

      IF nIndexForm > 0

         cFName := GetFormNameByIndex( nIndexForm )
         IF _IsWindowDefined( cFName )
            // _DefineHotKey ( cParentForm , nMod , nKey , bAction )
            _DefineHotKey( cFName, aOriginalKeys[ i, 2 ], aOriginalKeys[ i, 3 ], aOriginalKeys[ i, 4 ] )
         ENDIF

      ENDIF

   NEXT i

RETURN NIL
