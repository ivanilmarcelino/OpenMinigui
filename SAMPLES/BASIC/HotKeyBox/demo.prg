/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * HMG HOTKEYBOX demo
 * (C) 2006 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

STATIC nKey := 0, nModif := 0

/*
 * PROCEDURE Main
 *
 * Initializes the main window, defines UI elements, and sets up hotkey functionality.
 *
 * Purpose:
 *   This is the main entry point of the application. It performs the following tasks:
 *     1. Reads a previously saved hotkey from an INI file.
 *     2. Defines the main window with a HotKeyBox, EditBox, and several buttons.
 *     3. Sets the initial value of the HotKeyBox based on the loaded hotkey.
 *     4. Defines actions for the buttons, including setting a new hotkey, setting a default hotkey, and simulating a key press.
 *     5. Registers the hotkey using _DefineHotKey, which triggers a message when the hotkey is pressed.
 *     6. Centers and activates the main window.
 *
 * Notes:
 *   The "demo.ini" file stores the saved hotkey.
 *   The _DefineHotKey function is a custom function that registers the hotkey with the operating system.
 *   The HMG_PressKey function simulates a key press.
 */
PROCEDURE Main
   LOCAL Key := 0

   BEGIN INI FILE "demo.ini"
      GET Key SECTION "Save" ENTRY "HotKey"
   END INI

   DEFINE WINDOW Form_Main ;
      AT 0, 0 ;
      WIDTH 640 HEIGHT 480 ;
      TITLE 'HotKeyBox Demo (Contributed by Grigory Filatov)' ;
      MAIN ;
      ON RELEASE SaveHotKey()

   @ 20, 200 HOTKEYBOX HotKey_1 ;
      VALUE Key ;
      WIDTH 120 HEIGHT 21 ;
      FONT 'Tahoma' SIZE 9 ;
      TOOLTIP "HotkeyBox Control 1" ;
      ON CHANGE AddText( GetHotKeyName( HotKey_1, Form_Main ) )

   @ 20, 20 EDITBOX Editbox_1 VALUE "" WIDTH 150 HEIGHT 400

   @ 60, 200 BUTTON btn_1 CAPTION "Set HotKey" ACTION SetNewHotKey()

   @ 20, 330 BUTTON btn_2 CAPTION "Default" ;
      WIDTH 80 HEIGHT 22 ;
      ACTION ( Form_Main.HotKey_1.Value := 833, SetNewHotKey() )  // Ctrl+Shift+A

   @ 20, 420 BUTTON btn_3 CAPTION "Press Default key" ;
      WIDTH 120 HEIGHT 22 ;
      ACTION HMG_PressKey( VK_CONTROL, VK_SHIFT, VK_A )

   END WINDOW

   IF !Empty( Key )
      SetNewHotKey()
   ENDIF

   CENTER WINDOW Form_Main

   ACTIVATE WINDOW Form_Main

RETURN

/*
 * FUNCTION SaveHotKey()
 *
 * Saves the current hotkey value from the HotKeyBox to an INI file.
 *
 * Purpose:
 *   This function is called when the main window is released (closed). It saves the current value of the HotKeyBox control to the "demo.ini" file, so that the hotkey can be restored when the application is next run.
 *
 * Notes:
 *   The "demo.ini" file stores the saved hotkey.
 */
FUNCTION SaveHotKey()
   LOCAL Key := Form_Main.HotKey_1.Value

   BEGIN INI FILE "demo.ini"
      SET SECTION "Save" ENTRY "HotKey" TO Key
   END INI

RETURN NIL

/*
 * FUNCTION AddText( t )
 *
 * Appends text to the EditBox control in the main window.
 *
 * Parameters:
 *   t (STRING): The text to append to the EditBox.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function is called when the HotKeyBox value changes. It appends the name of the new hotkey to the EditBox control, providing visual feedback to the user.
 *
 * Notes:
 *   The EditBox control displays a history of the selected hotkeys.
 */
FUNCTION AddText( t )
   LOCAL a := Form_Main.Editbox_1.Value

   a += t + CRLF
   Form_Main.Editbox_1.Value := a

RETURN NIL

/*
 * FUNCTION SetNewHotKey()
 *
 * Registers a new hotkey using the _DefineHotKey function.
 *
 * Purpose:
 *   This function is called when the user changes the hotkey in the HotKeyBox or clicks the "Set HotKey" button. It performs the following steps:
 *     1. Unregisters the previously registered hotkey (if any) using _ReleaseHotKey.
 *     2. Gets the key code and modifier keys from the HotKeyBox using GetHotKeyValue.
 *     3. Registers the new hotkey using _DefineHotKey, which triggers a message when the hotkey is pressed.
 *
 * Notes:
 *   The GetHotKeyValue function retrieves the key code and modifier keys from the HotKeyBox control.
 *   The global variables nKey and nModif store the previously registered hotkey's key code and modifier keys, respectively.
 */
FUNCTION SetNewHotKey()
   LOCAL cKeyName
   LOCAL aKey := GetHotKeyValue( HotKey_1, Form_Main )

   IF !Empty( nKey )
      _ReleaseHotKey( "Form_Main", nModif, nKey )
   ENDIF

   nKey := aKey[ 1 ]
   nModif := aKey[ 2 ]
   cKeyName := GetHotKeyName( HotKey_1, Form_Main )

   _DefineHotKey( "Form_Main", nModif, nKey, {|| MsgInfo( StrTran( cKeyName, " ", "" ) + " is pressed" ) } )

RETURN NIL
