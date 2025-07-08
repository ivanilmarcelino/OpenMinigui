/*
 * Harbour MiniGUI Extended - ToggleSwitch Control
 * -----------------------------------------------
 * This example demonstrates a ToggleSwitch control implementation 
 * 
 * Features:
 * Looks like a modern On/Off switch.
 * Changes appearance dynamically.
 * Triggers an action when toggled.
 * 
 * How It Works:
 * Toggle Simulates the Switch
 * - Clicking it toggles between "ON" and "OFF".
 * - Background color changes:
 *   - Green (ON)
 *   - Red (OFF)
 * - ToggleSwitch() Handles Click Events
 *   - Changes state (lToggleState).
 *   - Updates label text & color.
 *   - Triggers an action (AlertInfo()).
 * - Helper Functions
 *   - GetToggleCaption(): Returns "ON" or "OFF".
 *   - GetToggleColor(): Returns Green (ON) or Red (OFF).
 * 
 * Advantages:
 * Lightweight & Simple.
 * No External Libraries Required.
 * Looks & Works Like a Real Toggle Switch.
 *
 * Developed for Harbour MiniGUI Extended.
 * Perfect for displaying a modern On/Off switch in GUI applications.
 */

#include "minigui.ch"

STATIC lToggleState := .F.  // Initial state: OFF

FUNCTION Main()
   DEFINE WINDOW Win_1 ;
      AT 100, 100 ;
      WIDTH 400 ;
      HEIGHT 200 ;
      TITLE "ToggleSwitch Example" ;
      MAIN

      @ 50, 100 SWITCHER btnToggle ;
         HEIGHT 46 ;
         VALUE GetToggleCaption() ;
         FONT 'Arial' SIZE 12 ;
         IMAGE { 'MINIGUI_SWITCH_ON', 'MINIGUI_SWITCH_OFF' } ;
         ON MOUSEHOVER ( Win_1.btnToggle.FontColor := BLUE ) ;
         ON MOUSELEAVE ( Win_1.btnToggle.FontColor := GetToggleColor() ) ;
         ONCLICK ToggleSwitch() ;
         TOOLTIP "Click to toggle On/Off"

      Win_1.btnToggle.FontColor := GetToggleColor()

   END WINDOW

   ACTIVATE WINDOW Win_1
RETURN NIL

FUNCTION ToggleSwitch()
   Win_1.btnToggle.Checked := !lToggleState // Change Toggle state

   lToggleState := Win_1.btnToggle.Checked  // Toggle state

   // Update button caption
   Win_1.btnToggle.Value := GetToggleCaption()

   // Perform an action based on the state
   IF lToggleState
      AlertInfo("Switch is now ON", "ToggleSwitch")
   ELSE
      AlertInfo("Switch is now OFF", "ToggleSwitch")
   ENDIF
RETURN NIL

FUNCTION GetToggleCaption()
   RETURN IIF(lToggleState, "ON", "OFF")

FUNCTION GetToggleColor()
   RETURN IIF(lToggleState, {0, 200, 0}, {200, 0, 0})  // Green for ON, Red for OFF
