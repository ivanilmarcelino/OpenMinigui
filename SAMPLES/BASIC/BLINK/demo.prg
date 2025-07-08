/*
 * MiniGUI Blinking Label Demo
 *
 * This program demonstrates a simple GUI in MiniGUI where a label can blink
 * on and off, controlled by buttons. The program consists of a window with 
 * buttons to start/stop the blinking of a label.
 */

#include "minigui.ch"

PROCEDURE Main()

    // Define the main window (Form_1) with specific properties
    DEFINE WINDOW Form_1 ;
        AT 0,0 ;          // Position the window at the top-left corner of the screen
        WIDTH 400 ;       // Set the window width to 400 pixels
        HEIGHT 200 ;      // Set the window height to 200 pixels
        TITLE 'Hello World!' ;  // The window title is "Hello World!"
        MAIN              // Indicate that this is the main window

        // Define Button_1 which will turn blinking ON for Label_1
        DEFINE BUTTON Button_1
            ROW 10        // Position the button at row 10
            COL 10        // Position the button at column 10
            CAPTION 'Blink ON'  // The button text is "Blink ON"
            // Action: Turn on blinking for Label_1 and update Label_2 to show "ON"
            ACTION ( Form_1.Label_1.Blink := .T., Form_1.Label_2.Value := "ON" )
            DEFAULT .T.   // Set this button as the default one (triggered when "Enter" is pressed)
        END BUTTON

        // Define Button_2 which will turn blinking OFF for Label_1
        DEFINE BUTTON Button_2
            ROW 40        // Position the button at row 40
            COL 10        // Position the button at column 10
            CAPTION 'Blink OFF'  // The button text is "Blink OFF"
            // Action: Turn off blinking for Label_1 and update Label_2 to show "OFF"
            ACTION ( Form_1.Label_1.Blink := .F., Form_1.Label_2.Value := "OFF" )
        END BUTTON

        // Define Button_3 which will close the window (Cancel button)
        DEFINE BUTTON Button_3
            ROW 70        // Position the button at row 70
            COL 10        // Position the button at column 10
            CAPTION 'Cancel'  // The button text is "Cancel"
            ACTION ThisWindow.Release  // Action: Close the window and release resources
        END BUTTON

        // Define Label_1 which will be the label that blinks on and off
        @ 15,150 LABEL Label_1 ;
            VALUE 'Blink Test:' AUTOSIZE ;  // The label's text is "Blink Test:"
            // Action: Turn on blinking and update Label_2 to show "ON"
            ACTION ( This.Blink := .T., Form_1.Label_2.Value := "ON" )

        // Define Label_2 which displays the current status ("ON" or "OFF")
        @ 15,220 LABEL Label_2 ;
            VALUE Space(6) AUTOSIZE ;  // Set an empty space initially (6 spaces)
            TRANSPARENT ;  // Make the label background transparent
            // Action: Trigger Button_2's click event, which turns off the blinking
            ACTION Form_1.Button_2.OnClick

    END WINDOW

    CENTER WINDOW Form_1  // Center the window on the screen

    ACTIVATE WINDOW Form_1  // Activate (display) the window

RETURN
