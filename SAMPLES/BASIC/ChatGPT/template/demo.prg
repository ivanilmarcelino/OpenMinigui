/******************************************
 * Harbour MiniGUI Program Template
 * This example demonstrates the use of a window, menu, toolbar,
 * status bar, controls, and event handling in a Harbour MiniGUI application.
 ******************************************/

#include "minigui.ch"

FUNCTION Main()

    LOCAL cText := ""   // Variable to store text entered by the user

    // Define the main application window with size and position
    DEFINE WINDOW oWnd ;
        AT 100, 100 WIDTH 600 HEIGHT 400 ;  // Window position and size
        TITLE "Harbour MiniGUI Example" ;   // Window title
        MAIN ;  // Specifies this is the main application window
        ON INIT MsgInfo("Welcome to Harbour MiniGUI") ;  // Event: Display a message when the window is initialized
        ON RELEASE MsgInfo("Goodbye!")      // Event: Display a message when the window is closed

    END WINDOW

    // Define a menu for the window with "File" and "Edit" options
    DEFINE MAIN MENU OF oWnd
        POPUP "File"          // First menu: "File"
            MENUITEM "Open" ACTION MsgInfo("Open selected")   // Menu item: Open
            MENUITEM "Save" ACTION MsgInfo("Save selected")   // Menu item: Save
            SEPARATOR   // A separator line between menu items
            MENUITEM "Exit" ACTION EXIT PROGRAM    // Menu item: Exit the program
        END POPUP

        POPUP "Edit"         // Second menu: "Edit"
            MENUITEM "Copy" ACTION MsgInfo("Copy selected")   // Menu item: Copy
            MENUITEM "Paste" ACTION MsgInfo("Paste selected") // Menu item: Paste
        END POPUP
    END MENU

    // Define a SplitBox to manage layout of window contents (optional)
    DEFINE SPLITBOX OF oWnd

    // Define a toolbar with three buttons (New, Open, Save)
    DEFINE TOOLBAR oToolbar OF oWnd BUTTONSIZE 18, 18   // Toolbar with button size 18x18
        BUTTON b1 ;   // Button 1
            PICTURE "new.bmp" ;   // Button icon (bitmap image)
            TOOLTIP "New" ;       // Tooltip text shown on hover
            ACTION MsgInfo("New file")   // Event: Action when the button is clicked

        BUTTON b2 ;   // Button 2
            PICTURE "open.bmp" ;  // Button icon for Open action
            TOOLTIP "Open" ;      // Tooltip for Open
            ACTION MsgInfo("Open file")   // Event: Open file action

        BUTTON b3 ;   // Button 3
            PICTURE "save.bmp" ;  // Button icon for Save
            TOOLTIP "Save" ;      // Tooltip for Save
            ACTION MsgInfo("Save file")   // Event: Save file action
    END TOOLBAR

    END SPLITBOX   // End of SplitBox definition

    // Define a status bar at the bottom of the window to show information
    DEFINE STATUSBAR OF oWnd KEYBOARD PROMPT "Ready"   // Status bar with the text "Ready"
    END STATUSBAR

    // Define controls (label, buttons, and textbox) inside the main window
    @ 50, 10 LABEL oLabel VALUE "Hello, World!" WIDTH 200 HEIGHT 24 OF oWnd   // Label control with the text "Hello, World!"

    // Button 1: Displays a message when clicked
    @ 80, 10 BUTTON oBtn1 OF oWnd CAPTION "Click Me" WIDTH 100 HEIGHT 30 ;  // Button with caption "Click Me"
        ACTION MsgInfo("Button Clicked!")   // Event: Action triggered on button click

    // Textbox control to allow user to enter text, and update `cText` variable on change
    @ 120, 10 TEXTBOX oText VALUE cText WIDTH 200 HEIGHT 24 OF oWnd ON CHANGE cText := This.Value   // Textbox for user input

    // Button 2: Displays the text entered in the textbox when clicked
    @ 150, 10 BUTTON oBtn2 OF oWnd CAPTION "Show Text" WIDTH 100 HEIGHT 30 ;  // Button with caption "Show Text"
        ACTION MsgInfo("You entered: " + cText)   // Event: Show entered text in a message box

    // Center the window on the screen
    CENTER WINDOW oWnd

    // Activate the window (make it visible and start processing events)
    ACTIVATE WINDOW oWnd

RETURN NIL
