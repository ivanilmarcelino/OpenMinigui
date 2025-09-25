#include "minigui.ch"

STATIC g_cUser := ""

/*
 * PROCEDURE ShowLogin()
 *
 * Displays the login window for user authentication.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Nil
 *
 * Purpose:
 *   Presents a modal login window to capture the username before accessing the main UI.
 *   Example: Called at application startup to authenticate the user.
 *
 * Notes:
 *   Disables the main window until login is complete.
 *   Loads saved username from app.ini as the default value.
 */
PROCEDURE ShowLogin()
   // Load saved username from configuration file
   g_cUser := LoadAppSettings()
   // Disable main window during login
   SET WINDOW MAIN OFF
   // Create modal login window with white background
   DEFINE WINDOW oLogin TITLE "Login" ;
         AT 0, 0 WIDTH 400 HEIGHT 200 ;
         MODAL ;
         BACKCOLOR { 255, 255, 255 } ;
         FONT "Segoe UI" SIZE 10
      // Display username label
      @ 30, 30 LABEL NUL VALUE "Username:" WIDTH 80 HEIGHT 24 BACKCOLOR { 255, 255, 255 }
      // Create textbox with saved username, update g_cUser on change
      @ 30, 120 TEXTBOX txtLoginUser VALUE g_cUser WIDTH 150 HEIGHT 24 ON CHANGE g_cUser := This.VALUE
      // Create login button to trigger OnLogin()
      @ 70, 120 BUTTONEX btnLogin CAPTION "Login" FLAT ACTION {|| OnLogin() } ;
         BACKCOLOR { 230, 230, 250 } NOXPSTYLE
   // End window definition
   END WINDOW
   // Center login window on screen
   CENTER WINDOW oLogin
   // Activate and display login window
   ACTIVATE WINDOW oLogin
RETURN

/*
 * PROCEDURE OnLogin()
 *
 * Processes the login action and proceeds to the main UI.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Nil
 *
 * Purpose:
 *   Validates the username and transitions to the main UI if valid.
 *   Example: Called when the Login button is clicked.
 *
 * Notes:
 *   Displays an error if the username is empty.
 *   Closes the login window and enables the main UI on success.
 */
PROCEDURE OnLogin()
   // Check if username is empty
   IF Empty( g_cUser )
      // Show error message if username is empty
      MsgStop( "Username required." )
      RETURN
   ENDIF
   // Close login window
   RELEASE WINDOW oLogin
   // Enable main window
   SET WINDOW MAIN ON
   // Initialize main UI
   InitGUI()
RETURN