#require "hbsqlit3"

/*
 * PROCEDURE Main()
 *
 * Entry point of the application, initiating the login process.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Nil
 *
 * Purpose:
 *   Starts the application by displaying the login window.
 *   Example: Automatically called to begin the application.
 *
 * Notes:
 *   Disables main window until login is complete.
 */
PROCEDURE Main()
   // Display login window to authenticate user
   ShowLogin()

RETURN