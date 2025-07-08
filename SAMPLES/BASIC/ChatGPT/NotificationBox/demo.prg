/*
 * Harbour MiniGUI Extended - NotificationBox with Auto-Close Feature
 * -------------------------------------------------------------------
 * This example demonstrates how to create a temporary NotificationBox
 * using AlertInfo(), AlertExclamation(), and AlertStop() for 
 * different types of messages.
 * 
 * The NotificationBox:
 * Displays an alert for Info, Warning, or Error messages.
 * Uses a Timer to automatically close the alert after a few seconds.
 * 
 * Features:
 * "Show Info" - Displays an information message for 3 seconds.
 * "Show Warning" - Displays a warning alert for 3 seconds.
 * "Show Error" - Displays an error message for 3 seconds.
 * 
 * How it Works:
 * ShowNotification(): Displays an alert and starts a timer.
 * The timer waits for nDuration seconds and then closing the alert.
 * 
 * Advantages:
 * No need for additional GUI elements.
 * Uses built-in alert functions.
 * Automatically closes alerts without user interaction.
 * Lightweight and efficient.
 *
 * Developed for Harbour MiniGUI Extended.
 * Perfect for displaying temporary status notifications in GUI applications.
 */

#include "minigui.ch"

FUNCTION Main()
   DEFINE WINDOW Win_Main ;
         AT 100, 100 ;
         WIDTH 400 ;
         HEIGHT 300 ;
         TITLE "NotificationBox Example" ;
         MAIN

      @ 50, 50 BUTTON btnInfo CAPTION "Show Info" ACTION ShowNotification( "Information Message", "info", 3 ) WIDTH 150 HEIGHT 30
      @ 100, 50 BUTTON btnWarning CAPTION "Show Warning" ACTION ShowNotification( "Warning Alert!", "warning", 3 ) WIDTH 150 HEIGHT 30
      @ 150, 50 BUTTON btnError CAPTION "Show Error" ACTION ShowNotification( "An Error Occurred!", "error", 3 ) WIDTH 150 HEIGHT 30

   END WINDOW

   ACTIVATE WINDOW Win_Main

RETURN NIL

FUNCTION ShowNotification( cMessage, cType, nDuration )

   LOCAL cTitle := "Notification"

   // Show the appropriate Alert Box
   DO CASE
   CASE cType == "warning"
      AlertExclamation( cMessage, { cTitle, nDuration } )
   CASE cType == "error"
      AlertStop( cMessage, { cTitle, nDuration } )
   OTHERWISE
      AlertInfo( cMessage, { cTitle, nDuration } )
   ENDCASE

RETURN NIL
