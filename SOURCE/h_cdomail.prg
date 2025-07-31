/*
 * Harbour TCDOMail Class v1.0
 * Copyright 2022 Grigory Filatov <gfilatov@gmail.com>
 *
 * This class provides a simple way to send emails using the CDO (Collaboration Data Objects) library
 * in a Harbour/MiniGUI environment.  It encapsulates the necessary CDO objects and settings
 * to send emails with attachments, recipients, and various options like priority and receipts.
 *
 * Require:
 *  - CDO (Collaboration Data Objects) installed and configured on the system.
 *
 * Usage:
 *  1. Create an instance of the TCDOMail class, providing the necessary email parameters.
 *  2. Call the Activate() method to send the email.
 *  3. Check the lSuccess property to determine if the email was sent successfully.
 *
 * Example:
 *
 *    LOCAL oMail := TCDOMail():New( "smtp.example.com", 587, "user@example.com", "password", ;
 *                                  "Subject", "Body", CDO_NORMAL_PRIORITY, .T., ;
 *                                  { "Sender Name", "sender@example.com" }, ;
 *                                  { { "Recipient Name", "recipient@example.com" } }, ;
 *                                  { { "C:\path\to\attachment.txt" } } )
 *    oMail:Activate()
 *    IF oMail:lSuccess
 *       MsgInfo( "Email sent successfully!" )
 *    ELSE
 *       // Error message is displayed within the Activate() method.
 *    ENDIF
 */

#include "minigui.ch"
#include "hbclass.ch"  // Includes Harbour class definition macros.

// CDO Constants - Define constants for CDO settings to improve readability and maintainability.
#define CDO_SENDUSINGPICKUP  1  // Send message using local SMTP service pickup directory.
#define CDO_SENDUSINGPORT    2  // Send the message using SMTP over TCP/IP networking.

#define CDO_AUTH_ANONYMOUS   0  // No authentication
#define CDO_AUTH_BASIC       1  // BASIC clear text authentication
#define CDO_AUTH_NTLM        2  // NTLM, Microsoft proprietary authentication

#define CDO_LOW_PRIORITY     0  // Low importance
#define CDO_NORMAL_PRIORITY  1  // Normal importance
#define CDO_HIGH_PRIORITY    2  // High importance

#define CDO_DSN_DEFAULT      0  // Delivery Status Notification


CLASS TCDOMail

   // CLASSDATA - A block to be evaluated.
   CLASSDATA bEmail

   // DATA - Instance variables to store email properties.
   DATA cSubject, cTextBody          // Subject and body of the email.
   DATA cServer, nPort, cUser, cPass // SMTP server details: server address, port, username, and password.
   DATA lReceipt, nPriority          // Flags for requesting a read receipt and setting email priority.
   DATA aOrigin, aRecipients, aFiles // Arrays to store sender information, recipient lists, and file attachments.

   // DATA - Optional email properties with default values.
   DATA CCopy AS CHARACTER INIT ""   // Carbon Copy recipients (optional).
   DATA nTimeout AS NUMERIC INIT 30  // Connection timeout in seconds (default: 30 seconds).

   // VAR -  A variable to store the success status of the email sending operation.
   VAR lSuccess AS LOGICAL INIT .F.  // Flag indicating whether the email was sent successfully.

   // METHOD New - Constructor method for sending the email.
   METHOD New( cServer, nPort, cUser, cPass, ;
      cSubject, cText, nPriority, lReceipt, aOrigin, aRecipients, ;
      aFiles ) CONSTRUCTOR

   METHOD Activate()                 // Method to send the email.

ENDCLASS

/*
 * METHOD TCDOMail:New( cServer, nPort, cUser, cPass, cSubject, cText, nPriority, lReceipt, aOrigin, aRecipients, aFiles ) CONSTRUCTOR
 *
 * Initializes a new instance of the TCDOMail class with the provided email parameters.
 *
 * Parameters:
 *   cServer     (STRING): The SMTP server address (e.g., "smtp.example.com").
 *   nPort       (NUMERIC): The SMTP server port number (e.g., 465 for SSL).
 *   cUser       (STRING): The SMTP username for authentication.
 *   cPass       (STRING): The SMTP password for authentication.
 *   cSubject    (STRING): The subject of the email.
 *   cText       (STRING): The body text of the email.
 *   nPriority   (NUMERIC): The email priority, using CDO constants (CDO_LOW_PRIORITY, CDO_NORMAL_PRIORITY, CDO_HIGH_PRIORITY).
 *   lReceipt    (LOGICAL): A flag indicating whether to request a read receipt.
 *   aOrigin     (ARRAY): An array containing the sender's name and email address: { "Sender Name", "sender@example.com" }.
 *   aRecipients (ARRAY): An array of recipient arrays, each containing a recipient's name and email address: { { "Recipient Name", "recipient@example.com" }, ... }.
 *   aFiles      (ARRAY): An array of file attachment paths: { { "C:\path\to\attachment.txt" }, ... }.
 *
 * Return Value:
 *   OBJECT: Returns the newly created TCDOMail object (Self).
 *
 * Purpose:
 *   This constructor initializes the TCDOMail object with the necessary information to send an email.
 *   It assigns the provided parameters to the corresponding data members of the class, preparing the
 *   object for the Activate() method, which handles the actual email sending process.  It also sets
 *   default values for optional parameters if they are not provided.
 *
 * Notes:
 *   The aOrigin, aRecipients, and aFiles parameters are expected to be arrays with specific structures.
 *   Incorrectly formatted arrays may lead to errors during the email sending process.
 */
METHOD New( cServer, nPort, cUser, cPass, ;
      cSubject, cText, nPriority, lReceipt, aOrigin, aRecipients, ;
      aFiles ) CLASS TCDOMail

   // DEFAULT - Assign default values to parameters if they are not provided.
   DEFAULT cText := "", cSubject := "", ;
      cServer := "", nPort := 465, cUser := "", cPass := "", ;
      lReceipt := .F., nPriority := CDO_NORMAL_PRIORITY, ;
      aOrigin := {}, aRecipients := {}, aFiles := {}

   // Assign the passed parameters to the class's data members.
   ::cTextBody := cText
   ::cSubject := cSubject
   ::cServer := cServer
   ::nPort := nPort
   ::cUser := cUser
   ::cPass := cPass
   ::lReceipt := lReceipt
   ::nPriority := nPriority
   ::aOrigin := aOrigin
   ::aRecipients := aRecipients
   ::aFiles := aFiles

RETURN Self  // Return the newly created object.


/*
 * METHOD TCDOMail:Activate()
 *
 * Sends the email using the CDO library.
 *
 * Side Effects:
 *   Sets the ::lSuccess property to .T. if the email is sent successfully, otherwise it remains .F.
 *   Displays an error message using MsgStop() if an error occurs during the email sending process.
 *
 * Purpose:
 *   This method is the core of the TCDOMail class. It performs the following steps:
 *     1. Creates the necessary CDO objects (CDO.Message).
 *     2. Sets the email properties (sender, recipients, subject, body, attachments).
 *     3. Configures the SMTP server settings (server address, port, authentication).
 *     4. Sends the email using the CDO library.
 *     5. Handles potential errors using a TRY...CATCH block, displaying an error message if necessary.
 *   The method encapsulates the complex logic of interacting with the CDO library, providing a simple
 *   interface for sending emails from Harbour/MiniGUI applications.
 *
 * Notes:
 *   The method relies on the CDO library being properly installed and configured on the system.
 *   The error handling provides detailed information about the error, including the error code, subcode,
 *   OS code, subsystem, and description.
 *   The method checks for a bEmail block and evaluates it if it exists, allowing for dynamic modification
 *   of email properties before sending.
 */
METHOD Activate() CLASS TCDOMail

   LOCAL oEmailMsg, oError  // CDO objects for email message and error handling.
   LOCAL cSchema := "http://schemas.microsoft.com/cdo/configuration/"  // CDO configuration schema.
   LOCAL cEmailFromName, cEmailFrom, nEl, nLen, cTmp := ""  // Local variables for email address formatting and looping.

   // Check if a bEmail block exists and evaluate it.
   IF ::bEmail != NIL
      Eval( ::bEmail, Self )
      RETURN NIL
   ENDIF

   TRY  // Use a TRY...CATCH block to handle potential errors during the email sending process.

      // Create the CDO.Message object.
      oEmailMsg := CreateObject( "CDO.Message" )

      WITH OBJECT oEmailMsg  // Use a WITH OBJECT block to simplify access to the CDO object's properties and methods.

         // Format the sender's email address.
         cEmailFromName := ::aOrigin[ 1 ]  // Sender's name.
         cEmailFrom := ::aOrigin[ 2 ]      // Sender's email address.

         IF Empty( cEmailFrom )  // If the email address is empty, use the sender's name as the email address.
            cEmailFrom := cEmailFromName
         ELSE  // Otherwise, format the email address as "Sender Name <sender@example.com>".
            cEmailFrom := cEmailFromName + " <" + cEmailFrom + ">"
         ENDIF

         :From := cEmailFrom  // Set the sender's email address.

         // Format the recipient list.
         IF ( nLen := Len( ::aRecipients ) ) > 0  // Check if there are any recipients.

            FOR nEl := 1 TO nLen  // Loop through the recipient list.
               IF Empty( ::aRecipients[ nEl ][ 2 ] )  // If the recipient's email address is empty, use the recipient's name.
                  cTmp := cTmp + ::aRecipients[ nEl ][ 1 ] + iif( nEl = nLen, "", ";" )
               ELSE  // Otherwise, format the email address as "Recipient Name <recipient@example.com>".
                  cTmp := cTmp + ::aRecipients[ nEl ][ 1 ] + " <" + ::aRecipients[ nEl ][ 2 ] + ">" + iif( nEl = nLen, "", ";" )
               ENDIF
            NEXT

            :To = cTmp  // Set the recipient list.

         ENDIF

         :CC := ::CCopy  // Set the carbon copy recipients.
         :BCC := ""   // Set the blind carbon copy recipients (currently empty).
         :Subject := ::cSubject  // Set the email subject.

         // Determine whether to send the email as HTML or plain text.
         IF "<" $ ::cTextBody .AND. ">" $ ::cTextBody  // If the email body contains HTML tags.
            :HTMLBody := ::cTextBody  // Set the email body as HTML.
         ELSE  // Otherwise, send the email as plain text.
            :TextBody := ::cTextBody  // Set the email body as plain text.
         ENDIF

         :BodyPart:Charset := "utf-8"  // Set the character set for the email body.

         // Add attachments.
         IF ( nLen := Len( ::aFiles ) ) > 0  // Check if there are any attachments.
            FOR nEl := 1 TO nLen  // Loop through the attachment list.
               :AddAttachment( ::aFiles[ nEl ][ 1 ] ) // Full path must be informed
            NEXT
         ENDIF

         // Configure the SMTP server settings.
         WITH OBJECT :configuration:Fields

            :Item( cSchema + "smtpserver" ):Value := ::cServer  // Set the SMTP server address.
            :Item( cSchema + "smtpserverport" ):Value := ::nPort  // Set the SMTP server port.
            :Item( cSchema + "sendusing" ):Value := CDO_SENDUSINGPORT  // Set the sending method to SMTP over TCP/IP.
            :Item( cSchema + "smtpauthenticate" ):Value := CDO_AUTH_BASIC  // Set the authentication method to BASIC.
            :Item( cSchema + "smtpusessl" ):Value := ( ::nPort == 465 )  // Enable SSL if the port is 465.
            :Item( cSchema + "sendusername" ):Value := ::cUser  // Set the SMTP username.
            :Item( cSchema + "sendpassword" ):Value := ::cPass  // Set the SMTP password.
            :Item( cSchema + "smtpconnectiontimeout" ):Value := ::nTimeout  // Set the connection timeout.

            :Update()  // Update the configuration settings.

         END WITH

         // Configure email-specific settings (priority, receipts).
         WITH OBJECT oEmailMsg:Fields

            :Item( "urn:schemas:httpmail:importance" ):Value := ::nPriority  // Set the email importance.
            :Item( "urn:schemas:mailheader:X-Priority" ):Value := ::nPriority - 1  // Set the X-Priority header.
            IF ::lReceipt  // If a read receipt is requested.
               :Item( "urn:schemas:mailheader:return-receipt-to" ):Value := cEmailFrom  // Set the return receipt address.
               :Item( "urn:schemas:mailheader:disposition-notification-to" ):Value := cEmailFrom  // Set the disposition notification address.
            ENDIF

            :Update()  // Update the field settings.

         END WITH

         :DSNOptions := CDO_DSN_DEFAULT  // Set the Delivery Status Notification options.

         :Send()  // Send the email.

         ::lSuccess := .T.  // Set the success flag to true.

      END WITH  // End of WITH OBJECT oEmailMsg

   CATCH oError  // Catch any exceptions that occur during the email sending process.

      // Display an error message if the email was not sent.
      MsgStop ( "The email was not sent." + CRLF + ;
         "Error:      " + cValToChar( oError:GenCode ) + CRLF + ;
         "SubCode:   " + cValToChar( oError:SubCode ) + CRLF + ;
         "OSCode:    " + cValToChar( oError:OsCode ) + CRLF + ;
         "SubSystem: " + cValToChar( oError:SubSystem ) + CRLF + ;
         "Description:      " + oError:Description )

      oEmailMsg := NIL

   END  // End of TRY...CATCH block

RETURN NIL 
