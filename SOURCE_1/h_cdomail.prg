/*
 * Harbour TCDOMail Class v1.1
 * Copyright 2022-2026 Grigory Filatov <gfilatov@gmail.com>
 *
 * This class provides a simple way to send emails using the CDO
 * (Collaboration Data Objects) library in a Harbour/MiniGUI environment.
 * It encapsulates the necessary CDO objects and settings to send emails
 * with attachments, recipients, and various options like priority and receipts.
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
 *   LOCAL oMail := TCDOMail():New( "smtp.example.com", 587, "user@example.com", "password", ;
 *                                  "Subject", "Body", CDO_NORMAL_PRIORITY, .T., ;
 *                                  { "Sender Name", "sender@example.com" }, ;
 *                                  { { "Recipient Name", "recipient@example.com" } }, ;
 *                                  { { "C:\path\to\attachment.txt" } } )
 *   oMail:Activate()
 *   IF oMail:lSuccess
 *      MsgInfo( "Email sent successfully!" )
 *   ELSE
 *      // Error message is displayed within the Activate() method.
 *   ENDIF
 */

#include "minigui.ch"
#include "hbclass.ch"

*-----------------------------------------------------------------------------*
* FUNCTION hb_Implode( cDelimiter, aItems )
*-----------------------------------------------------------------------------*
*
* Concatenates array elements into a single string using a delimiter.
*
* Parameters:
*    cDelimiter  -> separator string
*    aItems      -> array of values
*
* Returns:
*    Character string
*
* Example:
*
*    hb_Implode( ";", { "A", "B", "C" } )
*    --> "A;B;C"
*
*----------------------------------------------------------------------------*

#xtranslate hb_Implode( <cDelimiter>, <aItems> ) => LB_Array2String( <aItems>, <cDelimiter> )

*-----------------------------------------------------------------------------*
* CDO Constants
*-----------------------------------------------------------------------------*

#define CDO_SENDUSINGPICKUP  1
#define CDO_SENDUSINGPORT    2

#define CDO_AUTH_ANONYMOUS   0
#define CDO_AUTH_BASIC       1
#define CDO_AUTH_NTLM        2

#define CDO_LOW_PRIORITY     0
#define CDO_NORMAL_PRIORITY  1
#define CDO_HIGH_PRIORITY    2

#define CDO_DSN_DEFAULT      0


*-----------------------------------------------------------------------------*
CLASS TCDOMail
*-----------------------------------------------------------------------------*

   CLASSDATA bEmail

   DATA cSubject
   DATA cTextBody

   DATA cServer
   DATA nPort
   DATA cUser
   DATA cPass

   DATA lReceipt
   DATA nPriority

   DATA aOrigin
   DATA aRecipients
   DATA aFiles

   DATA CCopy     AS CHARACTER INIT ""
   DATA nTimeout  AS NUMERIC   INIT 30

   VAR  lSuccess  AS LOGICAL   INIT .F.

   METHOD New( ;
      cServer, ;
      nPort, ;
      cUser, ;
      cPass, ;
      cSubject, ;
      cText, ;
      nPriority, ;
      lReceipt, ;
      aOrigin, ;
      aRecipients, ;
      aFiles ) CONSTRUCTOR

   METHOD Activate()

ENDCLASS


*-----------------------------------------------------------------------------*
METHOD New( ;
      cServer, ;
      nPort, ;
      cUser, ;
      cPass, ;
      cSubject, ;
      cText, ;
      nPriority, ;
      lReceipt, ;
      aOrigin, ;
      aRecipients, ;
      aFiles ) CLASS TCDOMail
*-----------------------------------------------------------------------------*

   DEFAULT ;
      cText        := "", ;
      cSubject     := "", ;
      cServer      := "", ;
      nPort        := 465, ;
      cUser        := "", ;
      cPass        := "", ;
      lReceipt     := .F., ;
      nPriority    := CDO_NORMAL_PRIORITY, ;
      aOrigin      := {}, ;
      aRecipients  := {}, ;
      aFiles       := {}

   ::cTextBody   := cText
   ::cSubject    := cSubject

   ::cServer     := cServer
   ::nPort       := nPort
   ::cUser       := cUser
   ::cPass       := cPass

   ::lReceipt    := lReceipt
   ::nPriority   := nPriority

   ::aOrigin     := aOrigin
   ::aRecipients := aRecipients
   ::aFiles      := aFiles

RETURN Self


*-----------------------------------------------------------------------------*
METHOD Activate() CLASS TCDOMail
*-----------------------------------------------------------------------------*

   LOCAL oMessage
   LOCAL oError

   LOCAL cSchema := ;
      "http://schemas.microsoft.com/cdo/configuration/"

   LOCAL cFrom
   LOCAL cRecipients

   /*-----------------------------------------------------------------------*/
   /* External override callback                                            */
   /*-----------------------------------------------------------------------*/

   IF ::bEmail != NIL
      Eval( ::bEmail, Self )
      RETURN NIL
   ENDIF

   TRY

      oMessage := CreateObject( "CDO.Message" )

      /*--------------------------------------------------------------------*/
      /* Sender / recipients                                                */
      /*--------------------------------------------------------------------*/

      cFrom := _CDOFormatAddress( ::aOrigin )

      cRecipients := ;
         _CDOBuildRecipients( ::aRecipients )

      /*--------------------------------------------------------------------*/
      /* Message body                                                       */
      /*--------------------------------------------------------------------*/

      WITH OBJECT oMessage

         :From    := cFrom
         :To      := cRecipients
         :CC      := ::CCopy
         :BCC     := ""

         :Subject := ::cSubject

         IF _CDOIsHtml( ::cTextBody )
            :HTMLBody := ::cTextBody
         ELSE
            :TextBody := ::cTextBody
         ENDIF

         :BodyPart:Charset := "utf-8"

         /*-----------------------------------------------------------------*/
         /* Attachments                                                     */
         /*-----------------------------------------------------------------*/

         _CDOAddAttachments( oMessage, ::aFiles )

         /*-----------------------------------------------------------------*/
         /* SMTP configuration                                              */
         /*-----------------------------------------------------------------*/

         WITH OBJECT :Configuration:Fields

            :Item( cSchema + "smtpserver" ):Value := ;
               ::cServer

            :Item( cSchema + "smtpserverport" ):Value := ;
               ::nPort

            :Item( cSchema + "sendusing" ):Value := ;
               CDO_SENDUSINGPORT

            :Item( cSchema + "smtpauthenticate" ):Value := ;
               CDO_AUTH_BASIC

            :Item( cSchema + "smtpusessl" ):Value := ;
               ( ::nPort == 465 )

            :Item( cSchema + "sendusername" ):Value := ;
               ::cUser

            :Item( cSchema + "sendpassword" ):Value := ;
               ::cPass

            :Item( cSchema + "smtpconnectiontimeout" ):Value := ;
               ::nTimeout

            :Update()

         END WITH

         /*-----------------------------------------------------------------*/
         /* Message headers                                                 */
         /*-----------------------------------------------------------------*/

         WITH OBJECT :Fields

            :Item( "urn:schemas:httpmail:importance" ):Value := ;
               ::nPriority

            :Item( "urn:schemas:mailheader:X-Priority" ):Value := ;
               ::nPriority - 1

            IF ::lReceipt

               :Item( ;
                  "urn:schemas:mailheader:return-receipt-to" ;
               ):Value := cFrom

               :Item( ;
                  "urn:schemas:mailheader:disposition-notification-to" ;
               ):Value := cFrom

            ENDIF

            :Update()

         END WITH

         :DSNOptions := CDO_DSN_DEFAULT

         /*-----------------------------------------------------------------*/
         /* Send                                                            */
         /*-----------------------------------------------------------------*/

         :Send()

      END WITH

      ::lSuccess := .T.

   CATCH oError

      MsgStop( ;
         "The email was not sent." + CRLF + CRLF + ;
         "Error      : " + cValToChar( oError:GenCode )    + CRLF + ;
         "SubCode   : " + cValToChar( oError:SubCode )     + CRLF + ;
         "OSCode    : " + cValToChar( oError:OsCode )      + CRLF + ;
         "SubSystem : " + cValToChar( oError:SubSystem )   + CRLF + ;
         "Description: " + oError:Description )

      oMessage := NIL

   END

RETURN NIL


*-----------------------------------------------------------------------------*
STATIC FUNCTION _CDOFormatAddress( aAddress )
*-----------------------------------------------------------------------------*

   LOCAL cName
   LOCAL cMail

   IF Empty( aAddress )
      RETURN ""
   ENDIF

   cName := aAddress[ 1 ]
   cMail := aAddress[ 2 ]

   IF Empty( cMail )
      RETURN cName
   ENDIF

RETURN cName + " <" + cMail + ">"


*-----------------------------------------------------------------------------*
STATIC FUNCTION _CDOBuildRecipients( aRecipients )
*-----------------------------------------------------------------------------*

   LOCAL aList := {}

   LOCAL i
   LOCAL cEntry

   FOR i := 1 TO Len( aRecipients )

      cEntry := ;
         _CDOFormatAddress( aRecipients[ i ] )

      IF ! Empty( cEntry )
         AAdd( aList, cEntry )
      ENDIF

   NEXT

RETURN hb_Implode( ";", aList )


*-----------------------------------------------------------------------------*
STATIC FUNCTION _CDOAddAttachments( oMessage, aFiles )
*-----------------------------------------------------------------------------*

   LOCAL i

   FOR i := 1 TO Len( aFiles )

      oMessage:AddAttachment( aFiles[ i ][ 1 ] )

   NEXT

RETURN NIL


*-----------------------------------------------------------------------------*
STATIC FUNCTION _CDOIsHtml( cText )
*-----------------------------------------------------------------------------*

   LOCAL cLower := Lower( AllTrim( cText ) )

   IF Empty( cText )
      RETURN .F.
   ENDIF

RETURN ;
      "<html" $ cLower .OR. ;
      "<body" $ cLower .OR. ;
      "<table" $ cLower .OR. ;
      "<div" $ cLower .OR. ;
      "<span" $ cLower .OR. ;
      "<p>" $ cLower
