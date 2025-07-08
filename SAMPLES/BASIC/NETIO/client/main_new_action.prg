#include "hmg.ch"

DECLARE WINDOW MAIN

FUNCTION main_new_action

   LOCAL TITLE, aLabels, aInitValues, aFormats, aValues

   IF MAIN.Query_Server.Enabled == .T.

      TITLE := 'New Record'

      aLabels := { 'First:', 'Last:', 'Street:', 'City:', 'State:', 'Zip:', 'Hire Date', 'Married', 'Age', 'Salary' }
      aInitValues := { '', '', '', '', '', '', Date(), .F., 0, 0 }
      aFormats := { 32, 32, 32, 32, 32, 32, NIL, NIL, '99', '999999' }

      aValues := InputWindow ( TITLE, aLabels, aInitValues, aFormats )

      IF aValues[ 1 ] == NIL

         MsgInfo( 'Canceled', 'New Record' )

      ELSE

         netio_funcexec( "query_003", aValues )

         MsgInfo( 'Operation Completed!' )

         MAIN.Query_String.VALUE := aValues[ 2 ]

         main_query_server_action()

      ENDIF

   ENDIF

RETURN NIL
