#include "hmg.ch"

DECLARE WINDOW MAIN

FUNCTION main_modify_action

   LOCAL I, nRecNo
   LOCAL cFirst, cLast, cStreet, cCity, cState, cZip, dHireDate, lMarried, nAge, nSalary
   LOCAL TITLE, aLabels, aInitValues, aFormats, aValues

   IF MAIN.Query_Server.Enabled == .T.

      I := MAIN.Grid_1.VALUE

      IF I == 0
         MsgStop( 'You must select a row!' )
         RETURN NIL
      ENDIF

      nRecNo := Val( MAIN.Grid_1.Cell( I, 1 ) )

      cFirst := MAIN.Grid_1.Cell( I, 3 )
      cLast := MAIN.Grid_1.Cell( I, 2 )
      cStreet := MAIN.Grid_1.Cell( I, 4 )
      cCity := MAIN.Grid_1.Cell( I, 5 )
      cState := MAIN.Grid_1.Cell( I, 6 )
      cZip := MAIN.Grid_1.Cell( I, 7 )
      dHireDate := CToD( MAIN.Grid_1.Cell( I, 8 ) )
      lMarried := iif( MAIN.Grid_1.Cell( I, 9 ) = '.T.', .T., .F. )
      nAge := Val( MAIN.Grid_1.Cell( I, 10 ) )
      nSalary := Val( MAIN.Grid_1.Cell( I, 11 ) )

      TITLE := 'Modify Record'

      aLabels := { 'First:', 'Last:', 'Street:', 'City:', 'State:', 'Zip:', 'Hire Date', 'Married', 'Age', 'Salary' }
      aInitValues := { cFirst, cLast, cStreet, cCity, cState, cZip, dHireDate, lMarried, nAge, nSalary }
      aFormats := { 32, 32, 32, 32, 32, 32, NIL, NIL, '99', '999999' }

      aValues := InputWindow ( TITLE, aLabels, aInitValues, aFormats )

      IF aValues[ 1 ] == NIL

         MsgInfo( 'Canceled', 'New Record' )

      ELSE

         netio_funcexec( "query_004", nRecNo, aValues )

         main_query_server_action()

         MsgInfo( 'Operation Completed!' )

      ENDIF

   ENDIF

RETURN NIL
