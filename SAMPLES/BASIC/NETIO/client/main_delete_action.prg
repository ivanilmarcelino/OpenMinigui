#include "hmg.ch"

DECLARE WINDOW MAIN

FUNCTION main_delete_action

   LOCAL I, nRecNo

   IF MAIN.Query_Server.Enabled == .T.

      IF .NOT. MSGYESNO( 'Are You Sure?' )
         RETURN NIL
      ENDIF

      I := MAIN.Grid_1.VALUE

      IF I == 0
         MsgStop( 'You must select a row!' )
         RETURN NIL
      ENDIF

      nRecNo := Val( MAIN.Grid_1.Cell( I, 1 ) )

      IF netio_funcexec( "query_002", nRecNo )
         MsgInfo( 'Record Deleted Successfully!' )
         MAIN.Query_String.VALUE := ''
         main_query_server_action()
      ELSE
         MsgStop( 'Error Deleting Record!' )
      ENDIF

   ENDIF

RETURN NIL
