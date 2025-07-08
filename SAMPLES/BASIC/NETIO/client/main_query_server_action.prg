
#include "hmg.ch"

DECLARE WINDOW MAIN

MEMVAR aRecordSet

FUNCTION main_query_server_action

   aRecordset := netio_funcexec( "query_001", MAIN.Query_String.Value )

   MAIN.Grid_1.ItemCount := Len( aRecordset )

   IF Len( aRecordset ) == 0
      MsgInfo( 'No Records Found!' )
      MAIN.Query_String.VALUE := ''
   ENDIF

RETURN NIL
