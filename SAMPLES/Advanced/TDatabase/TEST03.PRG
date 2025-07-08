/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Program : Test03.prg
 * Purpose : TData class test showing how to create an index of a database with a progress meter.
*/

#include "minigui.ch"

FUNCTION MAIN

   LOCAL oDB

   SET WINDOW MAIN OFF

   // setup test.dbf
   dbCreate( 'test', { { 'FLD1', "+", 8, 0 } } )
   USE test
   WHILE LastRec() < 100000
      APPEND BLANK
   END
   USE

   oDB := tdata():new(, "test" )
   IF oDB:use()
      oDB:createIndex( "test",, "fld1",,, .T., 5 )
   ENDIF
   oDB:close()

RETURN NIL
