#include "hmg.ch"

DECLARE WINDOW MAIN

MEMVAR cMainTitle

FUNCTION main_disconnect_action

   LOCAL cNetServer := '127.0.0.1'
   LOCAL nNetPort := 50000

   netio_disconnect( cNetServer, nNetPort )

   // Delete All Items
   MAIN.Grid_1.ItemCount := 0

   MAIN.Query_String.VALUE := ''
   MAIN.Query_Server.Enabled := .F.
   MAIN.Query_String.Enabled := .F.
   MAIN.Disconnect.Enabled := .F.
   MAIN.Connect.Enabled := .T.

   SetProperty( 'Main', 'Title', cMainTitle + ' - Disconnected!' )

RETURN NIL
