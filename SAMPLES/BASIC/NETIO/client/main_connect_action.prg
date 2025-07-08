#include "hmg.ch"

DECLARE WINDOW MAIN

MEMVAR cMainTitle

FUNCTION main_connect_action

   LOCAL cNetServer := '127.0.0.1' // Server Address
   LOCAL nNetPort := 50000 // Server Port
   LOCAL cNetPass := 'secret' // Server Password
   LOCAL lConnect

   lConnect := netio_connect( cNetServer, nNetPort,, cNetPass, 9 )
   IF .NOT. lConnect
      MSGSTOP( "Can't Connect To Server!" )
      RETURN NIL
   ELSE
      MAIN.Query_Server.Enabled := .T.
      MAIN.Disconnect.Enabled := .T.
      MAIN.Connect.Enabled := .F.
      MAIN.Query_String.Enabled := .T.
      SetProperty( 'Main', 'Title', cMainTitle + ' - Connected!' )
      MAIN.Query_String.SetFocus
   ENDIF

RETURN NIL
