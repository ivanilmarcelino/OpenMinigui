/*
 * MiniGUI Basic NetIO Client Sample.
 * Roberto Lopez <mail.box.hmg@gmail.com>
*/

#include <hmg.ch>

FUNCTION Main

   MEMVAR aRecordSet
   MEMVAR cMainTitle

   PUBLIC aRecordSet
   PUBLIC cMainTitle

   LOAD WINDOW Main

   IF ValType( cMainTitle ) != "C"
      cMainTitle := Main.TITLE
   ENDIF

   Main.Query_Server.Enabled := .F.
   Main.Disconnect.Enabled := .F.
   Main.Query_String.Enabled := .F.

   Main.CENTER

   Main.ACTIVATE

RETURN NIL
