/*
 * MiniGUI Basic NetIO Server Sample.
 * Roberto Lopez <mail.box.hmg@gmail.com>
*/

#include <hmg.ch>

FUNCTION Main

   LOCAL nPort := 50000
   LOCAL cIfAddr := '0.0.0.0'
   LOCAL cRootDir := '.'
   LOCAL lRPC := .T.
   LOCAL cPasswd := 'secret'
   LOCAL nCompressionLevel := 9
   LOCAL nStrategy := NIL
   LOCAL pSockSrv := NIL

   // Start Server

   pSockSrv := NETIO_MTSERVER( nPort, cIfAddr, cRootDir, lRPC, cPasswd, nCompressionLevel, nStrategy )
   IF Empty( pSockSrv )
      MSGSTOP( "Can't Start Server!" )
      RETURN NIL
   ENDIF

   LOAD WINDOW Main

   Main.ACTIVATE

RETURN NIL


FUNCTION ServerStop( pSockSrv )

   // Stop Server

   netio_serverstop( pSockSrv, .T. )

RETURN NIL
