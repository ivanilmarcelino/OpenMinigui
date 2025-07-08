
FUNCTION ConnectTo( oServer, n )

   LOCAL c 
   LOCAL hIni      
   LOCAL cServer, cUser, cPassword, nPort, cDBName

   c := "mysql"

   IF n != NIL 
      c += hb_ntos( n )
   ENDIF

   hIni      := hb_ReadIni( "connect.ini" )
   cServer   := hIni[ c ]["host"]
   cUser     := hIni[ c ]["user"]
   cPassword := hIni[ c ]["psw"]
   nPort     := Val( hIni[ c ]["port"] )
   cDBName   := hIni[ c ]["dbname"]

   oServer := TMySQLServer():New( cServer, cUser, cPassword, nPort )

   IF oServer:NetErr()
      MsgAlert( oServer:Error(), "MySQL Error" )
      RETURN .F.
   ENDIF

   oServer:SelectDb( cDBName )

   IF oServer:lError
      MsgAlert( oServer:Error(), "MySQL Error" )
      RETURN .F.
   ENDIF

RETURN .T.
