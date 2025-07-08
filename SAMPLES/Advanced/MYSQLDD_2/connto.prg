
FUNCTION ConnectTo( n )

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
      
RETURN !( rddInfo( RDDI_CONNECT, { "MYSQL", cServer, cUser, cPassword, cDBName, nPort } ) == 0 )
