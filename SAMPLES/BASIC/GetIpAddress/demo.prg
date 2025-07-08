/*
 * Copyright 2022 Pierpaolo Martinello
*/

#include "minigui.ch"

FUNCTION Main()

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 250 ;
         HEIGHT 210 ;
         MAIN ;
         TITLE 'Get IP Address' ;
         NOMAXIMIZE ;
         NOMINIMIZE

      DEFINE BUTTONEX BUTTON_1
         ROW 20
         COL 60
         CAPTION "Ip From Inet"
         ACTION IpFromInet()
         WIDTH 120
         HEIGHT 30
      END BUTTONEX

      DEFINE BUTTONEX BUTTON_2
         ROW 70
         COL 60
         CAPTION "Ip From Route"
         ACTION IpFromRoute()
         WIDTH 120
         HEIGHT 30
      END BUTTONEX

      DEFINE BUTTONEX BUTTON_3
         ROW 120
         COL 60
         CAPTION "Ip From Wmi"
         ACTION IpFromWmi()
         WIDTH 120
         HEIGHT 30
      END BUTTONEX

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION IpFromRoute () // work from Xp and Later
*-----------------------------------------------------------------------------*
   LOCAL aBase, C1, s, cx, rtv := {}

   Abase := scan( "Route print -4  0.0.0.0", .F. )

   C1 := hb_ATokens( Abase, CRLF )
   FOR EACH s IN c1
      IF hb_AtI( "0.0.0.0", s ) > 0
         cx := hb_ATokens( s, "   " )
         AEval( cx, {| x | iif( Val( x ) > 0, AAdd( rtv, LTrim( x ) ), ) } )
         EXIT
      ENDIF
   NEXT

   MsgInfo ( "Ip   = " + Chr( 9 ) + rtv[ 2 ] + CRLF + "Gw = " + Chr( 9 ) + rtv[ 1 ] ;
      + CRLF + "Metric =" + Chr( 9 ) + rtv[ 3 ], "Ip From Route" )

RETURN rtv // this is the ip of the main network adapter!
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION IpFromInet () // work from Xp and Later
*-----------------------------------------------------------------------------*
   LOCAL ip, /*s:='',*/ s1 := ''
   /*
   discovery only Ip
   Ip := INETGETHOSTS( NETNAME() )
   aeval(ip,{|x,y|s += "Ip "+hb_ntos(y)+" = "+CHR(9)+x+CRLF})
   */

   ip := hb_inetIFInfo() // all information but which network adapter is the main one?

   AEval( ip, {| x, y | s1 += "Ip " + hb_ntos( y ) + " = " + PadR( x[ 4 ], 15 ) ;
      + " Mask =" + PadR( x[ 5 ], 15 ) + PadR( if( ISNIL( x[ 8 ] ), Space( 8 ) + 'No MacAddress', " Mac :" + x[ 8 ] ), 23 ) + CRLF } )

   s1 += CRLF + "All information but which network adapter is the used as main one?"
   s1 += CRLF + CRLF + Space( 8 ) + 'To find it use the [ Ip FromRoute ] or [ Ip From Wmi ] Button!'

   MsgBox( s1, "Ip From Inet" )

RETURN ip
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION Scan( cCommand )
*-----------------------------------------------------------------------------*
   LOCAL hProcess, cResult
   LOCAL hStdOut, hStderr, nState, nBytes
   LOCAL cBuff

   cBuff := Space( 2048 )

   hProcess := hb_processOpen( cCommand, NIL, @hStdOut, @hStdErr, .T. )

   IF hProcess != -1

      nState := hb_processValue( hProcess, .T. )

      WHILE nState <> -1

         nBytes := FRead( hStdOut, @cBuff, 2048 )

         IF nBytes == 0
            EXIT
         ENDIF

         nState := hb_processValue( hProcess, .T. )

      END

      cBuff := RTrim( cBuff )
      cResult := REPLRIGHT( cBuff, " ", Chr( 10 ) )

      hb_processClose( hProcess )

   ENDIF

RETURN cResult
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION IpFromWmi ()
*-----------------------------------------------------------------------------*
   LOCAL oWmi, oNetA, S1 := "", ip, gw

   oWmi := WmiService()

   FOR EACH oNetA IN oWmi:ExecQuery( "SELECT * FROM Win32_NetworkAdapterConfiguration WHERE IPEnabled = True" )

      IF ! ISNIL( oNetA:DefaultIPGateway )
         ip := oneta:IPAddress( 0 )
         gw := oNeta:MACAddress

         S1 += "Ip    =" + Chr( 9 ) + Ip + CRLF ;
            + "Gw  =" + Chr( 9 ) + oneta:DefaultIPGateway[ 1 ] + CRLF ;
            + "Mac = " + Chr( 9 ) + gw
      ENDIF
   NEXT

   MsgInfo( S1, "Ip From Wmi" )

RETURN { ip, gw }
/*
*/
*-----------------------------------------------------------------------------*
STATIC FUNCTION WMIService()
*-----------------------------------------------------------------------------*
   STATIC oWMI

   LOCAL oLocator

   IF oWMI == NIL

      oLocator := CreateObject( "wbemScripting.SwbemLocator" )
      oWMI := oLocator:ConnectServer()

   ENDIF

RETURN oWMI
