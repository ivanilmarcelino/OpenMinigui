/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2016, Verchenko Andrey <verchenkoag@gmail.com>
 *
*/

#include "minigui.ch"

#define MSGINFO     1005        

#define COPYRIGHT  "(c) Copyright by Andrey Verchenko, 2018. All Right Reserved. Dmitrov, Russia."
#define PRG_NAME   "SetArrayTo Demo to Export: XLS/XLM/DOC/DBF/CSV"
#define PRG_VERS   "Ver 9.88"
#define PRG_INFO1  "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
#define PRG_INFO2  "Tips and tricks programmers from our forum http://clipper.borda.ru"
#define PRG_INFO3  "Sergej Kiselev, Sidorov Aleksandr, Dima, Pavel Tsarenko, Igor Nazarov"

///////////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgAbout()
   RETURN MsgInfo( PRG_NAME + " - " + PRG_VERS  + CRLF + CRLF +  ;
                   PadC( COPYRIGHT, 80 ) + CRLF + CRLF + ;
                   PadR( PRG_INFO1, 80 ) + CRLF + ;
                   PadR( PRG_INFO2, 80 ) + CRLF + ;
                   PRG_INFO3 + CRLF + CRLF + ;
                   hb_compiler() + CRLF + ;
                   Version() + CRLF + ;
                   MiniGuiVersion() + CRLF + CRLF + ;
                   PadC( "This program is Freeware!", 70 ) + CRLF + ;
                   PadC( "Copying is allowed!", 70 ), "About", MSGINFO, .F. )

///////////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgAboutDim(nVal)
   LOCAL aDim := { COPYRIGHT, PRG_INFO1, PRG_INFO2, PRG_INFO3, PRG_VERS }
   DEFAULT nVal := 1
   RETURN aDim[nVal] 
