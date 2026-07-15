/*
 * MINIGUI - Harbour Win32 GUI library source code
 * 
 * Copyright 2022 Grigory Filatov <gfilatov@gmail.com>
 */

#ifndef _MAIL_CH
#define _MAIL_CH

#xcommand DEFINE MAIL [ OBJ ] <oMail> ;
             [ SERVER <cServer> ] ;
             [ PORT <nPort> ] ;
             [ USERAUTH <cUser> ] ;
             [ PASSAUTH <cPass> ] ;
             [ SUBJECT <cSubject> ] ;
             [ TEXT <cText> ] ;
             [ PRIORITY <nPriority> ] ;
             [ <rec: RECEIPT> ] ;
             [ FILES <cFileName1> ;
                 [,<cFileNameN> ] ] ;
             [ FROM <cOrigin> [,<cOriginAddress>] ] ;
             [ TO <cTarget1> [,<cTargetAddress1>] ;
                [,<cTargetN> [,<cTargetAddressN>] ] ] ;
       => ;
          [ <oMail> := ] TCDOMail():New( <cServer>, <nPort>, <cUser>, <cPass>,;
             <cSubject>, <cText>, <nPriority>, <.rec.>,;
             [ \{<cOrigin>, <cOriginAddress>\} ],;
             \{ [ \{<cTarget1>,<cTargetAddress1>\} ] ;
                [,\{<cTargetN>,<cTargetAddressN>\} ] \},;
             \{ [ \{<cFileName1>\} ] ;
                [,\{<cFileNameN>\} ] \} )

#xcommand ACTIVATE MAIL <oMail> => <oMail>:Activate()

#xcommand SEND MAIL <oMail> => <oMail>:Activate()

#endif
