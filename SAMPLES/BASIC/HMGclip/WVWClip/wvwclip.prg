/****

      wvwclip.prg
      An example of integrating GTWVW GUI controls in Harbour.

      This work is based on cbtest6.prg
      Copyright 2005 Budyanto Dj. <budyanto@centrin.net.id>

      Copyright 2016 Ashfaq Sial

      To Build:
      hbmk2 wvwclip.hbp

*/

#include "inkey.ch"
#include "wvwstd.ch"

ANNOUNCE HB_NOSTARTUPWINDOW

FUNCTION Main()

   LOCAL nOpt

   SET CENTURY ON
   SET SCOREBOARD OFF
   SET CONFIRM ON
   SET CURSOR OFF

   SET EVENTMASK TO INKEY_ALL
altd()
   SetMode( 25, 80 )
   SetCancel( .F. )

   WVWSetUp('Visual Clipper via GTWVW', 4 )

   // Couldn't think of a better way.
   MainMenu()

   // Process menu events.
   DO WHILE .T.

      MENU TO nOpt

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      DO CASE

      CASE nOpt == 1101
         DO empdet

      CASE nOpt == 1102
         RadioButton()

      CASE nOpt == 1103
         EXIT

      ENDCASE

   ENDDO

   RETURN NIL

// EOF: WVWCLIP.PRG
