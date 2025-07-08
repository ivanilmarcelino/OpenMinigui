/*

   Multiple CSBox ( Combined Search Box ) with tabbing test

*/

#include "minigui.ch"
#include "combosearchbox.ch"

SET PROCEDURE TO combosearchbox

PROCEDURE Main()

   LOCAL aCountries := hb_ATokens( MemoRead( "Countries.lst" ), CRLF )
   LOCAL aLargCits := hb_ATokens( MemoRead( "LargCits.lst" ), CRLF )
   LOCAL aNationals := hb_ATokens( MemoRead( "Nationality.lst" ), CRLF )

   ASort( aCountries )
   ASort( aLargCits )
   ASort( aNationals )

   SET NAVIGATION EXTENDED
   SET AUTOADJUST ON

   DEFINE WINDOW frmMCSBTest ;
         AT 0, 0 ;
         WIDTH 550 ;
         HEIGHT 300 ;
         TITLE 'Multiple CSBox ( Combined Search Box ) Sample' ;
         MAIN

      ON KEY ESCAPE ACTION frmMCSBTest.RELEASE

      DEFINE LABEL lblCountries
         ROW 27
         COL 10
         WIDTH 70
         VALUE "Country :"
         RIGHTALIGN .T.
      END LABEL

      DEFINE LABEL lblCities
         ROW 57
         COL 10
         WIDTH 70
         VALUE "City :"
         RIGHTALIGN .T.
      END LABEL

      DEFINE COMBOSEARCHBOX csbxCountries
         ROW        25
         COL        90
         WIDTH      150
         ITEMS      aCountries
         ON ENTER   iif( !Empty( this.Value ), MsgBox( this.Value ), NIL )
      END COMBOSEARCHBOX

      DEFINE COMBOSEARCHBOX csbxLargCits
         ROW        55
         COL        90
         WIDTH      150
         ITEMS      aLargCits
      END COMBOSEARCHBOX

      DEFINE TAB tabMCSBox ;
            AT 20, 250 ;
            WIDTH 280 ;
            HEIGHT 240

         DEFINE PAGE "Blank Page"
            @ 90, 5 LABEL lblBlank WIDTH 270 VALUE "This page left intentionally blank." CENTERALIGN
         END PAGE

         DEFINE PAGE "Tabbed CSBox"

            DEFINE LABEL lblNations
               ROW 37
               COL 10
               WIDTH 70
               VALUE "Nationality :"
               RIGHTALIGN .T.
            END LABEL

            DEFINE COMBOSEARCHBOX csbxNations
               ROW        35
               COL        90
               WIDTH      150
               ITEMS      aNationals
            END COMBOSEARCHBOX

         END PAGE

      END TAB

   END WINDOW // frmMCSBTest

   frmMCSBTest.CENTER

   frmMCSBTest.ACTIVATE

RETURN // Main()
