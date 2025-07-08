/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */
#define _HMG_OUTLOG

#include "minigui.ch"
#include "dbinfo.ch"

REQUEST DBFCDX

FUNCTION Main()

   LOCAL cCust, cStat
   LOCAL cFirst, cName

   SET WINDOW MAIN OFF

   USE CUSTOMER NEW SHARED Alias ( cCust := cGetNewAlias( "CUST" ) ) VIA "DBFCDX"
   USE STATES NEW SHARED Alias ( cStat := cGetNewAlias( "STAT" ) ) VIA "DBFCDX"

   // save values
   cFirst := ( cCust )->FIRST
   cName := ( cStat )->NAME

   // Start testing

   TRY
      IF ( cCust )->( RLock() ) .AND. ( cStat )->( RLock() )

         ? "Original Values:", ( cCust )->FIRST, ( cStat )->NAME
         ( cCust )->FIRST := NTOCDOW( hb_RandomInt( 1, 7 ) )
         ( cStat )->NAME := NTOCMONTH( hb_RandomInt( 1, 12 ) )
         ? "Modified Values:", ( cCust )->FIRST, ( cStat )->NAME

         IF HMG_Alert( "CHOOSE", { "COMMIT", "ROLLBACK" }, "OPTION" ) == 1
            COMMIT
            UNLOCK ALL
         ELSE
            dbRollBackAll()
            UNLOCK ALL
         ENDIF
      ELSE
         UNLOCK ALL
      ENDIF

   CATCH
      dbRollBackAll()
      UNLOCK ALL
   END

   ? "Final Values:", ( cCust )->FIRST, ( cStat )->NAME

   // Restore Values

   IF ( cCust )->( RLock() ) .AND. ( cStat )->( RLock() )
      ( cCust )->FIRST := cFirst
      ( cStat )->NAME := cName
      COMMIT
      UNLOCK ALL
   ENDIF

   CLOSE DATA

RETURN NIL

//----------------------------------------------------------------------------//

FUNCTION DBROLLBACK()

   IF dbRecordInfo( DBRI_UPDATED )
      dbInfo( DBI_ROLLBACK )
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//

FUNCTION DBROLLBACKALL()

   LOCAL n, cAlias

   FOR n := 1 TO 250
      IF ! Empty( cAlias := Alias( n ) ) .AND. ( cAlias )->( rddName() ) $ "DBFCDX,DBFNTX"
         ( cAlias )->( dbRollBack() )
      ENDIF
   NEXT

RETURN NIL

//----------------------------------------------------------------------------//

FUNCTION cGetNewAlias( cAlias ) // returns a new alias name for an alias

   LOCAL cNewAlias, nArea := 1

   IF Select( cAlias ) != 0
      WHILE Select( cNewAlias := ( cAlias + ;
            StrZero( nArea++, 3 ) ) ) != 0
      END
   ELSE
      cNewAlias = cAlias
   ENDIF

RETURN cNewAlias
