*:*****************************************************************************
*:
*: Procedure file: PRB_FULL.PRG
*:
*:         System: Major FUNCTION Library
*:         Author: Phil Barnett
*:  Last modified: 07/19/1994     21:09
*:
*:  Procs & Fncts: FULLDATE()
*:
*:
*:      Documented 07/19/94 at 21:13
*:*****************************************************************************
#include "common.ch"
*!*****************************************************************************
*!
*!       FUNCTION: FULLDATE()
*!
*!*****************************************************************************
FUNCTION FULLDATE( ANYDATE )

   DEFAULT ANYDATE TO date()

RETURN cmonth( ANYDATE ) + ' ' + hb_ntos( day( ANYDATE ), 2 ) + ', ' + str( year( ANYDATE ), 4 )

// EOF: PRB_FULL.PRG
