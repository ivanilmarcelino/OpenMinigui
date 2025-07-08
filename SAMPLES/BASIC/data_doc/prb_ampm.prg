*:*****************************************************************************
*:
*: Procedure file: PRB_AMPM.PRG
*:
*:         System: Major FUNCTION Library
*:         Author: Phil Barnett
*:  Last modified: 07/19/1994     21:09
*:
*:  Procs & Fncts: AM_PM()
*:
*:
*:      Documented 07/19/1994 at 21:13
*:*****************************************************************************
#include "common.ch"
*!*****************************************************************************
*!
*!       FUNCTION: AM_PM()
*!
*!*****************************************************************************
FUNCTION AM_PM( _TIME, seconds )

  LOCAL HOURS
  LOCAL AMPMFLAG := ' am'

  DEFAULT seconds  TO .f.
  DEFAULT _TIME TO time()

  HOURS := val( left( _TIME, 2 ) )

  IF HOURS >= 12
    AMPMFLAG := ' pm'
    HOURS -= 12
  ENDIF

  IF HOURS = 0
    HOURS := 12
  ENDIF

RETURN str( HOURS, 2 ) + substr( _TIME, 3, iif( seconds, 6, 3 ) ) + AMPMFLAG

// EOF: PRB_AMPM.PRG
