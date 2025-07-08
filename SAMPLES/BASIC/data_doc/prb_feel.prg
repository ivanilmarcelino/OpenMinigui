*:*****************************************************************************
*:
*: Procedure file: PRB_FEEL.PRG
*:
*:         System: Major FUNCTION Library
*:         Author: Phil Barnett
*:  Last modified: 07/19/1994     21:09
*:
*:  Procs & Fncts: FEEL()
*:
*:
*:      Documented 07/19/1994 at 21:13
*:*****************************************************************************
*!*****************************************************************************
*!
*!       FUNCTION: FEEL()
*!
*!*****************************************************************************
FUNCTION  FEEL( FILE_NAME, F_DATE, F_TIME )

  LOCAL DIR_ARRAY := directory( FILE_NAME )
  LOCAL FOUND_IT := !empty( DIR_ARRAY )

  IF FOUND_IT
    F_DATE := DIR_ARRAY[ 1, 3 ]
    F_TIME := DIR_ARRAY[ 1, 4 ]
  ENDIF

RETURN FOUND_IT

// EOF: PRB_FEEL.PRG
