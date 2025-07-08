/*
 * HMG - Harbour Win32 GUI library
*/

*************************************************************************************
* Attention: to detect processes 32 and 64 bits you should compiling with HMG-64 bits
*************************************************************************************

#include "common.ch"

#define INFINITE        0xFFFFFFFF

// Waits until the specified object is in the signaled state or the time-out interval elapses
FUNCTION WaitForSingleObject( hProcess, nWait )

   DEFAULT nWait TO INFINITE

RETURN wapi_WaitForSingleObject( win_N2P( hProcess ), nWait )
