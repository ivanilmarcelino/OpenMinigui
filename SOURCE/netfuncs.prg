/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 */

#include "minigui.ch"
#include "dbinfo.ch"
#include "error.ch"

// Constants for network locking operations
#define NET_RECLOCK  1  // Record Lock
#define NET_FILELOCK 2  // File Lock
#define NET_APPEND   3  // Append Blank

// Static variables (module-level scope)
STATIC s_nNetDelay := 1 // Default network delay in seconds
STATIC s_lNetOk := .F.  // Flag indicating success of the last network operation

// -----------------------------------------------------------------------------
// FUNCTION: NetLock
// PURPOSE:  Attempts to acquire a network lock (record or file).
// PARAMETERS:
//    nType:         Type of lock (NET_RECLOCK, NET_FILELOCK, NET_APPEND)
//    lReleaseLocks: .T. to release locks, .F. to acquire (relevant for NET_APPEND)
//    nSeconds:      Maximum time to wait for the lock (in seconds)
// RETURNS:    .T. if the lock was acquired successfully, .F. otherwise.
// -----------------------------------------------------------------------------
STATIC FUNCTION NetLock( nType, lReleaseLocks, nSeconds )

   LOCAL lSuccess := .F.
   LOCAL bOperation // Codeblock to execute for the lock operation
   LOCAL xIdentifier // Identifier for the lock (e.g., record number)
   LOCAL nStartTime // Time when the locking attempt started

   DEFAULT lReleaseLocks TO .F.
   DEFAULT nSeconds TO s_nNetDelay

   nStartTime := Seconds() // Record the start time

   DO CASE
   CASE nType == NET_RECLOCK
      xIdentifier := iif( lReleaseLocks, NIL, RecNo() )
      bOperation := {| x | dbRLock( x ) }
   CASE nType == NET_FILELOCK
      bOperation := {|| FLock() }
   CASE nType == NET_APPEND
      xIdentifier := lReleaseLocks
      bOperation := {| x | dbAppend( x ), ! NetErr() }
   OTHERWISE
      // Invalid lock type.
      RETURN .F.
   ENDCASE

   s_lNetOk := .F.

   WHILE ( Seconds() - nStartTime ) < nSeconds // Use Seconds() for more accurate timing

      IF Eval( bOperation, xIdentifier )
         lSuccess := .T.
         s_lNetOk := .T.
         EXIT
      ELSE
         HMG_SysWait( .25 )
      ENDIF

   ENDDO

RETURN lSuccess

// -----------------------------------------------------------------------------
// FUNCTION: NetDelete
// PURPOSE:  Deletes the current record with network locking.
// RETURNS:    .T. if the delete was successful, .F. otherwise.
// -----------------------------------------------------------------------------
FUNCTION NetDelete()

   s_lNetOk := .F.

   IF NetLock( NET_RECLOCK )
      dbDelete()
      s_lNetOk := .T.
   ENDIF

   IF ! NetErr()
      dbSkip( 0 )
      dbCommit()
   ELSE
      s_lNetOk := .F.
      HMG_Alert( "Failed to DELETE Record -> " + hb_ntos( RecNo() ) )
   ENDIF

RETURN s_lNetOk

// -----------------------------------------------------------------------------
// FUNCTION: NetRecall
// PURPOSE:  Recalls the current record with network locking.
// RETURNS:    .T. if the recall was successful, .F. otherwise.
// -----------------------------------------------------------------------------
FUNCTION NetRecall()

   s_lNetOk := .F.

   IF NetLock( NET_RECLOCK )
      dbRecall()
      s_lNetOk := .T.
   ENDIF

   IF ! NetErr()
      dbSkip( 0 )
      dbCommit()
   ELSE
      s_lNetOk := .F.
      HMG_Alert( "Failed to RECALL Record -> " + hb_ntos( RecNo() ) )
   ENDIF

RETURN s_lNetOk

// -----------------------------------------------------------------------------
// FUNCTION: NetRecLock
// PURPOSE:  Attempts to acquire a record lock.
// PARAMETERS:
//    nSeconds: Maximum time to wait for the lock (in seconds).     DEFAULTs to s_nNetDelay.
// RETURNS:    .T. if the lock was acquired successfully, .F. otherwise.
// -----------------------------------------------------------------------------
FUNCTION NetRecLock( nSeconds )

   DEFAULT nSeconds TO s_nNetDelay

   s_lNetOk := .F.

   IF NetLock( NET_RECLOCK, , nSeconds )
      s_lNetOk := .T.
   ENDIF

RETURN s_lNetOk

// -----------------------------------------------------------------------------
// FUNCTION: NetFileLock
// PURPOSE:  Attempts to acquire a file lock.
// PARAMETERS:
//    nSeconds: Maximum time to wait for the lock (in seconds).     DEFAULTs to s_nNetDelay.
// RETURNS:    .T. if the lock was acquired successfully, .F. otherwise.
// -----------------------------------------------------------------------------
FUNCTION NetFileLock( nSeconds )

   DEFAULT nSeconds TO s_nNetDelay

   s_lNetOk := .F.

   IF NetLock( NET_FILELOCK, , nSeconds )
      s_lNetOk := .T.
   ENDIF

RETURN s_lNetOk

// -----------------------------------------------------------------------------
// FUNCTION: NetAppend
// PURPOSE:  Appends a blank record with network locking.
// PARAMETERS:
//    nSeconds:      Maximum time to wait for the lock (in seconds).     DEFAULTs to s_nNetDelay.
//    lReleaseLocks: .T. to release locks after appending, .F. to keep them.    DEFAULTs to .T.
// RETURNS:    .T. if the append was successful, .F. otherwise.
// -----------------------------------------------------------------------------
FUNCTION NetAppend( nSeconds, lReleaseLocks )

   LOCAL cOrd

   DEFAULT lReleaseLocks TO .T.
   DEFAULT nSeconds TO s_nNetDelay

   s_lNetOk := .F.

   cOrd := ordSetFocus( 0 ) // set order to 0 to append

   IF NetLock( NET_APPEND, lReleaseLocks, nSeconds )
      s_lNetOk := .T.
   ENDIF

   IF ! Empty( cOrd )
      ordSetFocus( cOrd )
   ENDIF

RETURN s_lNetOk

// -----------------------------------------------------------------------------
// FUNCTION: IsLocked
// PURPOSE:  Checks if a record is currently locked.
// PARAMETERS:
//    nRecId:  Record number to check.     DEFAULTs to the current record.
// RETURNS:    .T. if the record is locked, .F. otherwise.
// -----------------------------------------------------------------------------
FUNCTION IsLocked( nRecId )

   DEFAULT nRecID TO RecNo()

RETURN AScan( dbRLockList(), {| n | n == nRecID } ) > 0

// -----------------------------------------------------------------------------
// FUNCTION: NetError
// PURPOSE:  Returns the status of the last network operation.
// RETURNS:    .T. if the last operation failed, .F. otherwise.
// -----------------------------------------------------------------------------
FUNCTION NetError()

RETURN ! s_lNetOk

// -----------------------------------------------------------------------------
// FUNCTION: SetNetDelay
// PURPOSE:  Sets the    DEFAULT network delay.
// PARAMETERS:
//    nSecs: New network delay in seconds.  If NIL, the delay is not changed.
// RETURNS:    The previous network delay.
// -----------------------------------------------------------------------------
FUNCTION SetNetDelay( nSecs )

   LOCAL nTemp := s_nNetDelay

   IF nSecs != NIL
      s_nNetDelay := nSecs
   ENDIF

RETURN nTemp
