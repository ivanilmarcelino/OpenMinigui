/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 */

#include "minigui.ch"
#include "dbinfo.ch"
#include "error.ch"

// Constants for network locking operations
#define NET_LOCK_RECORD  1
#define NET_LOCK_FILE    2
#define NET_LOCK_APPEND  3

STATIC nNetDelay := 1  // Default network delay in seconds
STATIC lNetOk := .F.   // Flag indicating success of the last network operation

/*
 * STATIC FUNCTION NetLock( nLockingType, lReleaseLocks, nTimeout )
 *
 * Attempts to acquire or release a network lock (record, file, or append).
 *
 * Parameters:
 *   nLockingType (NUMERIC):  Specifies the type of lock to acquire or release.
 *                           Must be one of the NET_LOCK_* constants (NET_LOCK_RECORD, NET_LOCK_FILE, NET_LOCK_APPEND).
 *   lReleaseLocks (LOGICAL): Optional.  Defaults to .F..
 *                           If .T., the function attempts to *release* the lock.
 *                           If .F., the function attempts to *acquire* the lock.
 *                           This parameter is only relevant for NET_LOCK_APPEND.
 *   nTimeout    (NUMERIC):  Optional.  Defaults to nNetDelay.
 *                           The maximum time (in seconds) to wait for the lock to be acquired.
 *
 * Returns:
 *   LOGICAL: .T. if the lock was successfully acquired (or released), .F. otherwise.
 *
 * Purpose:
 *   This function provides a mechanism for implementing network locking in a multi-user environment.
 *   It prevents multiple users from simultaneously modifying the same data, ensuring data integrity.
 *   The function uses a loop to repeatedly attempt to acquire the lock until either the lock is acquired
 *   or the timeout period expires.  The type of lock (record, file, or append) is determined by the nLockingType parameter.
 *   For NET_LOCK_APPEND, the lReleaseLocks parameter controls whether the lock is acquired or released.
 *
 * Notes:
 *   The function uses the dbRLock(), FLock(), and dbAppend() functions to perform the actual locking operations.
 *   The HMG_SysWait() function is used to pause execution between lock attempts, preventing excessive CPU usage.
 *   The lNetOk static variable is updated to reflect the success or failure of the lock operation.
 *   The function uses Seconds() for timing, which is more accurate than using seconds.
 */
STATIC FUNCTION NetLock( nLockingType, lReleaseLocks, nTimeout )

   LOCAL lSuccess := .F.
   LOCAL bLockOperation  // Codeblock to execute for the lock operation
   LOCAL xLockIdentifier // Identifier for the lock (e.g., record number)
   LOCAL nStartTime   // Time when the locking attempt started

   DEFAULT lReleaseLocks TO .F.
   DEFAULT nTimeout TO nNetDelay

   nStartTime := Seconds() // Record the start time

   DO CASE
   CASE nLockingType == NET_LOCK_RECORD
      xLockIdentifier := iif( lReleaseLocks, NIL, RecNo() )
      bLockOperation := {| x | dbRLock( x ) }
   CASE nLockingType == NET_LOCK_FILE
      bLockOperation := {|| FLock() }
   CASE nLockingType == NET_LOCK_APPEND
      xLockIdentifier := lReleaseLocks
      bLockOperation := {| x | dbAppend( x ), ! NetErr() }
   OTHERWISE
      // Invalid lock type.
      RETURN .F.
   ENDCASE

   WHILE ( Seconds() - nStartTime ) < nTimeout // Use Seconds() for more accurate timing

      IF Eval( bLockOperation, xLockIdentifier )
         lSuccess := .T.
         EXIT
      ELSE
         HMG_SysWait( .25 )
      ENDIF

   ENDDO

   lNetOk := lSuccess // Set lNetOk based on the loop's outcome

RETURN lSuccess

/*
 * STATIC FUNCTION NetModifyRecord( bOperation, cOperationName )
 *
 * Executes a database operation (e.g., delete, recall) on the current record with network locking.
 *
 * Parameters:
 *   bOperation     (CODEBLOCK): A codeblock containing the database operation to perform (e.g., {|| dbDelete() }).
 *   cOperationName (CHARACTER): A string describing the operation being performed (e.g., "DELETE", "RECALL").
 *
 * Returns:
 *   LOGICAL: .T. if the operation was successful, .F. otherwise.
 *
 * Purpose:
 *   This function simplifies the process of performing database operations that require network locking.
 *   It encapsulates the locking logic, the database operation, and error handling into a single function.
 *   This reduces code duplication and improves code maintainability.
 *   It first attempts to acquire a record lock using NetLock(NET_LOCK_RECORD).
 *   If the lock is acquired successfully, the codeblock bOperation is evaluated.
 *   After the operation is performed, the function checks for errors using NetErr().
 *   If no errors occurred, the changes are committed to the database using dbCommit().
 *   If an error occurred, an alert message is displayed using HMG_Alert().
 *
 * Notes:
 *   The function assumes that the database is already open and positioned to the correct record.
 *   The cOperationName parameter is used for displaying an error message if the operation fails.
 */
STATIC FUNCTION NetModifyRecord( bOperation, cOperationName )

   LOCAL lSuccess := .F.

   IF NetLock( NET_LOCK_RECORD )
      Eval( bOperation )
      lSuccess := .T.
   ENDIF

   IF ! NetErr() // Check for errors BEFORE committing
      dbSkip( 0 )
      dbCommit()
   ELSE
      HMG_Alert( "Failed to " + cOperationName + " Record -> " + hb_ntos( RecNo() ) )
   ENDIF

RETURN lSuccess

/*
 * FUNCTION NetDelete()
 *
 * Deletes the current record with network locking.
 *
 * Returns:
 *   LOGICAL: .T. if the delete was successful, .F. otherwise.
 *
 * Purpose:
 *   This function provides a safe way to delete records in a multi-user environment by ensuring that
 *   only one user can delete a record at a time. It uses the NetModifyRecord function to handle the
 *   locking and deletion process.
 */
FUNCTION NetDelete()

RETURN NetModifyRecord( {|| dbDelete() }, "DELETE" )

/*
 * FUNCTION NetRecall()
 *
 * Recalls the current record with network locking.
 *
 * Returns:
 *   LOGICAL: .T. if the recall was successful, .F. otherwise.
 *
 * Purpose:
 *   This function provides a safe way to recall (undelete) records in a multi-user environment by ensuring that
 *   only one user can recall a record at a time. It uses the NetModifyRecord function to handle the
 *   locking and recall process.
 */
FUNCTION NetRecall()

RETURN NetModifyRecord( {|| dbRecall() }, "RECALL" )

/*
 * FUNCTION NetRecLock( nTimeout )
 *
 * Attempts to acquire a record lock.
 *
 * Parameters:
 *   nTimeout (NUMERIC): Optional. Defaults to nNetDelay.
 *                       The maximum time (in seconds) to wait for the lock to be acquired.
 *
 * Returns:
 *   LOGICAL: .T. if the lock was successfully acquired, .F. otherwise.
 *
 * Purpose:
 *   This function provides a convenient way to acquire a record lock. It calls the NetLock function
 *   with the NET_LOCK_RECORD constant to perform the actual locking operation.
 *
 * Notes:
 *   If the timeout period expires before the lock can be acquired, the function returns .F..
 */
FUNCTION NetRecLock( nTimeout )

   DEFAULT nTimeout TO nNetDelay

RETURN NetLock( NET_LOCK_RECORD, , nTimeout )

/*
 * FUNCTION NetFileLock( nTimeout )
 *
 * Attempts to acquire a file lock.
 *
 * Parameters:
 *   nTimeout (NUMERIC): Optional. Defaults to nNetDelay.
 *                       The maximum time (in seconds) to wait for the lock to be acquired.
 *
 * Returns:
 *   LOGICAL: .T. if the lock was successfully acquired, .F. otherwise.
 *
 * Purpose:
 *   This function provides a convenient way to acquire a file lock. It calls the NetLock function
 *   with the NET_LOCK_FILE constant to perform the actual locking operation.
 *
 * Notes:
 *   If the timeout period expires before the lock can be acquired, the function returns .F..
 */
FUNCTION NetFileLock( nTimeout )

   DEFAULT nTimeout TO nNetDelay

RETURN NetLock( NET_LOCK_FILE, , nTimeout )

/*
 * FUNCTION NetAppend( nTimeout, lReleaseLocks )
 *
 * Appends a blank record with network locking.
 *
 * Parameters:
 *   nTimeout    (NUMERIC):  Optional. Defaults to nNetDelay.
 *                           The maximum time (in seconds) to wait for the lock to be acquired.
 *   lReleaseLocks (LOGICAL): Optional. Defaults to .T..
 *                           If .T., the function releases the lock after appending.
 *                           If .F., the function keeps the lock after appending.
 *
 * Returns:
 *   LOGICAL: .T. if the append was successful, .F. otherwise.
 *
 * Purpose:
 *   This function provides a safe way to append records in a multi-user environment by ensuring that
 *   only one user can append a record at a time. It calls the NetLock function with the
 *   NET_LOCK_APPEND constant to perform the actual locking and appending operation.
 *   After appending, the function optionally releases the lock based on the lReleaseLocks parameter.
 *
 * Notes:
 *   Setting lReleaseLocks to .F. can be useful if you need to perform additional operations on the
 *   newly appended record immediately after appending it.
 */
FUNCTION NetAppend( nTimeout, lReleaseLocks )

   LOCAL cOrd
   LOCAL lSuccess

   DEFAULT nTimeout TO nNetDelay
   DEFAULT lReleaseLocks TO .T.

   cOrd := ordSetFocus( 0 ) // set order to 0 to append

   lSuccess := NetLock( NET_LOCK_APPEND, lReleaseLocks, nTimeout )

   IF ! Empty( cOrd )
      ordSetFocus( cOrd )
   ENDIF

RETURN lSuccess

/*
 * FUNCTION IsLocked( nRecId )
 *
 * Checks if a record is currently locked.
 *
 * Parameters:
 *   nRecId (NUMERIC): Optional. Defaults to the current record (RecNo()).
 *                     The record number to check.
 *
 * Returns:
 *   LOGICAL: .T. if the record is locked, .F. otherwise.
 *
 * Purpose:
 *   This function allows you to determine whether a specific record is currently locked by another user.
 *   It can be used to prevent users from attempting to modify a record that is already being modified.
 *   It retrieves a list of locked records using dbRLockList() and then searches for the specified record number.
 *
 * Notes:
 *   The function relies on the dbRLockList() function to provide an accurate list of locked records.
 *   The performance of the function may be affected if the database contains a large number of locked records.
 */
FUNCTION IsLocked( nRecId )

   DEFAULT nRecID TO RecNo()

RETURN AScan( dbRLockList(), {| n | n == nRecID } ) > 0

/*
 * FUNCTION NetError()
 *
 * Returns the status of the last network operation.
 *
 * Returns:
 *   LOGICAL: .T. if the last operation failed, .F. otherwise.
 *
 * Purpose:
 *   This function provides a simple way to check if the last network locking operation (e.g., NetLock, NetAppend)
 *   was successful. It returns the value of the lNetOk static variable, which is set by the NetLock function.
 *   This allows you to implement error handling logic based on the success or failure of network operations.
 *
 * Notes:
 *   The function only reflects the status of the last network operation.
 */
FUNCTION NetError()

RETURN ! lNetOk

/*
 * FUNCTION SetnNetDelay( nSeconds )
 *
 * Sets the default network delay.
 *
 * Parameters:
 *   nSeconds (NUMERIC): Optional. If provided, sets the new network delay in seconds.
 *                       If NIL, the delay is not changed.
 *
 * Returns:
 *   NUMERIC: The previous network delay.
 *
 * Purpose:
 *   This function allows you to adjust the default network delay, which is the maximum time that the
 *   application will wait for a network lock to be acquired.  Increasing the delay may be necessary
 *   in environments with slow or unreliable networks.  The function returns the previous delay value,
 *   allowing you to restore the original delay if needed.
 */
FUNCTION SetNetDelay( nSeconds )

   LOCAL nOldDelay := nNetDelay

   IF nSeconds != NIL
      nNetDelay := nSeconds
   ENDIF

RETURN nOldDelay
