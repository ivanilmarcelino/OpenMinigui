/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2021 Grigory Filatov <gfilatov@gmail.com>
 */

#include "minigui.ch"

#ifdef _NAMES_LIST_
/*-----------------------------------------------------------------------------*
 * FUNCTION _SetGetNamesList( cName, nIndex, lDelete )
 *-----------------------------------------------------------------------------*
 * Purpose:
 *   This function manages a list of names (strings) stored in a static object.
 *   It allows getting, setting, and deleting names from the list.  The list is
 *   implemented using an oHmgData() object, which acts as a dictionary.
 *
 * Parameters:
 *   cName   - The name (string) to get, set, or delete.
 *   nIndex  - The index (numeric) associated with the name when setting.  Ignored when getting or deleting.
 *   lDelete - A logical value indicating whether to delete the name (TRUE) or set it to NIL (FALSE).
 *
 * Return Value:
 *   If only cName is passed (PCount() == 1), returns the value associated with the name.
 *   Otherwise, returns the internal static _HMG_NAMESLIST object.
 *
 * Notes:
 *   - The _HMG_NAMESLIST is a static variable, so its value persists between calls.
 *   - oHmgData() is assumed to be a function that creates an object suitable for storing key-value pairs.
 *   - This function is only compiled if _NAMES_LIST_ is defined.
 */
FUNCTION _SetGetNamesList( cName, nIndex, lDelete )
   STATIC _HMG_NAMESLIST

   IF HB_ISNIL( _HMG_NAMESLIST )
      _HMG_NAMESLIST := oHmgData()
   ENDIF

   IF PCount() == 1

      RETURN _HMG_NAMESLIST:Get( cName, 0 )

   ELSEIF PCount() == 2

      _HMG_NAMESLIST:Set( cName, nIndex )

   ELSEIF PCount() == 3

      IF lDelete
         _HMG_NAMESLIST:Del( cName )
      ELSE
         _HMG_NAMESLIST:Set( cName, NIL )
      ENDIF

   ENDIF 

RETURN _HMG_NAMESLIST

#endif

/*-----------------------------------------------------------------------------*
 * FUNCTION _SetGetGlobal( cVarName, xNewValue, ... )
 *-----------------------------------------------------------------------------*
 * Purpose:
 *   This function manages a list of "global" variables stored in a static object.
 *   It allows getting, setting, and deleting variables from the list.  The list is
 *   implemented using an oHmgData() object, which acts as a dictionary.  This function
 *   provides a way to simulate global variables within the HMG environment.
 *
 * Parameters:
 *   cVarName  - The name (string) of the variable to get, set, or delete.
 *   xNewValue - The new value to assign to the variable when setting.
 *   ...       - (xHarbour only) Variable number of parameters passed to the function.
 *
 * Return Value:
 *   If only cVarName is passed (PCount() == 1), returns the current value of the variable.
 *   If no parameters are passed (PCount() == 0), returns the internal static _HMG_STATIC object.
 *   If setting a value, returns the *old* value of the variable.
 *
 * Notes:
 *   - The _HMG_STATIC is a static variable, so its value persists between calls.
 *   - oHmgData() is a function that creates an object suitable for storing key-value pairs.
 *   - The xHarbour version uses hb_AParams() to handle variable arguments.
 */
#ifndef __XHARBOUR__
FUNCTION _SetGetGlobal( cVarName, xNewValue, ... )
#else
FUNCTION _SetGetGlobal( ... )
#endif
   LOCAL xOldValue

#ifdef __XHARBOUR__
   LOCAL cVarName, xNewValue
   LOCAL aParams := hb_AParams()
   LOCAL nParams := Len( aParams )
#endif

   STATIC _HMG_STATIC

#ifdef __XHARBOUR__
   IF nParams > 1
      cVarName := aParams[ 1 ]
      xNewValue := aParams[ 2 ]
   ELSEIF nParams == 1
      cVarName := aParams[ 1 ]
   ENDIF
#endif
   IF HB_ISNIL( _HMG_STATIC )
      _HMG_STATIC := oHmgData()
   ENDIF

   SWITCH PCount()
   CASE 0
      RETURN _HMG_STATIC
   CASE 1
      RETURN _HMG_STATIC:Get( cVarName, NIL )
   CASE 2
      xOldValue := _HMG_STATIC:Get( cVarName, NIL )
      _HMG_STATIC:Set( cVarName, xNewValue )
      EXIT
   CASE 3
      _HMG_STATIC:Del( cVarName )
      EXIT
   ENDSWITCH

RETURN xOldValue

/*-----------------------------------------------------------------------------*
 * FUNCTION _AddNewGlobal( cVarName, xValue )
 *-----------------------------------------------------------------------------*
 * Purpose:
 *   This function adds a new "global" variable to the application.  It first checks
 *   if the variable already exists using _SetGetGlobal(). If it doesn't exist, it
 *   declares a STATIC variable with the given name and value.  This allows for
 *   dynamic creation of static variables at runtime.
 *
 * Parameters:
 *   cVarName - The name (string) of the new global variable.
 *   xValue   - The initial value of the new global variable.
 *
 * Return Value:
 *   Returns the value of the newly created (or existing) global variable, obtained
 *   through a call to _SetGetGlobal().
 *
 * Notes:
 *   - This function relies on the Harbour compiler's ability to create static
 *     variables dynamically using the STATIC &cVarName AS GLOBAL VALUE xValue syntax.
 *   - The & symbol is used for macro substitution, allowing the variable name to be
 *     dynamically inserted into the STATIC declaration.
 */
FUNCTION _AddNewGlobal( cVarName, xValue )
   // If cVarName not found, then ...
   IF _SetGetGlobal( cVarName ) == NIL
      // Add a new variable in the Pseudo-Variable List
      STATIC &cVarName AS GLOBAL VALUE xValue
   ENDIF

RETURN _SetGetGlobal( cVarName )

/*-----------------------------------------------------------------------------*
 * FUNCTION CheckStatic()
 *-----------------------------------------------------------------------------*
 * Purpose:
 *   This function iterates through all static variables defined in the application
 *   and logs their information to a file.  It's primarily a debugging tool to
 *   inspect the contents of static variables.
 *
 * Parameters:
 *   None
 *
 * Return Value:
 *   NIL
 *
 * Notes:
 *   - It uses _SetGetLogFile() and _LogFile() to manage the log file.
 *   - nStatics() and Static(n) are used to retrieve the number of static variables
 *     and the value of the nth static variable, respectively.
 *   - Scan() is a recursive function used to traverse and log the contents of
 *     complex data structures (arrays, objects) stored in static variables.
 */
FUNCTION CheckStatic()
   LOCAL cInfo := ""
   LOCAL nCnt := 0
   LOCAL n

   _SetGetLogFile( GetStartUpFolder() + hb_ps() + "checkstatic.txt" )
   FErase( _SetGetLogFile() )

   cInfo += "Statics variables:" + CRLF
   cInfo += "=================="
   _LogFile( .F., cInfo )

   FOR n = 1 TO nStatics()
      if ! Empty( Static( n ) )
         cInfo := CRLF
         cInfo += hb_ntos( n ) + Replicate( "-", 55 ) + "> "
         _LogFile( .F., cInfo )
         Scan( Static( n ) )
         nCnt++
      ENDIF
   NEXT

   cInfo := CRLF
   cInfo += Replicate( "-", 59 ) + "> "
   _LogFile( .F., cInfo )

   _LogFile( .T., "Amount = " + hb_ntos( nCnt ) )
   _LogFile( .T., Replicate( "=", 60 ) )
   _LogFile( .T., GetExeFileName() )

RETURN NIL

/*-----------------------------------------------------------------------------*
 * STATIC FUNCTION Scan( u, cData )
 *-----------------------------------------------------------------------------*
 * Purpose:
 *   This recursive function scans a variable and logs its type, value, and
 *   contents to a log file.  It's used by CheckStatic() to inspect the contents
 *   of static variables, especially complex data structures like arrays and objects.
 *
 * Parameters:
 *   u     - The variable to scan.
 *   cData - An optional string containing additional data to log (e.g., the name
 *           of the variable being scanned).
 *
 * Return Value:
 *   NIL
 *
 * Notes:
 *   - ValType() returns the data type of a variable as a string (e.g., "A" for array,
 *     "C" for character, "N" for numeric).
 *   - cValToChar() converts a variable to a character string representation.
 *   - The function handles nested arrays and objects recursively, but limits the
 *     recursion depth to 50 to prevent stack overflows.
 *   - DoEvents() is called periodically to allow the application to remain responsive
 *     during the scanning process.
 */
STATIC FUNCTION Scan( u, cData )
   LOCAL cType := ValType( u )
   LOCAL n
   LOCAL cInfo := ""

   STATIC nNested := 0

   DEFAULT cData := ""

   DoEvents()

   IF nNested > 50
      cInfo += CRLF
      cInfo += Space( nNested ) + "Break on 50 nested loops"
      cInfo += CRLF
      _LogFile( .T., cInfo )
   ELSE
      // We write the data and its containts
      _LogFile( .F., CRLF + Space( nNested ) + cType + " " + cData )
      IF .NOT. cType $ "UAO"
         _LogFile( .F., " " + cValToChar( u ) )
      ENDIF

      IF cType $ "AO"
         _LogFile( .F., " (Len = " + hb_ntos( Len( u ) ) + ")" )
      ENDIF

      IF cType == "A"

         FOR n := 1 TO Len( u )

            DoEvents()

            IF ValType( u[ n ] ) == ValType( u ) .AND. u[ n ] == u
               _LogFile( .T., " Direct reference to its container" )
            ELSE
               nNested++
               Scan( u[ n ], cData )
               nNested--
            ENDIF

         NEXT n

      ENDIF

   ENDIF

RETURN NIL

#ifndef __XHARBOUR__
#translate HB_DBG_VMVARSLEN   =>  __dbgVMVarSLen
#translate HB_DBG_VMVARSLIST  =>  __dbgVMVarSList
#translate HB_DBG_VMVARSGET   =>  __dbgVMVarSGet
#endif
/*-----------------------------------------------------------------------------*
 * STATIC FUNCTION nStatics()
 *-----------------------------------------------------------------------------*
 * Purpose:
 *   This function returns the number of static variables defined in the application.
 *   It uses the HB_DBG_VMVARSLEN() function (or its translated equivalent in
 *   non-xHarbour environments) to retrieve this information from the Harbour
 *   virtual machine.
 *
 * Parameters:
 *   None
 *
 * Return Value:
 *   The number of static variables.
 *
 * Notes:
 *   - HB_DBG_VMVARSLEN() is a Harbour debugging function that provides access
 *     to the internal state of the virtual machine.
 */
STATIC FUNCTION nStatics()

RETURN HB_DBG_VMVARSLEN()

/*-----------------------------------------------------------------------------*
 * STATIC FUNCTION Static( n )
 *-----------------------------------------------------------------------------*
 * Purpose:
 *   This function returns the value of the nth static variable defined in the
 *   application.  It uses the HB_DBG_VMVARSGET() and HB_DBG_VMVARSLIST() functions
 *   (or their translated equivalents in non-xHarbour environments) to retrieve
 *   this information from the Harbour virtual machine.
 *
 * Parameters:
 *   n - The index of the static variable to retrieve (1-based).
 *
 * Return Value:
 *   The value of the nth static variable.
 *
 * Notes:
 *   - HB_DBG_VMVARSGET() and HB_DBG_VMVARSLIST() are Harbour debugging functions
 *     that provide access to the internal state of the virtual machine.
 */
STATIC FUNCTION Static( n )

RETURN HB_DBG_VMVARSGET( HB_DBG_VMVARSLIST(), n )
