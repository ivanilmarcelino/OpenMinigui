/*
 * MINIGUI Registry Unit Tests
*/

#include "minigui.ch"

#define REGISTRY_PATH "Software\\MiniGUI\\Test"

// Static variables for logging and test tracking
STATIC aTestResults // Array to hold test result strings
STATIC nTestsPassed // Counter for passed tests
STATIC nTestsFailed // Counter for failed tests

// ==================================
// Entry point: Main window routine
// ==================================
FUNCTION Main()

   // Define main window
   DEFINE WINDOW oWnd TITLE "Registry Unit Tests" ;
         WIDTH 400 HEIGHT 320 ;
         MAIN

      // Listbox to show test results
      @ 10, 10 LISTBOX oList ;
         WIDTH 365 HEIGHT 220 ;
         ITEMS {} ;
         FONT "Courier New" SIZE 9

      // Run Tests button
      @ 240, 10 BUTTON NUL CAPTION "Run Tests" ;
         WIDTH 100 HEIGHT 30 ;
         ACTION RunTests() ;
         DEFAULT

      // Close button
      @ 240, 120 BUTTON NUL CAPTION "Close" ;
         WIDTH 100 HEIGHT 30 ;
         ACTION oWnd.Release()

   END WINDOW

   CENTER WINDOW oWnd   // Center the window on screen
   ACTIVATE WINDOW oWnd // Start the event loop

RETURN NIL

// ==================================
// RunTests() - Executes all test cases
// ==================================
FUNCTION RunTests()

   // Initialize counters and result array
   nTestsPassed := 0
   nTestsFailed := 0
   aTestResults := {}

   // Run individual tests and log results
   TestRun( "CreateRegistryKey", Test_CreateRegistryKey() )
   TestRun( "SetRegistryValue", Test_SetRegistryValue() )
   TestRun( "GetRegistryValue", Test_GetRegistryValue() )
   TestRun( "IsRegistryKey", Test_IsRegistryKey() )
   TestRun( "DeleteRegistryVar", Test_DeleteRegistryVar() )
   TestRun( "DeleteRegistryKey", Test_DeleteRegistryKey() )

   // Append test summary to log
   AAdd( aTestResults, "----------------------------" )
   AAdd( aTestResults, "Passed: " + hb_ntos( nTestsPassed ) )
   AAdd( aTestResults, "Failed: " + hb_ntos( nTestsFailed ) )
   AAdd( aTestResults, "----------------------------" )

   // Update listbox with test log
   UpdateListbox( aTestResults )

RETURN NIL

// ==================================
// TestRun() - Logs individual test outcome
// ==================================
FUNCTION TestRun( cName, lResult )

   LOCAL cLine

   // Format test result string
   IF lResult
      cLine := "[PASS] " + cName
      nTestsPassed++
   ELSE
      cLine := "[FAIL] " + cName
      nTestsFailed++
   ENDIF

   // Add result to log array
   AAdd( aTestResults, cLine )

RETURN NIL

// ==================================
// Unit Test: Create a registry key
// ==================================
FUNCTION Test_CreateRegistryKey()
RETURN CreateRegistryKey( HKEY_CURRENT_USER, REGISTRY_PATH )

// ==================================
// Unit Test: Set a registry value
// ==================================
FUNCTION Test_SetRegistryValue()
RETURN SetRegistryValue( HKEY_CURRENT_USER, REGISTRY_PATH, "Username", "HarbourUser" )

// ==================================
// Unit Test: Get a registry value
// ==================================
FUNCTION Test_GetRegistryValue()

   LOCAL cVal := GetRegistryValue( HKEY_CURRENT_USER, REGISTRY_PATH, "Username" )

RETURN cVal == "HarbourUser"

// ==================================
// Unit Test: Check registry key existence
// ==================================
FUNCTION Test_IsRegistryKey()
RETURN IsRegistryKey( HKEY_CURRENT_USER, REGISTRY_PATH )

// ==================================
// Unit Test: Delete a registry value
// ==================================
FUNCTION Test_DeleteRegistryVar()
RETURN DeleteRegistryVar( HKEY_CURRENT_USER, REGISTRY_PATH, "Username" )

// ==================================
// Unit Test: Delete the registry key
// ==================================
FUNCTION Test_DeleteRegistryKey()
RETURN DeleteRegistryKey( HKEY_CURRENT_USER, REGISTRY_PATH )

// ==================================
// Helper function to update the listbox
// ==================================
FUNCTION UpdateListbox( aItems )
   oWnd.oList.SetArray( aItems )

RETURN NIL
