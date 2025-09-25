/*
 * test_hbziparc.prg
 * Unit tests for hbziparc library using Harbour MiniGUI
 * Created: 27.07.2025
 * Author: Grigory Filatov
 */

#include "minigui.ch"
#include "fileio.ch"

STATIC aTestResults := {} // Array to store test results
STATIC cZipComment := ""  // Global zip file comment

/*
 * FUNCTION Main()
 *
 * Initializes the application, defines the main window, and runs the hbziparc unit tests.
 *
 * Purpose:
 *   This is the main entry point of the application. It defines the main window,
 *   creates a grid to display test results, and provides a button to execute the tests.
 *   The test results are dynamically colored based on their success or failure.
 *   A summary label displays the number of passed and total tests.
 *
 * Notes:
 *   The aTestResults array stores the results of each test.
 *   The bColor block dynamically sets the foreground color of the grid rows based on the test result.
 */
FUNCTION Main()
   LOCAL bColor := {|x, nRow| IIF(aTestResults[nRow][2], {0, 128, 0}, {255, 0, 0})} // Dynamic row coloring based on test result

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 500 ;
      HEIGHT 440 ;
      TITLE "hbziparc Unit Tests" ;
      MAIN

      // Test results grid
      @ 10, 10 GRID Grid_1 ;
         WIDTH 460 ;
         HEIGHT 300 ;
         HEADERS {"Test Name", "Result"} ;
         WIDTHS {300, 120} ;
         ITEMS {} ; // Initially empty
         FONT "Arial" SIZE 9 ;
         JUSTIFY {GRID_JTFY_LEFT, GRID_JTFY_CENTER} ;
         DYNAMICFORECOLOR {bColor, bColor}

      // Summary label
      @ 320, 10 LABEL lblSummary ;
         WIDTH 440 ;
         HEIGHT 25 ;
         VALUE "" ;
         FONT "Arial" SIZE 10 BOLD ;
         CENTERALIGN

      // Run tests button
      @ 350, 10 BUTTON btnRun ;
         CAPTION "Run Tests" ;
         ACTION RunAllTests() ;
         WIDTH 120 ;
         HEIGHT 30 ;
         DEFAULT

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

// --------------------- Test Execution Functions ---------------------

/*
 * PROCEDURE RunAllTests()
 *
 * Executes all unit tests and updates the user interface with the results.
 *
 * Purpose:
 *   This procedure orchestrates the execution of all unit tests for the hbziparc library.
 *   It records the start time, resets the test results array, calls the core test logic,
 *   refreshes the UI to display the results, and displays an alert with the execution time.
 *
 * Notes:
 *   The RunAllTestsCore() procedure contains the actual test logic.
 *   The RefreshUI() procedure updates the GUI with the test results.
 */
PROCEDURE RunAllTests()
   LOCAL nStart := Seconds() // Record start time
   aTestResults := {}        // Reset test results
   RunAllTestsCore()         // Run core test logic
   RefreshUI()               // Update GUI
   Alert("Finished in " + LTrim(Str(Seconds() - nStart, 5, 2)) + " sec") // Show execution time

RETURN

/*
 * STATIC PROCEDURE RunAllTestsCore()
 *
 * Executes the core test logic for the hbziparc library.
 *
 * Purpose:
 *   This procedure contains the core logic for running all the unit tests.
 *   It defines the test zip file name, cleans up any existing test files and directories,
 *   and then adds each test and its result to the aTestResults array.
 *
 * Notes:
 *   The CleanUp() procedure ensures a clean environment before running the tests.
 *   Each AAdd() call adds a test name and its result (a logical value) to the aTestResults array.
 */
STATIC PROCEDURE RunAllTestsCore()
   LOCAL cZipFile := "test.zip" // Test zip file name

   CleanUp() // Ensure clean environment

   // Add tests to results array
   AAdd(aTestResults, {"hb_SetZipComment", Test_SetZipComment()})
   AAdd(aTestResults, {"ZipSingleFile", Test_ZipSingleFile(cZipFile)})
   AAdd(aTestResults, {"UnzipSingleFile", Test_UnzipSingleFile(cZipFile)})
   AAdd(aTestResults, {"InvalidZipHandling", Test_InvalidZipHandling()})
   AAdd(aTestResults, {"hb_GetZipComment", Test_GetZipComment(cZipFile)})
   AAdd(aTestResults, {"hb_GetFileCount", Test_GetFileCount(cZipFile)})
   AAdd(aTestResults, {"hb_ZipWithPassword", Test_ZipWithPassword(cZipFile)})
   AAdd(aTestResults, {"hb_GetFilesInZip", Test_GetFilesInZip(cZipFile)})
   AAdd(aTestResults, {"ZipUnzipDirectory", Test_ZipUnzipDirectory()})
   AAdd(aTestResults, {"ZipUnzipWithPassword", Test_ZipUnzipWithPassword()})

RETURN

// --------------------- Individual Test Functions ---------------------

/*
 * FUNCTION Test_SetZipComment()
 *
 * Tests the hb_SetZipComment() function.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   .T. (Always returns true, indicating the function call was successful)
 *
 * Purpose:
 *   This function tests the functionality of setting a zip file comment using the hb_SetZipComment() function.
 *   It sets a predefined comment and returns .T. to indicate that the function call itself was successful.
 *   The actual verification of the comment being set correctly is done in the Test_GetZipComment() function.
 *
 * Notes:
 *   This function only tests the successful execution of hb_SetZipComment().
 *   The Test_GetZipComment() function verifies that the comment was actually set correctly.
 */
FUNCTION Test_SetZipComment()
   LOCAL cComment := "Unit Test Comment"
   hb_SetZipComment(cComment)
RETURN .T.

/*
 * FUNCTION Test_GetZipComment()
 *
 * Tests the hb_GetZipComment() function.
 *
 * Parameters:
 *   cFile (STRING): The name of the zip file to retrieve the comment from.
 *
 * Returns:
 *   LOGICAL: .T. if the retrieved comment matches the expected comment, .F. otherwise.
 *
 * Purpose:
 *   This function tests the functionality of retrieving a zip file comment using the hb_GetZipComment() function.
 *   It calls hb_GetZipComment() to retrieve the comment from the specified zip file and then compares it to the expected comment.
 *   The result of the comparison is returned as a logical value.
 *
 * Notes:
 *   This function relies on the Test_SetZipComment() function having been executed previously to set the zip file comment.
 *   The global variable cZipComment is updated with the retrieved comment.
 */
FUNCTION Test_GetZipComment(cFile)
   cZipComment := hb_GetZipComment(cFile)
RETURN ValType(cZipComment) == "C" .AND. cZipComment == "Unit Test Comment"

/*
 * FUNCTION Test_InvalidZipHandling()
 *
 * Tests the handling of invalid zip files using the hb_IsZipFile() function.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   LOGICAL: .T. if hb_IsZipFile() returns .F. for a nonexistent file, .F. otherwise.
 *
 * Purpose:
 *   This function tests the ability of the hb_IsZipFile() function to correctly identify invalid zip files.
 *   It calls hb_IsZipFile() with a nonexistent file name and expects it to return .F.
 *   The result of the test is returned as a logical value.
 *
 * Notes:
 *   This function tests a negative scenario, ensuring that the hb_IsZipFile() function correctly identifies invalid zip files.
 */
FUNCTION Test_InvalidZipHandling()
RETURN !hb_IsZipFile("nonexistent.zip")

/*
 * FUNCTION Test_GetFileCount()
 *
 * Tests the hb_GetFileCount() function.
 *
 * Parameters:
 *   cFile (STRING): The name of the zip file to retrieve the file count from.
 *
 * Returns:
 *   LOGICAL: .T. if hb_GetFileCount() returns a numeric value, .F. otherwise.
 *
 * Purpose:
 *   This function tests the functionality of retrieving the number of files in a zip archive using the hb_GetFileCount() function.
 *   It calls hb_GetFileCount() to retrieve the file count from the specified zip file and then checks if the returned value is numeric.
 *   The result of the test is returned as a logical value.
 *
 * Notes:
 *   This function only checks the data type of the returned value, not the actual file count.
 */
FUNCTION Test_GetFileCount(cFile)
RETURN ValType(hb_GetFileCount(cFile)) == "N"

/*
 * FUNCTION Test_ZipWithPassword()
 *
 * Tests the hb_ZipWithPassword() function.
 *
 * Parameters:
 *   cFile (STRING): The name of the zip file to create.
 *
 * Returns:
 *   LOGICAL: .T. if hb_ZipWithPassword() returns a logical value, .F. otherwise.
 *
 * Purpose:
 *   This function tests the functionality of creating a password-protected zip archive using the hb_ZipWithPassword() function.
 *   It calls hb_ZipWithPassword() to create a password-protected zip file and then checks if the returned value is a logical value.
 *   The result of the test is returned as a logical value.
 *
 * Notes:
 *   This function only checks the data type of the returned value, not the actual creation of the zip file or the password protection.
 */
FUNCTION Test_ZipWithPassword(cFile)
RETURN ValType(hb_ZipWithPassword(cFile)) == "L"

/*
 * FUNCTION Test_GetFilesInZip()
 *
 * Tests the hb_GetFilesInZip() function.
 *
 * Parameters:
 *   cFile (STRING): The name of the zip file to retrieve the file list from.
 *
 * Returns:
 *   LOGICAL: .T. if the retrieved file list contains the expected file name, .F. otherwise.
 *
 * Purpose:
 *   This function tests the functionality of retrieving the list of files in a zip archive using the hb_GetFilesInZip() function.
 *   It calls hb_GetFilesInZip() to retrieve the file list from the specified zip file and then checks if the list contains the expected file name ("testfile.txt").
 *   The result of the test is returned as a logical value.
 *
 * Notes:
 *   This function assumes that the zip file contains a file named "testfile.txt".
 */
FUNCTION Test_GetFilesInZip(cFile)
   LOCAL aFiles := hb_GetFilesInZip(cFile, .F.)
RETURN Len(aFiles) == 1 .AND. aFiles[1] == "testfile.txt"

/*
 * FUNCTION Test_ZipSingleFile()
 *
 * Tests the hb_ZipFile() function by zipping a single file.
 *
 * Parameters:
 *   cZip (STRING): The name of the zip file to create.
 *
 * Returns:
 *   LOGICAL: .T. if the file is successfully zipped, .F. otherwise.
 *
 * Purpose:
 *   This function tests the ability to zip a single file using the hb_ZipFile() function.
 *   It first creates a test file with some content, then calls hb_ZipFile() to zip the file.
 *   The function returns .T. if the zipping process is successful, and .F. otherwise.
 *
 * Notes:
 *   The SafeFileWrite() function is used to safely create the test file.
 *   The test file is named "testfile.txt".
 */
FUNCTION Test_ZipSingleFile(cZip)
   LOCAL cFile := "testfile.txt"
   IF !SafeFileWrite(cFile, "This is a test.")
      RETURN .F.
   ENDIF
RETURN hb_ZipFile(cZip, {cFile}, 5, NIL, .T., NIL, .F., .F., NIL, .T., NIL)

/*
 * FUNCTION Test_UnzipSingleFile()
 *
 * Tests the hb_UnzipFile() function by unzipping a single file.
 *
 * Parameters:
 *   cZip (STRING): The name of the zip file to unzip.
 *
 * Returns:
 *   LOGICAL: .T. if the file is successfully unzipped, .F. otherwise.
 *
 * Purpose:
 *   This function tests the ability to unzip a single file using the hb_UnzipFile() function.
 *   It first creates a directory to unzip the file into, then calls hb_UnzipFile() to unzip the file.
 *   The function returns .T. if the unzipping process is successful and the unzipped file exists, and .F. otherwise.
 *
 * Notes:
 *   The hb_DirBuild() function is used to create the output directory.
 *   The hb_GetFilesInZip() function is used to get the name of the file in the zip archive.
 *   The output directory is named "unzipped/".
 */
FUNCTION Test_UnzipSingleFile(cZip)
   LOCAL cOutDir := "unzipped/"
   LOCAL cFile
   hb_DirBuild(cOutDir)
   IF !hb_UnzipFile(cZip, NIL, .F., NIL, cOutDir, NIL, NIL)
      RETURN .F.
   ENDIF
   cFile := hb_GetFilesInZip(cZip, .F.)[1]
RETURN hb_FileExists(cOutDir + cFile)

/*
 * FUNCTION Test_ZipUnzipDirectory()
 *
 * Tests zipping and unzipping a directory.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   LOGICAL: .T. if the directory is successfully zipped and unzipped, and the content of the unzipped file matches the original, .F. otherwise.
 *
 * Purpose:
 *   This function tests the ability to zip and unzip a directory containing a single file.
 *   It creates a directory, creates a file inside the directory, zips the directory, unzips the directory,
 *   and then verifies that the content of the unzipped file matches the original content.
 *
 * Notes:
 *   The test directory is named "testdir/".
 *   The zip file is named "testdir.zip".
 *   The output directory is named "unzipped_dir/".
 *   The test file is named "file1.txt".
 */
FUNCTION Test_ZipUnzipDirectory()
   LOCAL cDir := "testdir/"
   LOCAL cZip := "testdir.zip"
   LOCAL cOutDir := "unzipped_dir/"
   LOCAL cOutFile := cOutDir + "testdir/file1.txt"
   LOCAL cFile := cDir + "file1.txt"
   LOCAL cContent := "Directory file test."

   IF !hb_DirExists(cDir)
      hb_DirBuild(cDir)
   ENDIF
   IF !SafeFileWrite(cFile, cContent)
      RETURN .F.
   ENDIF
   IF !hb_ZipFile(cZip, {cFile}, 5, NIL, .T., NIL, .T., .F., NIL, .T., NIL)
      RETURN .F.
   ENDIF
   IF !hb_UnzipFile(cZip, NIL, .T., NIL, cOutDir, NIL, NIL)
      RETURN .F.
   ENDIF
   IF !hb_FileExists(cOutFile)
      RETURN .F.
   ENDIF
RETURN SafeFileRead(cOutFile) == cContent

/*
 * FUNCTION Test_ZipUnzipWithPassword()
 *
 * Tests zipping and unzipping a file with a password.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   LOGICAL: .T. if the file is successfully zipped and unzipped with the password, and the content of the unzipped file matches the original, .F. otherwise.
 *
 * Purpose:
 *   This function tests the ability to zip and unzip a file with a password.
 *   It creates a file, zips the file with a password, unzips the file with the same password,
 *   and then verifies that the content of the unzipped file matches the original content.
 *
 * Notes:
 *   The test file is named "secret.txt".
 *   The zip file is named "secret.zip".
 *   The output directory is named "unzipped_secret/".
 *   The password is "secure123".
 */
FUNCTION Test_ZipUnzipWithPassword()
   LOCAL cFile := "secret.txt"
   LOCAL cZip := "secret.zip"
   LOCAL cOutDir := "unzipped_secret/"
   LOCAL cOutFile := cOutDir + "secret.txt"
   LOCAL cContent := "Top secret contents."
   LOCAL cPass := "secure123"

   IF !SafeFileWrite(cFile, cContent)
      RETURN .F.
   ENDIF
   IF !hb_ZipFile(cZip, {cFile}, 5, NIL, .T., cPass)
      RETURN .F.
   ENDIF
   IF !hb_UnzipFile(cZip, NIL, .T., cPass, cOutDir, NIL, NIL)
      RETURN .F.
   ENDIF
   IF !hb_FileExists(cOutFile)
      RETURN .F.
   ENDIF
RETURN SafeFileRead(cOutFile) == cContent

// --------------------- GUI Helper Functions ---------------------

/*
 * STATIC PROCEDURE RefreshUI()
 *
 * Updates the GUI with the test results.
 *
 * Purpose:
 *   This procedure updates the main window's title with the defined zip comment,
 *   sets the grid's items with the test results, and sets the summary label's value with the test summary.
 *
 * Notes:
 *   The GetGridItems() function prepares the data for the grid.
 *   The GetSummaryText() function generates the summary text.
 */
STATIC PROCEDURE RefreshUI()
   Form_1.Title := "Defined Zip Comment is: " + cZipComment
   DoMethod("Form_1", "Grid_1", "SETARRAY", GetGridItems())
   SetProperty("Form_1", "lblSummary", "VALUE", GetSummaryText())
RETURN

/*
 * FUNCTION GetGridItems()
 *
 * Prepares the data for the test results grid.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   ARRAY: An array of arrays, where each inner array represents a row in the grid, containing the test name and result.
 *
 * Purpose:
 *   This function iterates through the aTestResults array and prepares the data to be displayed in the test results grid.
 *   For each test, it extracts the test name and result, and formats them into an array that can be used by the grid.
 *
 * Notes:
 *   The aTestResults array contains the test results.
 */
FUNCTION GetGridItems()
   LOCAL cName, lStatus, cResult, aRows := {}, i
   FOR i := 1 TO Len(aTestResults)
      cName := aTestResults[i][1]
      lStatus := aTestResults[i][2]
      cResult := IF(lStatus, "Passed", "Failed")
      AAdd(aRows, {cName, cResult})
   NEXT
RETURN aRows

/*
 * FUNCTION GetSummaryText()
 *
 * Generates the summary text for the test results.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   STRING: A string containing the number of passed tests and the total number of tests.
 *
 * Purpose:
 *   This function calculates the number of passed tests and generates a summary string that can be displayed in the UI.
 *   It iterates through the aTestResults array and counts the number of tests that failed.
 *   The summary string includes the number of passed tests and the total number of tests.
 *
 * Notes:
 *   The aTestResults array contains the test results.
 */
FUNCTION GetSummaryText()
   LOCAL nFails := 0, i
   FOR i := 1 TO Len(aTestResults)
      IF !aTestResults[i][2]
         nFails++
      ENDIF
   NEXT
RETURN "Tests Passed: " + LTrim(Str(Len(aTestResults) - nFails)) + ;
       " / " + LTrim(Str(Len(aTestResults)))

// --------------------- File I/O Helper Functions ---------------------

/*
 * FUNCTION SafeFileWrite()
 *
 * Safely writes data to a file.
 *
 * Parameters:
 *   cFile (STRING): The name of the file to write to.
 *   cData (STRING): The data to write to the file.
 *
 * Returns:
 *   LOGICAL: .T. if the data is successfully written to the file, .F. otherwise.
 *
 * Purpose:
 *   This function provides a safe way to write data to a file.
 *   It handles potential errors during file creation and writing, and displays an alert if an error occurs.
 *
 * Notes:
 *   This function uses the FCreate(), FWrite(), and FClose() functions to write the data to the file.
 */
FUNCTION SafeFileWrite(cFile, cData)
   LOCAL h := FCreate(cFile)
   IF h == F_ERROR
      Alert("Cannot create: " + cFile)
      RETURN .F.
   ENDIF
   FWrite(h, cData)
   FClose(h)
RETURN .T.

/*
 * FUNCTION SafeFileRead()
 *
 * Safely reads the content of a file.
 *
 * Parameters:
 *   cFile (STRING): The name of the file to read from.
 *
 * Returns:
 *   STRING: The content of the file, or an empty string if an error occurs.
 *
 * Purpose:
 *   This function provides a safe way to read the content of a file.
 *   It handles potential errors during file opening and reading, and returns an empty string if an error occurs.
 *
 * Notes:
 *   This function uses the FOpen(), FSeek(), FRead(), and FClose() functions to read the content of the file.
 */
FUNCTION SafeFileRead(cFile)
   LOCAL h := FOpen(cFile, FO_READ)
   LOCAL cBuf
   IF h == F_ERROR
      RETURN ""
   ENDIF
   cBuf := Space(FSeek(h, 0, FS_END))
   FSeek(h, 0, FS_SET)
   FRead(h, @cBuf, Len(cBuf))
   FClose(h)
RETURN cBuf

// --------------------- Cleanup Functions ---------------------

/*
 * STATIC PROCEDURE CleanUp()
 *
 * Removes test files and directories.
 *
 * Purpose:
 *   This procedure removes all test files and directories that may have been created during the test execution.
 *   This ensures that the test environment is clean before each test run.
 *
 * Notes:
 *   The hb_FileDelete() function is used to delete files.
 *   The hb_DirRemoveAll() function is used to remove directories and their contents.
 */
STATIC PROCEDURE CleanUp()
   LOCAL aFiles := {"testfile.txt", "secret.txt", "test.zip", "secret.zip", "testdir.zip"}
   AEval(aFiles, {|c| hb_FileDelete(c)})
   hb_DirRemoveAll("unzipped")
   hb_DirRemoveAll("unzipped_dir")
   hb_DirRemoveAll("unzipped_secret")
   hb_DirRemoveAll("testdir")
RETURN
