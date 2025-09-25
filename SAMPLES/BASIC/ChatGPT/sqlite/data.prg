#include "minigui.ch"
#include "hbsqlit3.ch"

STATIC g_cUsername := ""

/*
 * FUNCTION LoadAppSettings()
 *
 * Loads the username from the configuration file (app.ini) into a global variable.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   cUsername (string): The username read from app.ini, defaults to "Guest" if not found.
 *
 * Purpose:
 *   Initializes the global g_cUsername variable by reading from app.ini for use in UI elements like a welcome label. Called at application startup.
 *
 * Notes:
 *   Uses IniRead from the HMG Extended library. Defaults to "Guest" if app.ini is missing or corrupted to ensure functionality.
 */
FUNCTION LoadAppSettings()
   // Read username from app.ini, default to "Guest" if not found
   g_cUsername := IniRead( "app.ini", "User", "Username", "Guest" )

RETURN g_cUsername

/*
 * PROCEDURE SaveAppSettings()
 *
 * Saves the username from the settings textbox to the configuration file (app.ini).
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Nil
 *
 * Purpose:
 *   Persists the username entered in the settings tab to app.ini for retention across sessions. Triggered by the "Save Settings" button.
 *
 * Notes:
 *   Validates non-empty input to prevent invalid saves. Shows a confirmation message via MsgInfo. Skips saving if input is empty to preserve existing data.
 */
PROCEDURE SaveAppSettings()
   // Retrieve username from settings textbox
   LOCAL cInput := GetProperty( "oWndMain", "txtUser", "Value" )
   // Check if username is not empty to avoid invalid saves
   IF ! Empty( cInput )
      // Write username to app.ini file
      IniWrite( "app.ini", "User", "Username", cInput )
      // Confirm save operation to user
      MsgInfo( "Settings saved to app.ini" )
   ENDIF

RETURN

/*
 * FUNCTION GetUserName()
 *
 * Retrieves the current username from the global variable.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   cUsername (string): The current value of g_cUsername.
 *
 * Purpose:
 *   Provides access to the global username for UI elements, such as displaying a welcome message. Used after loading or updating settings.
 *
 * Notes:
 *   Returns an empty string if g_cUsername is not initialized by LoadAppSettings().
 */
FUNCTION GetUserName()
RETURN g_cUsername

/*
 * PROCEDURE InitSQLiteDB()
 *
 * Initializes a SQLite database (people.db) with a "people" table and sample data.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Nil
 *
 * Purpose:
 *   Creates a "people" table and inserts sample records (name, age) if people.db is missing. Called at startup to ensure a functional database for grid display.
 *
 * Notes:
 *   Drops existing table, overwriting data, for a clean state. Uses parameterized queries to prevent SQL injection. Closes database connection to free resources.
 */
PROCEDURE InitSQLiteDB()
   LOCAL cSQL := "INSERT INTO people (name, age) VALUES (:name, :age)"
   // Open or create SQLite database file
   LOCAL hDb := sqlite3_open( "people.db", .T. )
   // Create table with ID (auto-increment), name, and age columns
   sqlite3_exec( hDb, ;
      "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER);" )
   // Insert sample records using parameterized query
   sqlite3_bind_exec( hDb, cSQL, { 'Alice', 30 } )
   sqlite3_bind_exec( hDb, cSQL, { 'Bob', 25 } )
   sqlite3_bind_exec( hDb, cSQL, { 'Charlie', 35 } )
   // Close database to free resources
   hDb := NIL

RETURN

/*
 * FUNCTION sqlite3_fetch_all()
 *
 * Retrieves all rows from a SQLite query result as a Harbour array.
 *
 * Parameters:
 *   db (handle): SQLite database connection handle.
 *   cSQL (string): SQL query to execute.
 *   lRawTypes (logical, optional): If .T., returns raw data types; if .F., converts to strings. Defaults to .F.
 *
 * Returns:
 *   aData (array): Array of rows, each an array of column values, or NIL if query fails or no data is found.
 *
 * Purpose:
 *   Fetches all rows from a SQLite query for UI components like grids. Used to populate the "people" table data in the main grid.
 *
 * Notes:
 *   Supports NULL, INTEGER, FLOAT, TEXT, and BLOB types. Converts to strings for UI compatibility unless lRawTypes is .T. Finalizes statement to prevent resource leaks.
 */
FUNCTION sqlite3_fetch_all( db, cSQL, lRawTypes )
   LOCAL stmt, aData := {}, aRow, i, nType

   // Validate inputs
   IF db == NIL .OR. Empty( cSQL )
      RETURN NIL // Indicate failure
   ENDIF

   // Prepare SQL statement
   stmt := sqlite3_prepare( db, cSQL )
   IF stmt == NIL
      RETURN NIL // Preparation failed
   ENDIF

   DEFAULT lRawTypes := .F.

   // Iterate through result set
   DO WHILE sqlite3_step( stmt ) == SQLITE_ROW
      aRow := {}
      // Process each column
      FOR i := 1 TO sqlite3_column_count( stmt )
         nType := sqlite3_column_type( stmt, i )
         DO CASE
         CASE nType == SQLITE_NULL
            AAdd( aRow, iif( lRawTypes, NIL, "NULL" ) ) // Return NIL for raw types, "NULL" for string
         CASE nType == SQLITE_INTEGER
            AAdd( aRow, iif( lRawTypes, sqlite3_column_int( stmt, i ), LTrim( Str( sqlite3_column_int( stmt, i ) ) ) ) )
         CASE nType == SQLITE_FLOAT
            AAdd( aRow, iif( lRawTypes, sqlite3_column_double( stmt, i ), LTrim( Str( sqlite3_column_double( stmt, i ) ) ) ) )
         CASE nType == SQLITE_TEXT
            AAdd( aRow, sqlite3_column_text( stmt, i ) )
         CASE nType == SQLITE_BLOB
            AAdd( aRow, iif( lRawTypes, sqlite3_column_blob( stmt, i ), "<BLOB>" ) )
         OTHERWISE
            AAdd( aRow, "UNKNOWN" ) // Handle unexpected types
         ENDCASE
      NEXT
      AAdd( aData, aRow )
   ENDDO

   // Clean up
   sqlite3_finalize( stmt )

RETURN iif( Empty( aData ), NIL, aData ) // Return NIL if no data, otherwise return array

/*
 * FUNCTION sqlite3_bind_exec()
 *
 * Executes a parameterized SQLite query with bound values.
 *
 * Parameters:
 *   db (handle): SQLite database connection handle.
 *   cSQL (string): SQL query with placeholders (e.g., :name or ?).
 *   aParams (array): Array of parameter values to bind (strings, numbers, or NIL).
 *
 * Returns:
 *   lSuccess (logical): .T. if execution succeeds and affects rows, .F. otherwise.
 *
 * Purpose:
 *   Safely executes SQL queries with bound parameters to prevent injection. Used for inserting records into the "people" table.
 *
 * Notes:
 *   Supports NIL, integer, float, and string parameters. Validates parameter count against placeholders. Finalizes statement to prevent resource leaks. Returns .T. only if rows are affected.
 */
FUNCTION sqlite3_bind_exec( db, cSQL, aParams )
   LOCAL stmt, i, nResult, nAffected := 0

   // Validate inputs
   IF db == NIL .OR. Empty( cSQL )
      RETURN .F. // Indicate failure
   ENDIF

   // Prepare SQL statement
   stmt := sqlite3_prepare( db, cSQL )
   IF stmt == NIL
      RETURN .F. // Preparation failed
   ENDIF

   // Validate parameter count
   IF ! Empty( aParams ) .AND. Len( aParams ) != sqlite3_bind_parameter_count( stmt )
      sqlite3_finalize( stmt )
      RETURN .F. // Mismatch in parameters
   ENDIF

   // Bind parameters
   IF ! Empty( aParams )
      FOR i := 1 TO Len( aParams )
         DO CASE
         CASE aParams[ i ] == NIL
            sqlite3_bind_null( stmt, i )
         CASE ValType( aParams[ i ] ) == "N"
            // Check if number is float by comparing with integer part
            IF aParams[ i ] != Int( aParams[ i ] )
               sqlite3_bind_double( stmt, i, aParams[ i ] )
            ELSE
               sqlite3_bind_int( stmt, i, aParams[ i ] )
            ENDIF
         CASE ValType( aParams[ i ] ) == "C"
            sqlite3_bind_text( stmt, i, aParams[ i ] )
         OTHERWISE
            sqlite3_finalize( stmt )
            RETURN .F. // Unsupported type
         ENDCASE
      NEXT
   ENDIF

   // Execute statement
   nResult := sqlite3_step( stmt )
   IF nResult == SQLITE_DONE
      nAffected := sqlite3_changes( db ) // Get affected rows
   ELSE
      sqlite3_finalize( stmt )
      RETURN .F. // Execution failed
   ENDIF

   // Clean up
   sqlite3_finalize( stmt )

RETURN ( nAffected > 0 ) // Return .T. if rows affected, .F. otherwise

/*
 * PROCEDURE ExportToCSV()
 *
 * Exports grid data to a CSV file (people.csv).
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Nil
 *
 * Purpose:
 *   Saves grid data (ID, Name, Age) to a CSV file for external use, such as reporting or sharing. Triggered by the "Export CSV" button.
 *
 * Notes:
 *   Overwrites existing people.csv file. Uses CRLF for Windows-compatible line endings. Assumes MainGrid contains data in the format [ID, Name, Age].
 */
PROCEDURE ExportToCSV()
   // Get grid data as an array
   LOCAL aData := DoMethod( "oWndMain", "MainGrid", "GetArray" ), i, cLine := ""
   // Add header row to CSV
   cLine += "ID,Name,Age" + CRLF
   // Build CSV rows from grid data
   FOR i := 1 TO Len( aData )
      cLine += aData[ i ][ 1 ] + "," + ;
         aData[ i ][ 2 ] + "," + ;
         aData[ i ][ 3 ]
      cLine += CRLF
   NEXT
   // Write CSV content to file
   hb_MemoWrit( "people.csv", cLine )
   // Confirm export to user
   MsgInfo( "Exported to people.csv" )

RETURN

/*
 * PROCEDURE ExportToXML()
 *
 * Exports grid data to an XML file (people.xml).
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Nil
 *
 * Purpose:
 *   Saves grid data (ID, Name, Age) to an XML file for structured data export. Triggered by the "Export XML" button.
 *
 * Notes:
 *   Overwrites existing people.xml file. Uses a simple XML structure with <People> root and <Person> tags. Assumes MainGrid contains data in the format [ID, Name, Age].
 */
PROCEDURE ExportToXML()
   // Get grid data as an array
   LOCAL aData := DoMethod( "oWndMain", "MainGrid", "GetArray" ), i, cXml := "<People>" + CRLF
   // Build XML structure for each record
   FOR i := 1 TO Len( aData )
      cXml += "  <Person>" + CRLF
      cXml += "    <ID>" + aData[ i ][ 1 ] + "</ID>" + CRLF
      cXml += "    <Name>" + aData[ i ][ 2 ] + "</Name>" + CRLF
      cXml += "    <Age>" + aData[ i ][ 3 ] + "</Age>" + CRLF
      cXml += "  </Person>" + CRLF
   NEXT
   cXml += "</People>"
   // Write XML content to file
   hb_MemoWrit( "people.xml", cXml )
   // Confirm export to user
   MsgInfo( "Exported to people.xml" )

RETURN