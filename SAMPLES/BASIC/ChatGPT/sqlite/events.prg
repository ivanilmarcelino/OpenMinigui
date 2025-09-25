#include "minigui.ch"
#include "hbsqlit3.ch"

/*
 * PROCEDURE OnAutoRefresh()
 *
 * Refreshes the grid data periodically.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Nil
 *
 * Purpose:
 *   Updates the grid with the latest database data every 5 seconds via a timer.
 *   Example: Automatically triggered by the timer in the main window.
 *
 * Notes:
 *   Logs the refresh time to the console.
 *   Calls FillGrid() to update the grid.
 */
PROCEDURE OnAutoRefresh()
   // Log current time to console
   ? "Auto-refresh at: ", Time()
   // Refresh grid data
   FillGrid()

RETURN

/*
 * PROCEDURE FillGrid()
 *
 * Populates the main grid with data from the SQLite database.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Nil
 *
 * Purpose:
 *   Retrieves data from the people table in people.db and updates the grid.
 *   Example: Called on window initialization and auto-refresh.
 *
 * Notes:
 *   Initializes database with sample data if people.db is missing.
 */
PROCEDURE FillGrid()
   // Initialize data array and database variables
   LOCAL aData, hDb
   // Check if database file exists
   IF ! File( "people.db" )
      // Create and populate database if missing
      InitSQLiteDB()
   ENDIF
   // Open SQLite database
   hDb := sqlite3_open( "people.db" )
   // SQL query to select all columns from people table
   aData := sqlite3_fetch_all( hDb, "SELECT id, name, age FROM people" )
   // Close database to free resources
   hDb := NIL
   // Update grid with retrieved data
   DoMethod( "oWndMain", "MainGrid", "SetArray", aData )

RETURN
