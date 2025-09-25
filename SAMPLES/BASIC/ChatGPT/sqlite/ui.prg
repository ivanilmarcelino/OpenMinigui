#include "minigui.ch"

/*
 * PROCEDURE InitGUI()
 *
 * Initializes the main application window with a tabbed interface, grid, and buttons.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Nil
 *
 * Purpose:
 *   Sets up the main UI with a tab control (Dashboard and Settings pages), a grid for data display,
 *   export buttons, and a save button. Calls FillGrid() on window activation for initial data load.
 *   Example: Called after login to display the main interface.
 *
 * Notes:
 *   Uses global g_cUsername for user display.
 *   Grid rows alternate colors for readability.
 *   Timer triggers auto-refresh every 5 seconds.
 *   Requires "appicon" icon file to avoid runtime errors.
 */
PROCEDURE InitGUI()

   // Declare local variable for username
   LOCAL g_cUsername
   // Define function for alternating grid row colors (white and light gray)
   LOCAL bColor := {| x, nItem | HB_SYMBOL_UNUSED( x ), iif( nItem / 2 == Int( nItem / 2 ), RGB( 255, 255, 255 ), RGB( 245, 245, 245 ) ) }

   // Create main window with specified size, title, and light purple background
   DEFINE WINDOW oWndMain ;
         AT 0, 0 HEIGHT 600 WIDTH 800 ;
         TITLE "Advanced HMG App" ;
         ICON "appicon" ;
         MAIN ;
         BACKCOLOR { 250, 250, 255 } ;
         FONT "Segoe UI" SIZE 10 ;
         ON INIT FillGrid()

      // Define tab control for Dashboard and Settings pages
      DEFINE TAB oTab AT 10, 10 WIDTH 760 HEIGHT 500

         // Begin Dashboard tab
         PAGE "Dashboard"
            // Display welcome message with username from global variable
            @ 30, 30 LABEL NUL VALUE "Welcome, " + ( g_cUsername := GetUserName() ) WIDTH 300 HEIGHT 24 BACKCOLOR { 250, 250, 255 }

            // Define grid to display people data with headers and alternating row colors
            DEFINE GRID MainGrid
               HEADERS { "ID", "Name", "Age" }
               WIDTHS { 50, 200, 50 }
               ITEMS {}
               WIDTH 440
               HEIGHT 200
               ROW 70
               COL 30
               DYNAMICBACKCOLOR { bColor, bColor, bColor }
            END GRID

            // Create button to export grid data to CSV
            @ 300, 30 BUTTONEX btnCSV CAPTION "Export CSV" WIDTH 130 HEIGHT 30 ;
               ACTION ExportToCSV() BACKCOLOR { 220, 220, 245 } FLAT NOXPSTYLE

            // Create button to export grid data to XML
            @ 300, 180 BUTTONEX btnXML CAPTION "Export XML" WIDTH 130 HEIGHT 30 ;
               ACTION ExportToXML() BACKCOLOR { 220, 220, 245 } FLAT NOXPSTYLE

         // End Dashboard tab
         END PAGE

         // Begin Settings tab
         PAGE "Settings"
            // Display username label
            @ 40, 30 LABEL NUL VALUE "Username:" WIDTH 80 HEIGHT 24 BACKCOLOR { 250, 250, 255 }
            // Create textbox for editing username
            @ 40, 120 TEXTBOX txtUser VALUE g_cUsername WIDTH 200 HEIGHT 24
         // End Settings tab
         END PAGE

      // End tab control
      END TAB

      // Create button to save username to configuration file
      DEFINE BUTTONEX btnSave
         CAPTION "Save Settings"
         ROW 520
         COL 530
         WIDTH 150
         HEIGHT 30
         ACTION SaveAppSettings()
         NOXPSTYLE .T.
         FLAT .T.
         BACKCOLOR { 240, 255, 240 }
      END BUTTONEX

      // Define timer to refresh grid every 5 seconds
      DEFINE TIMER t_1 INTERVAL 5000 ACTION OnAutoRefresh()

   // End window definition
   END WINDOW

   // Center main window on screen
   CENTER WINDOW oWndMain
   // Activate and display main window
   ACTIVATE WINDOW oWndMain

RETURN