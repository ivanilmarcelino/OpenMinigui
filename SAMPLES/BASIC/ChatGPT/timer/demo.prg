#include "minigui.ch"

function Main()
   // Initialize a local array containing employee data: Name, Age, and Occupation
   LOCAL aData := { ;
      { "John Doe", 28, "Engineer" }, ;
      { "Jane Smith", 34, "Manager" }, ;
      { "Sam Johnson", 25, "Intern" } }
   
   // Set the timer interval to 5000 milliseconds (5 seconds)
   LOCAL nTimerInterval := 5000

   // Define the main window for the application
   DEFINE WINDOW WinMain ;
      AT 0,0 ;                // Position the window at the top-left corner of the screen
      WIDTH 600 ;             // Set the window width to 600 pixels
      HEIGHT 550 ;            // Set the window height to 550 pixels
      TITLE "Real-Time Data Sync" ; // Set the window title
      MAIN                    // Indicate this is the main application window

   // Define a grid to display the employee data
   @ 20,20 GRID GrdEmployees ;
      WIDTH 550 ;                       // Set grid width to 550 pixels
      HEIGHT 400 ;                      // Set grid height to 400 pixels
      HEADERS { "Name", "Age", "Occupation" } ;  // Define the grid headers
      WIDTHS { 200, 50, 150 } ;         // Set the widths of each column
      ITEMS aData                       // Bind the grid to the aData array

   // Define a button to stop the real-time data synchronization (stops the timer)
   @ 450,180 BUTTON BtnStopSync ;
      CAPTION "Stop Sync" ;             // Set the button caption
      ACTION WinMain.Timer.Enabled := .F.  // Disable the timer when clicked, stopping updates

   // Define a button to start/resume the real-time data synchronization (starts the timer)
   @ 450,320 BUTTON BtnStartSync ;
      CAPTION "Start Sync" ;            // Set the button caption
      ACTION WinMain.Timer.Enabled := .T.  // Enable the timer when clicked, resuming updates

   // Define a timer that triggers data updates every 5 seconds
   DEFINE TIMER Timer ;
      INTERVAL nTimerInterval ;         // Set the interval between updates (5 seconds)
      ACTION UpdateGrid("GrdEmployees", @aData)  // Call UpdateGrid function to modify grid data

   END WINDOW

   // Center the window on the screen
   CENTER WINDOW WinMain

   // Activate the main window
   ACTIVATE WINDOW WinMain

return nil

// Function to update the grid with new data every time the timer triggers
function UpdateGrid(oGrid, aData)
   // Randomly select an update type (1 = Add a new entry, 2 = Edit an existing entry, 3 = Delete an entry)
   LOCAL nUpdateType := Max(1, RANDOM(3))  

   LOCAL nRow, cName, nAge, cOccupation  // Variables for data manipulation

   DO CASE
      CASE nUpdateType == 1
         // Add a new row of data (new employee)
         cName := "New Person " + LTRIM(STR(LEN(aData) + 1))  // Create a new name
         nAge := RANDOM(50) + 20                              // Assign a random age between 20 and 70
         cOccupation := "Occupation " + LTRIM(STR(LEN(aData) + 1))  // Create a generic occupation
         AADD(aData, { cName, nAge, cOccupation })            // Add the new row to the data array

      CASE nUpdateType == 2 .AND. LEN(aData) > 0
         // Edit an existing row (modify the age of a random employee)
         nRow := Max(1, RANDOM(LEN(aData)))                   // Randomly select a row to edit
         aData[nRow][2] := aData[nRow][2] + 1                 // Increment the age by 1 year

      CASE nUpdateType == 3 .AND. LEN(aData) > 0
         // Delete a random row from the grid (remove an employee)
         nRow := Max(1, RANDOM(LEN(aData)))                   // Randomly select a row to delete
         hb_ADEL(aData, nRow, .T.)                            // Remove the selected row and adjust the array
   ENDCASE

   // Update the grid with the modified data
   WinMain.(oGrid).SetArray(aData)    // Re-bind the grid to the updated array
   WinMain.(oGrid).Refresh()          // Refresh the grid to display the changes

   // Temporarily stop the timer, display a message, and restart the timer
   WinMain.Timer.Enabled := .F.       // Stop the timer while showing the message
   MsgInfo("Grid updated!")           // Display a message indicating that the grid was updated
   WinMain.Timer.Enabled := .T.       // Restart the timer after the message is dismissed

return nil
