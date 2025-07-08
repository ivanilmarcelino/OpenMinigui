/*
 * HMG Timer demo
 * (c) 2010 Roberto Lopez <mail.box.hmg@gmail.com>
 * This example demonstrates the usage of timers in an HMG application.
*/

#include "hmg.ch"   // Include HMG library for GUI components

STATIC lChange := .T.  // Static variable to track the toggle state for labels

FUNCTION Main

   // Define the main window
   DEFINE WINDOW oWindow;
      ROW    10;               // Window top position
      COL    10;               // Window left position
      WIDTH  500;              // Window width
      HEIGHT 550;              // Window height
      TITLE  'HMG Timer Demo'; // Window title
      WINDOWTYPE MAIN;         // Main window type
      ONINIT { || oWindow.Center(), ShowStatus() }  // On initialization, center window and show status

      // Define the first label (empty by default)
      @ 20 , 10 LABEL oLabel1 ;
         VALUE '' ;
         AUTOSIZE ;            // Adjust size based on text
         FONT "Arial" ;        // Font type
         SIZE 24               // Font size

      // Define the second label (empty by default)
      @ 20, 250 LABEL oLabel2 ;
         AUTOSIZE ;
         VALUE '' ;
         FONT "Arial" ;
         SIZE 24

      // Define an editbox to display timer status
      @ 230, 10 EDITBOX oEdit1 ;
         WIDTH 480 ;           // Editbox width
         HEIGHT 300 ;          // Editbox height
         SIZE 12 ;             // Font size
         VALUE ''              // Initial value is empty

      // Define a button to stop the timer
      DEFINE BUTTON oButton1
         ROW     90
         COL     10
         CAPTION 'Stop'
         ONCLICK ( oWindow.oTimerBlink.Enabled := .F., ShowStatus() )  // Disable timer and show status
      END BUTTON

      // Define a button to start the timer
      DEFINE BUTTON oButton2
         ROW     90
         COL     250
         CAPTION 'Start'
         ONCLICK ( oWindow.oTimerBlink.Enabled := .T., ShowStatus() )  // Enable timer and show status
      END BUTTON

      // Define a button to set timer interval to 700 milliseconds
      DEFINE BUTTON oButton3
         ROW     120
         COL     10
         CAPTION 'Interval 700'
         ONCLICK ( oWindow.oTimerBlink.Interval := 700, ShowStatus() )  // Set interval to 700 ms and show status
      END BUTTON

      // Define a button to set timer interval to 250 milliseconds
      DEFINE BUTTON oButton4
         ROW     120
         COL     250
         CAPTION 'Interval 250'
         ONCLICK ( oWindow.oTimerBlink.Interval := 250, ShowStatus() )  // Set interval to 250 ms and show status
      END BUTTON

      // Define a button to change timer action to `testtimer2()`
      DEFINE BUTTON oButton5
         ROW     150
         COL     10
         CAPTION 'Action 2'
         ONCLICK ( oWindow.oTimerBlink.Action := { || testtimer2(), ShowStatus() } )  // Change timer action and show status
      END BUTTON

      // Define a button to change timer action to `testtimer1()`
      DEFINE BUTTON oButton6
         ROW     150
         COL     250
         CAPTION 'Action 1'
         ONCLICK ( oWindow.oTimerBlink.Action := { || testtimer1(), ShowStatus() } )  // Change timer action and show status
      END BUTTON

      // Define a button to toggle the "One Shot" property of the timer
      DEFINE BUTTON oButton7
         ROW     180
         COL     10
         CAPTION 'Toggle One Shot'
         ONCLICK ( oWindow.oTimerBlink.Once := ! (oWindow.oTimerBlink.Once), ShowStatus() )  // Toggle 'Once' property and show status
      END BUTTON

     // Define the main timer `oTimerBlink` that blinks the label
     DEFINE TIMER oTimerBlink
        INTERVAL 250                  // Timer interval (in milliseconds)
        ACTION   { || testtimer1() }  // Timer action (calls `testtimer1`)
     END TIMER

     // Define a secondary timer `oTimerColor` that changes the label's font color
     DEFINE TIMER oTimerColor
        INTERVAL 4000                 // Timer interval set to 4 seconds
        ONCE     .T.                  // This timer runs only once
        ACTION   { || oWindow.oLabel1.FontColor := { 255, 0, 0 }, ShowStatus() }  // Change label1 font color to red
     END TIMER

   END WINDOW

   ACTIVATE WINDOW oWindow  // Activate the window and start the event loop

   RETURN NIL  // Return nil to exit the function

/*----------------------------------------------------------------------*/

// Function to handle the second timer action, toggles the content of Label 2
FUNCTION testtimer2()

   lchange := ! lchange  // Toggle the state of `lChange`

   oWindow.oLabel2.Value := IF( lchange, ':)', ':D' )  // Alternate between ':)' and ':D'

   RETURN 0

/*----------------------------------------------------------------------*/

// Function to handle the main timer action, toggles the content of Label 1
FUNCTION testtimer1()

   lchange := ! lchange  // Toggle the state of `lChange`

   oWindow.oLabel1.Value := IF( lchange, 'HMG is great', '' )  // Alternate between showing 'HMG is great' and empty string

   RETURN 0

/*----------------------------------------------------------------------*/

// Function to show the status of both timers in the EditBox
FUNCTION ShowStatus()

   DoEvents()  // Process any pending events

   oWindow.oEdit1.Value := 'oTimerBlink  ---------' + HB_EOL() +;  // Show the status of `oTimerBlink`
                           '  Interval = ' + hb_NToS( oWindow.oTimerBlink.Interval ) + HB_EOL() +;  // Show interval
                           '  One Shot = ' + HB_ValToSTR( oWindow.oTimerBlink.Once ) + HB_EOL() +;  // Show 'One Shot' status
                           '  IsActive = ' + HB_ValToSTR( oWindow.oTimerBlink.Enabled ) + HB_EOL() +;  // Show if timer is active
                           '  Action = ' + cValToChar( GetProperty( 'oWindow', 'oTimerBlink', 'Action' ) ) + HB_EOL() + HB_EOL() +;
                           'oTimerColor  ---------' + HB_EOL() +;  // Show the status of `oTimerColor`
                           '  Interval = ' + hb_NToS( oWindow.oTimerColor.Interval ) + HB_EOL() +;  // Show interval
                           '  One Shot = ' + HB_ValToSTR( oWindow.oTimerColor.Once ) + HB_EOL() +;  // Show 'One Shot' status
                           '  IsActive = ' + HB_ValToSTR( oWindow.oTimerColor.Enabled ) + HB_EOL() +;  // Show if timer is active
                           '  Action = ' + cValToChar( GetProperty( 'oWindow', 'oTimerColor', 'Action' ) )  // Show action

   RETURN 0  // Return 0 to exit the function
