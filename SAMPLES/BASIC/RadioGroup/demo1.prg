/*
 * HMG RadioGroup Demo
 * (c) 2010 Roberto Lopez <mail.box.hmg@gmail.com>
 * This demo demonstrates how to use and manipulate RadioGroup controls in HMG.
 */

#include "minigui.ch"

FUNCTION Main()

   // Define the main window (Win1)
   DEFINE WINDOW Win1 ;
         ROW 10 ;                          // Row position of the window
         COL 10 ;                          // Column position of the window
         WIDTH 400 ;                       // Set the window width
         HEIGHT 400 ;                      // Set the window height
         TITLE 'HMG RadioGroup Demo' ;     // Window title
         WINDOWTYPE MAIN ;                 // Define the window as a main window
         ON INIT Win1.Center()             // Center the window when initialized

      // Define the main menu with several options to manipulate RadioGroup2
      DEFINE MAIN MENU
         DEFINE POPUP "&Properties"        // Popup menu titled "Properties"

            // Menu item to change the value of RadioGroup2 to the third option (index 3)
            MENUITEM "Change Value" ACTION Win1.RadioGroup2.Value := 3

            // Menu item to display the current value of RadioGroup2 in a message box
            MENUITEM "Get Value" ACTION Msginfo( Win1.RadioGroup2.Value )

            SEPARATOR  // Add a separator line in the menu

            // Menu item to change the options of RadioGroup2 to new items
            MENUITEM "Change Options" ACTION Win1.RadioGroup2.Options := { "New Item 1", "New Item 2", "New Item 3", "New Item 4" }

            // Menu item to display the current options of RadioGroup2 using debug output
            MENUITEM "Get Options value" ACTION MsgDebug( Win1.RadioGroup2.Options )

            SEPARATOR  // Add another separator line

            // Menu item to change the spacing between the radio buttons of RadioGroup2 to 32
            // If RadioGroup2 is horizontal, it adjusts the window width to 450
            MENUITEM "Change Spacing" ACTION ( SetProperty( 'Win1', 'RadioGroup2', 'Spacing', 32 ), iif( Win1.RadioGroup2.Horizontal, Win1.Width := 450, ) )

            // Menu item to show the current spacing value of RadioGroup2 in a message box
            MENUITEM "Get Spacing value" ACTION Msginfo( Win1.RadioGroup2.Spacing, 'RadioGroup2 Spacing' )

            SEPARATOR  // Another separator

            // Menu item to set the orientation of RadioGroup2 to horizontal
            MENUITEM "Set Horizontal orientation" ACTION SetHorizontal( 'RadioGroup2', 'Win1' )

            // Menu item to display whether RadioGroup2 is currently in horizontal orientation
            MENUITEM "Get Horizontal style" ACTION Msginfo( Win1.RadioGroup2.Horizontal, 'Is Horizontal?' )
         END POPUP
      END MENU

      // Define the first RadioGroup (horizontal orientation)
      @ 40, 10 RADIOGROUP RadioGroup1 ;
         OPTIONS { "Item 1", "Item 2", "Item 3" } ;  // Radio button options
         WIDTH 60 ;                                  // Set the width of the group
         SPACING 20 ;                                // Set spacing between buttons
         VALUE 2 ;                                   // Set the default selected option (second one)
         HORIZONTAL ;                                // Arrange buttons horizontally
         TOOLTIP 'Horizontal Radiogroup' ;           // Tooltip text for the group
         ON CHANGE MsgInfo( "Radiogroup 1 Value Changed!" )  // Display message when value changes

      // Define the second RadioGroup (vertical orientation by default)
      @ 110, 10 RADIOGROUP Radiogroup2 ;
         OPTIONS { "Option 1", "Option 2", "Option 3", "Option 4" } ;  // Set radio button options
         WIDTH 240 ;                                                   // Set the width of the group
         TOOLTIP 'Vertical Radiogroup' ;                               // Tooltip text for the group
         ON CHANGE {|| MsgInfo( "Radiogroup 2 Value Changed!" ) }      // Show message when value changes

   END WINDOW

   ACTIVATE WINDOW Win1  // Activate and display the window

RETURN NIL


// Procedure to set a RadioGroup control to horizontal orientation
PROCEDURE SetHorizontal( control, form )

   // Get the current options and value of the RadioGroup control
   LOCAL aoptions := GetProperty( Form, Control, 'options' )
   LOCAL nvalue := GetProperty( Form, Control, 'value' )

   // Check if the RadioGroup is not already horizontal
   IF ! GetProperty( Form, Control, 'horizontal' )
      // Release the current control and redraw the interface
      DoMethod( Form, Control, 'release' )
      DO EVENTS  // Process pending events

      // Recreate the RadioGroup in horizontal orientation with the same options and value
      @ 110, 10 RADIOGROUP (Control) OF (form) ;
         OPTIONS aoptions ;        // Set the options to the previously stored ones
         HORIZONTAL ;              // Set horizontal orientation
         WIDTH 80 ;                // Set the width of the group
         SPACING 12 ;              // Set spacing between buttons
         VALUE nvalue ;            // Keep the current selected option
         TOOLTIP 'Horizontal Radiogroup' ;  // Tooltip text for the group
         ON CHANGE {|| MsgInfo( "Radiogroup 2 Value Changed!" ) }  // Show message when value changes
   ENDIF

RETURN
