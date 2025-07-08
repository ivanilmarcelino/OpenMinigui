/*
   HMG Rating Control Demo
   This MiniGUI application demonstrates the use of a rating control,
   allowing users to select a rating, modify its value, and toggle read-only mode.
   
   Features:
   - Displays the current rating.
   - Allows users to change the rating using a spinner.
   - Supports toggling read-only mode.
   - Shows the selected rating in a message box.
*/

#include "hmg.ch"

STATIC nRating := 3         // Current rating value
STATIC nMaxValue := 5       // Maximum rating value
STATIC lReadOnly := .F.     // Read-only state

FUNCTION Main

   // Define main window
   DEFINE WINDOW Main_Window AT 0, 0 WIDTH 340 HEIGHT 260 TITLE "HMG Rating Control Demo" MAIN ;
         ON RELEASE _ReleaseRating( 'Main_Window', 'Rating_Control' )

      // Display current rating label
      @ 50, 10 LABEL lblCurrent VALUE "Current Rating:" WIDTH 120 HEIGHT 20
      @ 50, 140 GETBOX Get_Rating VALUE nRating READONLY

      // Rating selection label and control
      @ 10, 10 LABEL lblTitle VALUE "Select Rating:" WIDTH 100 HEIGHT 20
      @ 10, 120 RATING Rating_Control ;
         WIDTH 25 ;
         HEIGHT 25 ;
         STARS nMaxValue ;
         RATE nRating ;
         FROM RESOURCE ;
         SPACING 1 ;
         ON CHANGE RatingChanged( Main_Window.Rating_Control.VALUE )

      // Spinner to set new rating
      @ 90, 10 LABEL lblMax VALUE "Set New Rate:" WIDTH 100 HEIGHT 20
      @ 90, 120 SPINNER Spin_Max RANGE 0, nMaxValue VALUE nRating ON CHANGE UpdateRating()

      // Checkbox to toggle read-only mode
      @ 130, 10 CHECKBOX chkReadOnly CAPTION "Read Only" VALUE lReadOnly ON CHANGE ToggleReadOnly()

      // Button to show rating in a message box
      @ 170, 10 BUTTON btnShow CAPTION "Show Rating" ACTION ShowRating()

   END WINDOW

   CENTER WINDOW Main_Window
   ACTIVATE WINDOW Main_Window

RETURN NIL

// Handles changes in the rating control
PROCEDURE RatingChanged( nNewRating )
   nRating := nNewRating
   Main_Window.Get_Rating.VALUE := nRating
RETURN

// Updates the rating value when the spinner changes, unless read-only
PROCEDURE UpdateRating()
   IF ! lReadOnly
      nRating := Main_Window.Spin_Max.VALUE
      Main_Window.Rating_Control.VALUE := nRating
      RefreshRating( 'Main_Window', 'Rating_Control' )
   ENDIF
RETURN

// Toggles read-only state for the rating control
PROCEDURE ToggleReadOnly()
   lReadOnly := ! lReadOnly

   ToggleRatingReadOnly( 'Main_Window', 'Rating_Control', lReadOnly )
   Main_Window.Spin_Max.ReadOnly := lReadOnly
RETURN

// Displays a message box with the current rating
PROCEDURE ShowRating()
   MsgInfo( "Current Rating: " + hb_ntos( nRating ), "Rating Info" )
RETURN
