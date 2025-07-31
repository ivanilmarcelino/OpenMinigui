#include "hmg.ch"
#include "hbclass.ch"

/*
 * PROCEDURE Main()
 *
 * Initializes the main application window and creates two custom progress bar controls.
 *
 * Purpose:
 *   This is the entry point of the application. It defines the main window, creates two instances of the MyProgressbar class,
 *   positions them within the window, and adds a button that triggers the DoProgress procedure to update the progress bars.
 *   The purpose is to demonstrate the usage of the custom MyProgressbar control within an HMG Extended application.
 *   This allows developers to see how to integrate custom controls into their HMG applications.
 *
 * Notes:
 *   The MyProgressbar class is defined later in the code.
 *   The DoProgress procedure is responsible for incrementing the progress bar values.
 */
PROCEDURE Main()

   LOCAL oProgressBar1, oProgressBar2
   LOCAL nButtonRow := 30
   LOCAL nProgressBarRow1 := 80
   LOCAL nProgressBarRow2 := 160
   LOCAL nProgressBarCol := 10
   LOCAL nProgressBarWidth := 500
   LOCAL nProgressBarHeight := 50

   DEFINE WINDOW Main ROW 0 COL 0 WIDTH 600 HEIGHT 400 TITLE "Custom Progress Bar Control" WINDOWTYPE MAIN

      oProgressBar1 := MyProgressbar():New()
      WITH OBJECT oProgressBar1
         :SetPos( nProgressBarRow1, nProgressBarCol, nProgressBarWidth, nProgressBarHeight )
         :aBackColor := YELLOW
         :Create()
      ENDWITH

      oProgressBar2 := MyProgressbar():New()
      WITH OBJECT oProgressBar2
         :cCaption := "testing"
         :SetPos( nProgressBarRow2, nProgressBarCol, nProgressBarWidth, nProgressBarHeight )
         :aBackColor := BLUE
         :Create()
      ENDWITH

      DEFINE BUTTON Button_1
         PARENT Main
         ROW nButtonRow
         COL nProgressBarCol
         CAPTION 'Click Me!'
         ACTION DoProgress( oProgressBar1, oProgressBar2 )
      END BUTTON

   END WINDOW

   CENTER WINDOW Main
   ACTIVATE WINDOW Main

RETURN

/*
 * PROCEDURE DoProgress(oControl1, oControl2)
 *
 * Increments the value of two MyProgressbar objects until they reach their maximum value.
 *
 * Parameters:
 *   oControl1 (OBJECT): The first MyProgressbar object to update.
 *   oControl2 (OBJECT): The second MyProgressbar object to update.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This procedure is called when the "Click Me!" button is pressed. It iteratively increases the nValue property of the two
 *   MyProgressbar objects (oControl1 and oControl2) by 10 in each iteration until the value reaches the maximum value (nMax).
 *   It also calls the SetPercentage() method to update the percentage display and uses InkeyGUI(100) to introduce a small delay,
 *   allowing the user to visually observe the progress bar updates. This demonstrates how to dynamically update a custom control
 *   based on user interaction.
 *
 * Notes:
 *   The InkeyGUI(100) function pauses the execution for 100 milliseconds, preventing the loop from running too quickly and
 *   allowing the UI to update.
 */
PROCEDURE DoProgress( oControl1, oControl2 )

   LOCAL i

   IF oControl1:nValue == oControl1:nMax
      oControl1:Refresh()
      oControl2:Refresh()
   ENDIF

   FOR i := oControl1:nValue TO oControl1:nMax STEP 10
      oControl1:SetValue( i )
      oControl1:SetPercentage()
      oControl2:SetValue( i )
      InkeyGUI( 100 )
   NEXT

RETURN

/*
 * CLASS MyProgressbar
 *
 * Defines a custom progress bar control.
 *
 * Instance Variables:
 *   cName (STRING): The unique name of the window containing the progress bar.
 *   nTop (NUMERIC): The top position of the progress bar within its parent window.
 *   nLeft (NUMERIC): The left position of the progress bar within its parent window.
 *   nWidth (NUMERIC): The width of the progress bar.
 *   nHeight (NUMERIC): The height of the progress bar.
 *   cCaption (STRING): The caption displayed on the progress bar.
 *   aBackColor (COLOR): The background color of the progress bar.
 *   nMax (NUMERIC): The maximum value of the progress bar (default: calculated from width and column).
 *   nValue (NUMERIC): The current value of the progress bar (default: 0).
 *   cParent (STRING): The name of the parent window.
 *
 * Methods:
 *   New(): Constructor for the class.
 *   Create(): Creates the progress bar window and label.
 *   Refresh(): Resets the progress bar to zero and redraws it.
 *   SetPos( nRow, nCol, nW, nH ): Sets the position and size of the progress bar.
 *   SetValue( nVal ): Sets the current value of the progress bar.
 *   SetPercentage(): Updates the percentage display on the progress bar.
 *   Paint(): Updates the width of the progress bar label to visually represent the progress.
 *
 * Purpose:
 *   This class encapsulates the logic for creating and managing a custom progress bar control. It provides methods for setting the position,
 *   value, and percentage display of the progress bar. The Paint() method is responsible for visually updating the progress bar by
 *   adjusting the width of the label based on the current value. This allows developers to easily reuse a progress bar component
 *   in different parts of their application.
 *
 * Notes:
 *   The progress bar is implemented using a window containing a label. The width of the label is adjusted to represent the progress.
 *   The nMax variable defines the maximum value that the progress bar can reach. It is calculated based on the width and left position.
 */
CREATE CLASS MyProgressbar

   VAR cName         // Unique name of the progress bar window
   VAR nTop          // Top position
   VAR nLeft         // Left position
   VAR nWidth        // Width of the progress bar
   VAR nHeight       // Height of the progress bar
   VAR cCaption      // Caption text
   VAR aBackColor    // Background color
   VAR nMax          // Maximum value
   VAR nValue INIT 0 // Current progress value
   VAR cParent       // Parent window name

   METHOD Create()
   METHOD Refresh()
   METHOD SetPos( nRow, nCol, nW, nH ) INLINE ::nTop := nRow, ::nLeft := nCol, ::nWidth := nW, ::nHeight := nH
   METHOD SetValue( nVal )
   METHOD SetPercentage() INLINE SetProperty( ::cName, "label_1", "value", hb_ntos( ::nValue / ::nMax * 100 ) + " %" )
   METHOD Paint()

ENDCLASS

/*
 * METHOD Create()
 *
 * Creates the window and label that make up the progress bar control.
 *
 * Purpose:
 *   This method is responsible for creating the HMG window and label controls that visually represent the progress bar.
 *   It dynamically generates a unique name for the window using HMG_GetUniqueName(). The window is created as a PANEL type.
 *   A label is then created within the window, which will be used to display the progress bar and percentage.
 *   The use of a PANEL window allows the label to be contained within a defined area.
 *   The maximum value (nMax) of the progress bar is calculated based on the width of the progress bar and its left position.
 *   This ensures that the progress bar fills the available space within the window.
 *
 * Notes:
 *   The ::cName variable stores the unique name of the window, which is used to reference the window later.
 *   The label's width is initially set to 1, and its width is dynamically updated in the Paint() method to reflect the progress.
 */
METHOD Create() CLASS MyProgressbar

   // Calculate the maximum value based on the width, ensuring it's not negative
   DEFAULT ::nMax TO Max( 0, ::nWidth - ::nLeft )

   ::cParent := ThisWindow.Name
   ::cName := HMG_GetUniqueName( "W" )

   DEFINE WINDOW ( ::cName ) ;
         PARENT ( ::cParent ) ;
         ROW ::nTop ;
         COL ::nLeft ;
         WIDTH ::nWidth ;
         HEIGHT ::nHeight ;
         WINDOWTYPE PANEL

      DEFINE LABEL Label_1
         ROW 5
         COL 5
         VALUE ::cCaption
         FONTSIZE 12
         BORDER .T.
         WIDTH 1
         HEIGHT ::nHeight - 15
         BACKCOLOR ::aBackColor
         CENTERALIGN .T.
         VCENTERALIGN .T.
      END LABEL

   END WINDOW

RETURN NIL

/*
 * METHOD Refresh()
 *
 * Hides the progress bar window, resets the progress value to zero, and then shows the window again.
 *
 * Purpose:
 *   This method provides a way to reset the progress bar to its initial state. It first hides the window to prevent visual artifacts
 *   during the reset process. Then, it sets the nValue property to 0, effectively resetting the progress. Finally, it shows the window
 *   again, making the reset progress bar visible. This is useful when you need to reuse the progress bar for a new task.
 *
 * Notes:
 *   Hiding the window before resetting the value ensures a smooth visual transition.
 */
METHOD Refresh() CLASS MyProgressbar

   DoMethod( ::cName, "Hide" )
   ::nValue := 0
   ::Paint()

   SHOW WINDOW ( ::cName )

RETURN NIL

/*
 * METHOD SetValue( nVal )
 *
 * Sets the current value of the progress bar, ensuring it does not exceed the maximum value.
 *
 * Parameters:
 *   nVal (NUMERIC): The new value to set for the progress bar.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This method updates the nValue property of the MyProgressbar object. It ensures that the new value (nVal) does not exceed the maximum
 *   value (nMax) of the progress bar. If nVal is greater than nMax, nValue is set to nMax. The Paint() method is then called to
 *   update the visual representation of the progress bar. This method provides a controlled way to update the progress bar's value.
 *
 * Notes:
 *   The iif() function is used to conditionally set the new value, ensuring it stays within the valid range.
 *   The Paint() method is called only if the new value is different from the current value to avoid unnecessary UI updates.
 */
METHOD SetValue( nVal ) CLASS MyProgressbar

   LOCAL nNewValue := iif( nVal >= ::nMax, ::nMax, nVal )

   IF nNewValue != ::nValue
      ::nValue := nNewValue
      ::Paint()
   ENDIF

RETURN NIL

/*
 * METHOD Paint()
 *
 * Updates the width of the label to visually represent the progress bar's current value.
 *
 * Purpose:
 *   This method is called to update the visual representation of the progress bar. It sets the width of the label control
 *   (named "label_1" within the progress bar's window) to the current value of ::nValue. This effectively makes the label
 *   grow or shrink, visually indicating the progress. The width of the label is directly proportional to the current value,
 *   providing a clear visual representation of the progress.
 *
 * Notes:
 *   The SetProperty() function is used to modify the "width" property of the label control.
 *   The width of the label is directly proportional to the current value of the progress bar.
 */
METHOD Paint() CLASS MyProgressbar

   SetProperty( ::cName, "label_1", "width", ::nValue )

RETURN NIL
