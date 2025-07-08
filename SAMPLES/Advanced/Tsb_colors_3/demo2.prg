/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "minigui.ch"
#include "tsbrowse.ch"

function Main()

   local aData := FillArray()        // Populate a data array with random values.
   local oBrw                        // Declare a TBROWSE object (grid).
   local nCnt := 1                   // Initialize theme counter.

   // Define and create the main window.
   DEFINE WINDOW oDlg TITLE "Data Grid Example" ;
      ROW 100 COL 100 CLIENTAREA 470,410 ;    // Set position and size of the window.
      MAIN NOMAXIMIZE NOSIZE ON INIT This.Center() // Set window options and center on screen.

      ON KEY ESCAPE ACTION ThisWindow.Release()  // Exit window when 'Escape' key is pressed.
   END WINDOW

   // Define a toolbar at the top of the window.
   DEFINE TOOLBAR oBar OF oDlg BUTTONSIZE 460,40  // Set toolbar size.
   BUTTON oBtn1 CAPTION "Change Theme" ACTION ( nCnt := iif(++nCnt <= 5, nCnt, 1), SetTbrowseTheme( oBrw, nCnt ) )
   END TOOLBAR

   // Define the TBROWSE (data grid) for displaying the array data.
   @ 50, 5 TBROWSE oBrw WIDTH 460 HEIGHT 342 OF oDlg ;  // Set position and size of TBROWSE.
      HEADERS "Puntata", "Costo giocata", "Spesa", "Vincita", "Utile", "Utile%" ;  // Set column headers.
      ARRAY aData SELECTOR .T.  // Use aData for the grid and allow row selection.

   // Configure the TBROWSE columns.
   oBrw:aColSizes := {70, 70, 70, 70, 70, 70}  // Set column widths for each header.
   oBrw:nHeightCell := 20  // Set row height.
   oBrw:nHeightHead := 20  // Set header height.
   oBrw:bLogicLen := { || Len( aData ) }  // Set total number of columns dynamically.
   oBrw:lNoHScroll := .T.  // Disable horizontal scrolling.

   SetTbrowseTheme( oBrw, nCnt )  // Apply theme; initially set to the first theme (Office 2003).

   // Define an action when a row is double-clicked.
   oBrw:bLDblClick := { |nRow, nCol, nFlag, oBrw| MsgInfo( "Clicked row: " + hb_ntos( oBrw:nAt ) ) }  // Show the row number.

   oBrw:ResetVScroll( .T. )  // Reset vertical scrolling behavior.
   oBrw:AdjColumns( .T. )    // Adjust columns to fit data without padding.
   oBrw:SetNoHoles()         // Ensure no empty gaps between rows.
   oBrw:nColPos := 1         // Start with the first column selected.

   // Display the window.
   ACTIVATE WINDOW oDlg

return nil

/* Theme Color Details

1. Office 2003 Theme
   - Background: Light gray RGB(235, 235, 235)
   - Text: Black RGB(0, 0, 0)
   - Header Background: Slate blue RGB(102, 153, 255)
   - Header Text: White RGB(255, 255, 255)
   - Selected Row: Light blue RGB(204, 229, 255)
   - Grid Lines: Medium gray RGB(210, 210, 210)

2. Office 2010 Theme
   - Background: Very light gray RGB(245, 245, 245)
   - Text: Dark gray RGB(40, 40, 40)
   - Header Background: Soft blue RGB(166, 206, 227)
   - Header Text: Dark blue RGB(45, 80, 150)
   - Selected Row: Light azure RGB(230, 242, 255)
   - Grid Lines: Light silver RGB(220, 220, 220)

3. Office 2015 Theme
   - Background: Warm light gray RGB(240, 240, 240)
   - Text: Dark gray RGB(45, 45, 45)
   - Header Background: Flat dark blue RGB(0, 120, 215)
   - Header Text: White RGB(255, 255, 255)
   - Selected Row: Light blue-gray RGB(215, 235, 255)
   - Grid Lines: Medium-light gray RGB(210, 210, 210)

4. Office 2022 Theme
   - Background: Soft neutral gray RGB(248, 248, 248)
   - Text: Charcoal gray RGB(30, 30, 30)
   - Header Background: Cool blue RGB(85, 170, 255)
   - Header Text: White RGB(255, 255, 255)
   - Selected Row: Light aqua RGB(225, 245, 255)
   - Grid Lines: Light gray RGB(230, 230, 230)

5. Office 2024 Theme
   - Background: Off-white RGB(250, 250, 250)
   - Text: Dark slate gray RGB(50, 50, 50)
   - Header Background: Deep blue RGB(70, 130, 180)
   - Header Text: White RGB(255, 255, 255)
   - Selected Row: Pale lavender RGB(230, 240, 255)
   - Grid Lines: Light gray RGB(225, 225, 225)

*/

function SetTbrowseTheme( oBrw, nTheme )

   local aThemes := Array(4)  // Declare an array to store the color theme settings.

   // Select the color theme based on the nTheme argument.
   switch nTheme
      case 1  // Office 2003 Theme
         oDlg.Title := "Data Grid Example - Office 2003 Theme"

         aThemes := { { RGB(0, 0, 0), RGB(235, 235, 235) }, ;
                      { RGB(0, 0, 0), RGB(204, 229, 255) }, ;
                      { RGB(210, 210, 210), LINES_HORZ }, ;
                      { RGB(255, 255, 255), RGB(102, 153, 255) } }
         exit

      case 2  // Office 2010 Theme
         oDlg.Title := "Data Grid Example - Office 2010 Theme"

         aThemes := { { RGB(60, 60, 60), RGB(245, 245, 245) }, ;
                      { RGB(60, 60, 60), RGB(230, 242, 255) }, ;
                      { RGB(220, 220, 220), LINES_HORZ }, ;
                      { RGB(45, 80, 150), RGB(166, 206, 227) } }
         exit

      case 3  // Office 2015 Theme
         oDlg.Title := "Data Grid Example - Office 2015 Theme"

         aThemes := { { RGB(45, 45, 45), RGB(240, 240, 240) }, ;
                      { RGB(45, 45, 45), RGB(215, 235, 255) }, ;
                      { RGB(210, 210, 210), LINES_HORZ }, ;
                      { RGB(255, 255, 255), RGB(0, 120, 215) } }
         exit

      case 4  // Office 2022 Theme
         oDlg.Title := "Data Grid Example - Office 2022 Theme"

         aThemes := { { RGB(30, 30, 30), RGB(248, 248, 248) }, ;
                      { RGB(30, 30, 30), RGB(225, 245, 255) }, ;
                      { RGB(230, 230, 230), LINES_HORZ }, ;
                      { RGB(255, 255, 255), RGB(85, 170, 255) } }
         exit

      case 5  // Office 2024 Theme
         oDlg.Title := "Data Grid Example - Office 2024 Theme"

         aThemes := { { RGB(50, 50, 50), RGB(250, 250, 250) }, ;
                      { RGB(50, 50, 50), RGB(230, 240, 255) }, ;
                      { RGB(225, 225, 225), LINES_HORZ }, ;
                      { RGB(255, 255, 255), RGB(70, 130, 180) } }

   end switch

   // Apply the selected theme colors to the TBROWSE object
   with object oBrw
      :SetColor( { CLR_TEXT, CLR_PANE }, aThemes[1] )
      :SetColor( { CLR_FOCUSF, CLR_FOCUSB }, aThemes[2] )
      :nClrLine   := aThemes[3][1]           // Set grid line color.
      :nLineStyle := aThemes[3][2]           // Set line style (horizontal or vertical).
      :nClrHeadFore := aThemes[4][1]         // Set header text color.
      :nClrHeadBack := aThemes[4][2]         // Set header background color.
      :SetColor( { CLR_HEADF, CLR_HEADB }, { aThemes[4][1], aThemes[4][2] } )

      // Add ARRAYNO column if not already present, to show row numbers.
      if :nColumn( "ARRAYNO", .T. ) == 0
         :InsColNumber( 34, 1 )              // Insert column showing row numbers.
      endif

      // Set header background color for the selector
      :GetColumn( "SELECTOR" ):nClrBack := aThemes[4][2]

      // Set specific colors for the "ARRAYNO" column when it has focus
      :GetColumn( "ARRAYNO" ):nClrBack := aThemes[1][2]      // Background color
      :GetColumn( "ARRAYNO" ):nClrFocuFore := aThemes[2][1]  // Text color for focused rows.
      :GetColumn( "ARRAYNO" ):nClrFocuBack := aThemes[2][2]  // Background for focused rows.

   end with

return nil

// Function to populate a data array with random floating-point values.
static function FillArray()

   local aData := Array( 16, 6 )   // Create an array with 16 rows and 6 columns.
   local aRow

   // Fill each cell of the array with random data.
   for each aRow in aData
      AEval( aRow, { |u,i| aRow[ i ] := HB_RandomInt( 1,9999 ) * 0.01 } )  // Generate random values.
   next

return aData
