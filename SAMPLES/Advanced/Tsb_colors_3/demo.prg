/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "minigui.ch"
#include "tsbrowse.ch"

function Main()

   local aData := FillArray()        // Populate a data array with random values.
   local oBrw                        // Declare a TBROWSE object (grid).
   local aThemes := { 1, 2, 3, 4, 5, 6, 7, 8, 10, 11 } // Array of theme options.
   local nCnt := 1                   // Theme counter to track current theme index.

   // Define and create the main window.
   DEFINE WINDOW oDlg TITLE "Data Grid Example" ;
      ROW 100 COL 100 CLIENTAREA 470,410 ;    // Set position and size of the window.
      MAIN NOMAXIMIZE NOSIZE ON INIT This.Center() // Set window options and center on screen.

      ON KEY ESCAPE ACTION ThisWindow.Release()  // Exit window when 'Escape' key is pressed.
   END WINDOW

   // Define a toolbar at the top of the window.
   DEFINE TOOLBAR oBar OF oDlg BUTTONSIZE 460,40  // Set toolbar size.

   // Add a button on the toolbar to change the theme.
   BUTTON oBtn1 CAPTION "Change Theme" ACTION ( nCnt := iif(++nCnt <= Len( aThemes ), nCnt, 1), SetTbrowseTheme( oBrw, aThemes[nCnt] ) )

   END TOOLBAR

   // Define the TBROWSE (data grid) for displaying the array data.
   @ 50, 5 TBROWSE oBrw WIDTH 460 HEIGHT 344 OF oDlg ;  // Set position and size of TBROWSE.
      HEADERS "Puntata", "Costo giocata", "Spesa", "Vincita", "Utile", "Utile%" ;  // Set column headers.
      ARRAY aData SELECTOR .T.  // Use aData for the grid and allow row selection.

   // Configure the TBROWSE columns.
   oBrw:aColSizes := {70, 70, 70, 70, 70, 70}  // Set column widths for each header.
   oBrw:nHeightCell := 20  // Set row height.
   oBrw:nHeightHead := 22  // Set header height.
   oBrw:bLogicLen := { || Len( aData ) }  // Set total number of rows dynamically.
   oBrw:lNoHScroll := .T.  // Disable horizontal scrolling.

   // Set Windows theme colors and behavior for the TBROWSE.
   SetTbrowseTheme( oBrw, nCnt )

   // Define an action when a row is double-clicked.
   oBrw:bLDblClick := { |nRow, nCol, nFlag, oBrw| MsgInfo( "Clicked row: " + hb_ntos( oBrw:nAt ) ) }  // Display clicked row number.

   oBrw:ResetVScroll( .T. )  // Reset vertical scrolling behavior.
   oBrw:AdjColumns( .T. )    // Adjust columns to fit data without padding.
   oBrw:SetNoHoles()         // Ensure no empty gaps between rows.
   oBrw:nColPos := 1         // Set initial selected column.

   // Display the window.
   ACTIVATE WINDOW oDlg

return nil

// Define color constants for each theme
#define CLR_LIGHTGRAY    RGB(245, 245, 245)
#define CLR_DARKGRAY     RGB(45, 45, 48)
#define CLR_LIGHTBLUE    RGB(225, 243, 255)
#define CLR_BLUEGRAY     RGB(235, 245, 255)
#define CLR_SOFTWHITE    RGB(255, 255, 255)
#define CLR_LIGHYELLOW   RGB(255, 255, 200)
#define CLR_BRIGHTGRAY   RGB(245, 245, 245)
#define CLR_SOFTBLUE     RGB(160, 190, 210)
#define CLR_FLATBLUE     RGB(0, 120, 215)
#define CLR_LIGHTTURQ    RGB(230, 245, 255)

// Define color constants for each Gray theme
#define CLR_VLIGHTGRAY    RGB(240, 240, 240)
#define CLR_COOLGRAY      RGB(235, 235, 235)
#define CLR_WARMGRAY      RGB(245, 243, 240)

// Function to set theme colors for the TBROWSE based on theme index.
function SetTbrowseTheme( oBrw, nTheme )

   local aThemes := Array(4)

   switch nTheme
      case 1  // Classic Light Theme
         oDlg.Title := "Data Grid Example - Classic Light Theme"

         aThemes := { { CLR_BLACK, CLR_LIGHTGRAY }, ;
                      { CLR_BLACK, CLR_LIGHYELLOW }, ;
                      { CLR_LIGHTGRAY, LINES_HORZ }, ;
                      { RGB(0, 0, 128), RGB(200, 220, 255) } }
         exit

      case 2  // Classic Dark Theme
         oDlg.Title := "Data Grid Example - Classic Dark Theme"

         aThemes := { { RGB(255, 255, 255), CLR_DARKGRAY }, ;
                      { RGB(255, 255, 255), RGB(60, 90, 120) }, ;
                      { RGB(70, 70, 90), LINES_HORZ }, ;
                      { RGB(200, 200, 200), RGB(70, 70, 90) } }

         exit

      case 3  // Soft Light Gray Theme
         oDlg.Title := "Data Grid Example - Soft Light Gray Theme"

         aThemes := { { RGB(50, 50, 50), CLR_VLIGHTGRAY }, ;
                      { RGB(50, 50, 50), RGB(255, 250, 220) }, ;
                      { RGB(220, 220, 220), LINES_HORZ }, ;
                      { RGB(30, 30, 50), RGB(200, 220, 240) } }

         exit

      case 4  // Calming Cool Gray Theme
         oDlg.Title := "Data Grid Example - Calming Cool Gray Theme"

         aThemes := { { RGB(40, 40, 40), CLR_COOLGRAY }, ;
                      { RGB(40, 40, 40), RGB(230, 255, 255) }, ;
                      { RGB(210, 210, 210), LINES_HORZ }, ;
                      { RGB(30, 50, 30), RGB(210, 230, 210) } }

         exit

      case 5  // Muted Warm Gray Theme
         oDlg.Title := "Data Grid Example - Muted Warm Gray Theme"

         aThemes := { { RGB(70, 70, 70), CLR_WARMGRAY }, ;
                      { RGB(70, 70, 70), RGB(255, 245, 230) }, ;
                      { RGB(200, 200, 200), LINES_HORZ }, ;
                      { RGB(60, 40, 30), RGB(215, 200, 180) } }

         exit

      case 6  // Windows Vista Theme
         oDlg.Title := "Data Grid Example - Windows Vista Theme"

         aThemes := { { RGB(40, 40, 40), CLR_VLIGHTGRAY }, ;
                      { RGB(40, 40, 40), CLR_LIGHTTURQ }, ;
                      { RGB(220, 220, 220), LINES_HORZ }, ;
                      { RGB(70, 90, 110), CLR_SOFTBLUE } }

         exit

      case 7  // Windows 7 Theme
         oDlg.Title := "Data Grid Example - Windows 7 Theme"

         aThemes := { { CLR_BLACK, CLR_LIGHTGRAY }, ;
                      { CLR_BLACK, CLR_LIGHTBLUE }, ;
                      { RGB(215, 215, 215), LINES_HORZ }, ;
                      { RGB(0, 0, 0), RGB(86, 157, 229) } }

         exit

      case 8  // Windows 8 Theme
         oDlg.Title := "Data Grid Example - Windows 8 Theme"

         aThemes := { { RGB(30, 30, 30), CLR_BRIGHTGRAY }, ;
                      { RGB(30, 30, 30), RGB(215, 230, 255) }, ;
                      { RGB(200, 200, 200), LINES_HORZ }, ;
                      { RGB(255, 255, 255), CLR_FLATBLUE } }

         exit

      case 10 // Windows 10 Theme
         oDlg.Title := "Data Grid Example - Windows 10 Theme"

         aThemes := { { CLR_BLACK, RGB(250, 250, 250) }, ;
                      { CLR_BLACK, CLR_BLUEGRAY }, ;
                      { RGB(230, 230, 230), LINES_HORZ }, ;
                      { RGB(255, 255, 255), RGB(60, 60, 60) } }

         exit

      case 11 // Windows 11 Theme
         oDlg.Title := "Data Grid Example - Windows 11 Theme"

         aThemes := { { CLR_BLACK, CLR_SOFTWHITE }, ;
                      { CLR_BLACK, RGB(240, 248, 255) }, ;
                      { RGB(200, 200, 200), LINES_HORZ }, ;
                      { CLR_SOFTWHITE, RGB(98, 98, 128) } }
   end switch

   // Apply the selected theme colors and styles to the TBROWSE object
   with object oBrw
      :SetColor( { CLR_TEXT, CLR_PANE }, aThemes[1] )  // Set default text and pane colors
      :SetColor( { CLR_FOCUSF, CLR_FOCUSB }, aThemes[2] )  // Set colors for focused items

      :nClrLine   := aThemes[3][1]  // Set the line color
      :nLineStyle := aThemes[3][2]  // Set the line style

      // Set colors for specific elements in the TBROWSE table, such as headers and rows
      :nClrHeadFore := aThemes[4][1]  // Set header foreground color
      :nClrHeadBack := aThemes[4][2]  // Set header background color

      :SetColor( { CLR_HEADF, CLR_HEADB }, { aThemes[4][1], aThemes[4][2] } )

      if :nColumn( "ARRAYNO", .T. ) == 0  // If ARRAYNO column does not exist, add it
         :InsColNumber( 34, 1 )  // Insert a column showing row numbers
      endif

      // Set header background color for the selector
      :GetColumn( "SELECTOR" ):nClrBack := aThemes[4][2]

      // Set specific colors for the "ARRAYNO" column when it has focus
      :GetColumn( "ARRAYNO" ):nClrBack := aThemes[1][2]      // Background color
      :GetColumn( "ARRAYNO" ):nClrFocuFore := aThemes[2][1]  // Foreground color for focused rows
      :GetColumn( "ARRAYNO" ):nClrFocuBack := aThemes[2][2]  // Background color for focused rows
   end with

return nil

// Fills an array with random floating-point values for display in the TBROWSE table
static function FillArray()

   local aData := Array( 16, 6 )   // Create a 16-row, 6-column array
   local aRow

   // Fill each row with random floating-point values for testing
   for each aRow in aData
      AEval( aRow, { |u,i| aRow[ i ] := hb_RandomInt( 1,9999 ) * 0.01 } )  // Generate random values
   next

return aData
