/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demonstrates the use of TBrowse with tabs to display different pages.
 *
 * Author: Igor Nazarov
 *
 * Modified by Grigory Filatov
 */

#include "minigui.ch"
#include "i_winuser.ch"
#include "tsbrowse.ch"

PROCEDURE Main()
   /*
    *  Main application procedure.
    *  Creates a main window containing a TBrowse control.  The TBrowse acts as a tab control,
    *  and selecting a tab displays a corresponding page (window) below the TBrowse.
    */

   LOCAL oWnd          // Handle to the main window object
   LOCAL cTitle        // Title of the main window
   LOCAL oBrw          // Handle to the TBrowse object
   LOCAL aArray        // Array containing the data for the TBrowse (tab labels)
   LOCAL aFont := {}   // Array to store font handles for different TBrowse cell states
   LOCAL i, nIndex     // Loop counters
   LOCAL nTab          // Tab index

   SET OOP ON // Enable Object-Oriented Programming

   aArray := { { "Sheet 1", "Sheet 2", "Sheet 3", "Sheet 4" } } // Initialize the TBrowse data array with tab labels
   cTitle := "TBrowse TAB in TAB Test" // Set the title of the main window

   DEFINE WINDOW Form_0 ;              // Define the main window
         AT 0, 0 ;                     // Position of the window (top-left corner)
         WIDTH 400 HEIGHT 300 ;        // Dimensions of the window
         TITLE cTitle ;                // Set the window title
         MAIN ;                        // Designate this window as the main window
         ON INIT oBrw:SetFocus() ;     // Set focus to the TBrowse object when the window is initialized
         ON MAXIMIZE oBrw:SetFocus() ; // Set focus to the TBrowse object when the window is maximized
         ON SIZE oBrw:SetFocus()       // Set focus to the TBrowse object when the window is resized

      oWnd := ThisWindow.OBJECT        // Get the window object handle

   END WINDOW


   DEFINE FONT Font_1 FONTNAME "Times New Roman" SIZE 11      // Define a font named "Font_1" for unselected tabs
   DEFINE FONT Font_2 FONTNAME "Times New Roman" SIZE 13 BOLD // Define a font named "Font_2" (bold) for the selected tab

   AAdd( aFont, GetFontHandle( "Font_1" ) ) // Add the handle of "Font_1" to the aFont array
   AAdd( aFont, GetFontHandle( "Font_2" ) ) // Add the handle of "Font_2" to the aFont array


   DEFINE TBROWSE oBrw AT 5, 2 OF Form_0 ; // Define the TBrowse object within the main window
         WIDTH oWnd:ClientWidth() ;        // Set the width to the client area width of the main window
         HEIGHT 25 ;                       // Set the height of the TBrowse
         GRID                              // Display grid lines in the TBrowse

      :SetArray( aArray, .T. ) // Set the data array for the TBrowse, .T. means the array is read-only

      :nWheelLines := 0.1 // Set the number of lines to scroll per mouse wheel tick (fractional value) - effectively disables scrolling
      :nClrLine := COLOR_GRID // Set the color of the grid lines
      :lNoChangeOrd := TRUE // Disable changing the order of columns by dragging
      :lCellBrw := TRUE // Enable cell browsing (selecting individual cells)
      :lNoKeyChar := TRUE // Disable character input in the TBrowse

      :lNoVScroll := TRUE // Disable the vertical scrollbar
      :lNoHScroll := TRUE // Disable the horizontal scrollbar

      :nHeightCell := 25 // Set the height of each cell
      :nHeightHead := 0 // Set the height of the header (set to 0 to hide the header)
      :bChange := {|| nil } // Define a code block to execute when the TBrowse data changes (currently does nothing)
      :bLDblClick := {|| nil } // Define a code block to execute on a left double-click (currently does nothing)

      :SetColor( { 1 }, { {|| RGB( 0, 0, 0 ) } }, ) // Set the color for color scheme 1 (text color RGB(0,0,0) - black)
      :SetColor( { 5 }, { {|| RGB( 0, 0, 0 ) } }, ) // Set the color for color scheme 5 (text color RGB(0,0,0) - black)
      :SetColor( { 11 }, { {|| RGB( 0, 0, 0 ) } }, ) // Set the color for color scheme 11 (text color RGB(0,0,0) - black)


      :SetColor( { 6 }, { {| a, b, c | IF( c:nCell == b, { RGB( 255, 255, 255 ), RGB( 220, 220, 220 ) }, ;
         { RGB( 255, 255, 255 ), RGB( 220, 220, 220 ) } ) } } ) // Set the color for color scheme 6 (highlighted cell)


      :SetColor( { 12 }, { {| a, b, c | IF( c:nCell == b, { RGB( 255, 255, 255 ), RGB( 220, 220, 220 ) }, ;
         { RGB( 255, 255, 255 ), RGB( 220, 220, 220 ) } ) } } ) // Set the color for color scheme 12 (highlighted cell)

      // Loop through each column (tab) and change the font based on whether it's the selected tab.
      FOR nTab = 1 TO oBrw:nColCount()
         FOR EACH nIndex IN { nTab }
            :ChangeFont( {|| IF( oBrw:nCell == nIndex, aFont[ 2 ], aFont[ 1 ] ) }, nIndex, 1 )
         NEXT
      NEXT

      :bTSDrawCell := {| a, b, c | b:nClrLine := iif( b:nCell == oBrw:nCell, CLR_GRAY, COLOR_GRID ) } // Codeblock to change the color of the selected cell

      :bOnDraw := {|| DrawPage( oBrw:nCell ) } // Define a code block to execute when the TBrowse is drawn (calls DrawPage) - calls DrawPage to display the correct page

   END TBROWSE

   // Add a static edge style to the TBrowse window.
   SetWindowLong( oBrw:hWnd, GWL_EXSTYLE, WS_EX_STATICEDGE )

   // Define pages (windows) that are displayed based on the TBrowse selection.
   FOR i := 1 TO oBrw:nColCount() // Loop through the number of columns in the TBrowse

      DEFINE WINDOW &( "P" + hb_ntoc( i ) ) AT 30, 2 ; // Define a window for each page (dynamically named)
            PARENT Form_0 ; // Set the parent window to the main window
            WIDTH oWnd:ClientWidth() ; // Set the width to the client area width of the main window
            HEIGHT oWnd:ClientHeight() - 30 ; // Set the height to the client area height minus 30
            PANEL ; // Create a panel window (a child window that can contain other controls)
            BACKCOLOR { 220, 220, 220 } ; // Set the background color
            NOSHOW // Initially hide the window

         DEFINE TAB &( "T" + hb_ntoc( i ) ) ; // Define a tab control within each page window (dynamically named)
               AT 10, 10 ; // Position of the tab control
               WIDTH oWnd:ClientWidth() - 20 - 4 ; // Set the width
               HEIGHT oWnd:ClientHeight() - 50 ; // Set the height
               VALUE 1 ; // Set the initial selected tab
               FONT 'Font_1' ; // Set the font
               HOTTRACK ; // Enable hot tracking (highlighting tabs on mouse hover)
               HTINACTIVECOLOR GRAY ; // Set the color of inactive hot-tracked tabs
               HTFORECOLOR BLACK ; // Set the foreground color of hot-tracked tabs
               BACKCOLOR { 220, 220, 220 } // Set the background color

            PAGE 'Page &1' // Define the first tab page

               DEFINE LABEL &( "L" + hb_ntoc( i ) ) // Define a label within the first tab page (dynamically named)
                  ROW 100
                  COL 10
                  WIDTH oWnd:ClientWidth() - 40
                  HEIGHT 30
                  FONTNAME 'Arial'
                  FONTSIZE 18
                  FONTBOLD .T.
                  VALUE "Page " + hb_ntoc( i )
                  BACKCOLOR { 220, 220, 220 }
                  CENTERALIGN .T.
               END LABEL

            END PAGE

            IF i > 1 // Conditionally add more pages based on the loop counter
               PAGE 'Page &2'

               END PAGE
            ENDIF

            IF i > 2
               PAGE 'Page &3'

               END PAGE
            ENDIF

            IF i > 3
               PAGE 'Page &4'

               END PAGE
            ENDIF

         END TAB

      END WINDOW

   NEXT

   drawPage( 1 ) // Display the first page initially

   Form_0.Center() // Center the main window on the screen
   Form_0.Activate() // Activate the main window

RETURN


PROCEDURE drawPage( i )
   /*
    *  Procedure to show the page corresponding to the selected TBrowse cell (tab).
    *  It hides the previously displayed page and shows the new one.
    *
    *  Input parameters:
    *     i - The page number to display (corresponding to the TBrowse cell number).
    */

   STATIC nPage := 0 // Static variable to store the currently displayed page number

   IF nPage <> 0 // If a page is currently displayed
      DoMethod( "P" + hb_ntoc( nPage ), "hide" ) // Hide the current page
   END
   nPage := i // Set the new current page number
   DoMethod( "P" + hb_ntoc( nPage ), "show" ) // Show the new page

RETURN
