/* 
 * MINIGUI - Harbour Win32 GUI Library Demo
 *
 * This program demonstrates converting a Python script using the tkinter library to MiniGUI.
 * It allows users to enter a color name in a text box, which updates the background color dynamically.
 *
 * Features:
 * - A frame that changes color based on user input
 * - A text box for entering color names
 * - A validation function to ensure valid color names
 * - A function to convert color names to RGB values
 */

#include "hmg.ch" 
#include "winprint.ch"
  
////////////////////////////////////////////////////////////////////
// Main function that initializes and displays the main window
FUNCTION Main()

   LOCAL bg := nColorNameToRGB( "WHITE" ) // Convert color name to RGB value
  
   // Define main window
   DEFINE WINDOW Win_1 ;
      CLIENTAREA 600, 400 ; // Set window size
      TITLE 'Palette' ;     // Window title
      MAIN ;                // Define this as the main window
      BACKCOLOR bg          // Set background color

      // Draw a large frame on the window
      DrawFrame( 10, 10, 200, 580 )

      // Create a label inside the frame
      @ 10, 10 LABEL frame WIDTH 580 HEIGHT 200 ;
         VALUE "" ;   // Empty label
         BACKCOLOR bg // Background color

      // Draw a smaller frame for a text input
      DrawFrame( 320, 10, 50, 580 )

      // Define a text input box for color selection
      DEFINE GETBOX NUL
         ROW 330
         COL 15
         WIDTH 560
         HEIGHT 30
         BORDER .F. // No border
         FONTNAME "Martian Mono"
         FONTSIZE 20
         FONTBOLD .T.
         VALUE SPACE( 36 ) // Empty text box with 36 spaces
         ON CHANGE { || bg := This.Value, iif( Lower( AllTrim( bg ) ) == "quit", ThisWindow.Release, This.frame.BackColor := ;
            HMG_n2RGB( nColorNameToRGB( iif( ColorValid( bg ), bg, "WHITE" ) ) ) ) } // Update background color dynamically
      END GETBOX

   END WINDOW

   CENTER WINDOW Win_1   // Center the window on the screen
   ACTIVATE WINDOW Win_1 // Show the window

RETURN NIL 

////////////////////////////////////////////////////////////////////
// Function to draw a frame on the window
FUNCTION DrawFrame( t, l, b, r, aColor, nPen, cWindowName )

   DEFAULT cWindowName := ThisWindow.Name // Default to the current window
   DEFAULT aColor := BLACK, nPen := 5 // Default color is black and pen width is 5

   // Draw a rectangle to create a frame
   DRAW RECTANGLE IN WINDOW (cWindowName)  ;
        AT t - 3, l - 3 TO t + b + 3, l + r + 3 ;
        PENCOLOR aColor PENWIDTH nPen

RETURN NIL

////////////////////////////////////////////////////////////////////
// Function to check if the given color is valid
FUNCTION ColorValid( cColorName )

   LOCAL nColor

   INIT PRINTSYS                      // Initialize printing system (used for color conversion)
   nColor := HBPRNCOLOR( cColorName ) // Convert color name to numeric value
   RELEASE PRINTSYS                   // Release printing system

RETURN ( nColor > 0 ) // Return true if the color is valid

////////////////////////////////////////////////////////////////////
// Function to convert a color name to its RGB equivalent
FUNCTION nColorNameToRGB( cColorName )

   LOCAL nColor

   INIT PRINTSYS                      // Initialize printing system
   nColor := HBPRNCOLOR( cColorName ) // Convert color name to numeric format
   RELEASE PRINTSYS                   // Release printing system

RETURN nColor
