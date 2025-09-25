/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Data provided by gs.statcounter.com for July 2025
 */

#include "hmg.ch"
#include "GraphPlus.ch"

// define the static arrays for graph show and print routines
STATIC aSeries
STATIC aSerieNames
STATIC aColors
STATIC oObj

/*
 * FUNCTION Main()
 *
 * Initializes the application, defines the main window, and displays a pie chart.
 *
 * Purpose:
 *   This is the entry point of the application. It performs the following tasks:
 *     1. Creates a GraphPlus object to handle graph drawing.
 *     2. Defines the data series for the pie chart (desktop OS market share).
 *     3. Defines the labels for each slice of the pie chart.
 *     4. Sets the colors for each slice of the pie chart using the Netscape 216 color scheme.
 *     5. Formats the labels to include the percentage value for each slice.
 *     6. Defines the main window with its controls (image control for the chart, buttons for drawing and saving).
 *     7. Sets the initial values and event handlers for the controls.
 *     8. Centers the main window on the screen.
 *     9. Activates the main window, making it visible to the user.
 *
 * Notes:
 *   The GraphPlus object is responsible for drawing the pie chart.
 *   The aSeries, aSerieNames, and aColors arrays define the data, labels, and colors for the chart.
 *   The GetColors() function generates the color palette.
 */
FUNCTION Main

   // create graph object
   oObj := GraphPlus():New()

   // set the series data
   aSeries := { ;
      53.51, ;
      42.88, ;
      2.02, ;
      0.88, ;
      0.43, ;
      0.23, ;
      0.05  ;
      }

   // set the series names
   aSerieNames := { ;
      "Windows 11", ;
      "Windows 10", ;
      "Windows 7", ;
      "Windows 8", ;
      "Windows XP", ;
      "Windows 8.1", ;
      "Other" ;
      }

   // set the colors using of Netscape 216 color's scheme (51 * n)
   aColors := GetColors()

   AEval( aSerieNames, {|x, i| aSerieNames[ i ] := x + " (" + hb_ntos( aSeries[ i ] ) + "%)" } )

   SET FONT TO GetDefaultFontName(), 10

   // initialise a default font name
   IF Empty( _HMG_DefaultFontName )
      _HMG_DefaultFontName := GetDefaultFontName()
   ENDIF

   // initialise a default font size
   IF Empty( _HMG_DefaultFontSize )
      _HMG_DefaultFontSize := GetDefaultFontSize()
   ENDIF

   DEFINE WINDOW m ;
      AT 0, 0 ;
      WIDTH 720 HEIGHT 660 ;
      MAIN ;
      TITLE "Print Pie Graph" ;
      BACKCOLOR { 216, 208, 200 }

   DEFINE IMAGE chart
      ROW 10
      COL 130
      WIDTH 550
      HEIGHT 600
      STRETCH .T.
   END IMAGE

   DEFINE BUTTON NUL
      ROW 10
      COL 10
      CAPTION "Draw"
      ACTION showpie( .T. )
   END BUTTON

   DEFINE BUTTON NUL
      ROW 40
      COL 10
      CAPTION "Save"
      ACTION ( showpie( .F. ), savepie() )
   END BUTTON

   END WINDOW

   m.Center()
   m.Activate()

RETURN NIL

/*
 * FUNCTION showpie( lRandom )
 *
 * Draws the pie chart on the image control of the main window.
 *
 * Parameters:
 *   lRandom (LOGICAL):  If .T., the colors of the pie chart slices are randomized. If .F., the colors are used in the order they are defined in the aColors array.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function is responsible for drawing the pie chart using the data, labels, and colors defined in the global arrays.
 *   It uses the GraphPlus object to create the chart and then sets the HBITMAP property of the image control to display the chart.
 *   The function also creates a context menu for the main window, allowing the user to change the graph font.
 *
 * Notes:
 *   The function deletes the existing bitmap before creating a new one to avoid memory leaks.
 *   The function uses the _HMG_DefaultFontName and _HMG_DefaultFontSize global variables to set the font for the chart.
 */
FUNCTION showpie( lRandom )

   IF hb_defaultValue( lRandom, .F. )
      // Colors randomly
      AShuffle( aColors )
   ENDIF

   Create_CONTEXT_Menu( ThisWindow.Name )

   IF ! Empty( oObj:hBitmap )
      DeleteObject( oObj:hBitmap )
      oObj:hBitmap := NIL
   ENDIF

   WITH OBJECT oObj
      :nImageWidth := m.chart.Width
      :nImageHeight := m.chart.Height
      :GraphData := aSeries
      :Legends := aSerieNames
      :GraphColors := aColors
      :Title := 'Desktop Windows Version Market Share Worldwide'
      :GraphType := GT_PIE
      :ShowLegends := .T.
      :LegendPos := LEGEND_ON_BOTTOM
      :LegendFont := CREATE ARRAY FONT (_HMG_DefaultFontName) SIZE (_HMG_DefaultFontSize + 2) BOLD .F.
      :TitleFont := CREATE ARRAY FONT (_HMG_DefaultFontName) SIZE (_HMG_DefaultFontSize + 6) BOLD .T.
      :aTitleColor := BLACK
      :nPieGap := 1
      :Draw()
      SetProperty( ThisWindow.Name, 'chart', 'HBITMAP', :Bitmap )
   ENDWITH

RETURN NIL

/*
 * FUNCTION savepie()
 *
 * Saves the pie chart to a PNG file.
 *
 * Purpose:
 *   This function allows the user to save the generated pie chart as a PNG image file.
 *   It calls the Save() method of the GraphPlus object to save the chart to a file named "graph.png".
 *   After saving, it displays a message box informing the user of the file name.
 *
 * Notes:
 *   The file name is hardcoded as "graph.png".  A future enhancement could allow the user to specify the file name and location.
 */
FUNCTION savepie

   LOCAL cFileName := 'graph.png'

   oObj:Save( cFileName )

   MsgInfo( "Save as: " + cFileName )

RETURN NIL

/*
 * PROCEDURE Create_CONTEXT_Menu( cForm )
 *
 * Creates a context menu for the specified form.
 *
 * Parameters:
 *   cForm (CHARACTER): The name of the form to which the context menu will be attached.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This procedure creates a context menu that allows the user to change the font name and size of the graph.
 *   It first checks if a context menu already exists for the form and releases it if it does.
 *   Then, it defines the context menu with two items: "Change Graph Font Name" and "Change Graph Font Size".
 *   Each item calls the GetFont() function to allow the user to select a new font and then calls the showpie() function to redraw the graph with the new font.
 *
 * Notes:
 *   The GetFont() function is a standard HMG function that displays a font selection dialog.
 *   The _HMG_DefaultFontName and _HMG_DefaultFontSize global variables store the current font name and size.
 */
PROCEDURE Create_CONTEXT_Menu( cForm )

   IF IsContextMenuDefined( cForm ) == .T.
      Release_CONTEXT_Menu( cForm )
   ENDIF

   DEFINE CONTEXT MENU OF ( cForm )

      ITEM 'Change Graph Font Name' ACTION ;
         ( _HMG_DefaultFontName := GetFont ( _HMG_DefaultFontName, _HMG_DefaultFontSize, .F., .F., { 0, 0, 0 }, .F., .F., 0 ) [ 1 ], showpie() )

      ITEM 'Change Graph Font Size' ACTION ;
         ( _HMG_DefaultFontSize := GetFont ( _HMG_DefaultFontName, _HMG_DefaultFontSize, .F., .F., { 0, 0, 0 }, .F., .F., 0 ) [ 2 ], showpie() )

   END MENU

RETURN

/*
 * PROCEDURE Release_CONTEXT_Menu( cForm )
 *
 * Releases the context menu for the specified form.
 *
 * Parameters:
 *   cForm (CHARACTER): The name of the form whose context menu should be released.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This procedure releases the context menu associated with the given form.
 *   It first checks if a context menu is defined for the form. If not, it displays a message box.
 *   If a context menu is defined, it releases it using the RELEASE CONTEXT MENU command.
 *
 * Notes:
 *   This procedure is used to clean up the context menu before creating a new one.
 */
PROCEDURE Release_CONTEXT_Menu( cForm )

   IF IsContextMenuDefined( cForm ) == .F.
      MsgInfo( "Context Menu not defined" )
      RETURN
   ENDIF

   RELEASE CONTEXT MENU OF ( cForm )

RETURN

/*
 * FUNCTION AShuffle( aArray )
 *
 * Shuffles the elements of an array randomly.
 *
 * Parameters:
 *   aArray (ARRAY): The array to be shuffled.
 *
 * Returns:
 *   ARRAY: The shuffled array (the original array is modified in place).
 *
 * Purpose:
 *   This function shuffles the elements of an array randomly. It is used to randomize the colors of the pie chart slices.
 *   The algorithm works by iterating through the array and swapping each element with a randomly chosen element.
 *   It ensures that each index is only used once by keeping track of the used indices in a separate array.
 *
 * Notes:
 *   The function modifies the original array in place.
 *   The function uses the Random() function to generate random numbers.
 *   The function uses the AScan() function to check if an index has already been used.
 */
FUNCTION AShuffle( aArray )

   LOCAL n, i, j, a := {}

   IF ( n := Len( aArray ) ) > 1

      FOR i := 1 TO n

         REPEAT
            j := Random( n )       // Random index in [1..n]
         UNTIL AScan( a, j ) != 0  // Accept only if not already used

         AAdd( a, j )

         // Swap elements
         j := aArray[ i ]
         aArray[ i ] := aArray[ a[ i ] ]
         aArray[ a[ i ] ] := j
      NEXT i
   ENDIF

RETURN aArray

/*
 * FUNCTION GetColors()
 *
 * Generates an array of colors using the Netscape 216 color scheme.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   ARRAY: An array of color triplets, where each triplet is an array containing the red, green, and blue components of the color.
 *
 * Purpose:
 *   This function generates a color palette consisting of the 216 colors in the Netscape color cube.
 *   These colors are generated by iterating through all possible combinations of red, green, and blue values, where each value is a multiple of 51 (0, 51, 102, 153, 204, 255).
 *   The resulting colors are stored in an array of color triplets.
 *   The white color {255, 255, 255} is removed from the array.
 *
 * Notes:
 *   The Netscape color cube is a set of 216 colors that are guaranteed to be displayed correctly on all platforms.
 *   This function is used to generate the color palette for the pie chart.
 */
FUNCTION GetColors()

   LOCAL aColors := {}
   LOCAL r, g, b

   FOR r := 0 TO 255 STEP 51
      FOR g := 0 TO 255 STEP 51
         FOR b := 0 TO 255 STEP 51
            // Store as {R, G, B} triplet
            AAdd( aColors, { r, g, b } )
         NEXT
      NEXT
   NEXT
   
   hb_ADel( aColors, AScan( aColors, { 255, 255, 255 } ), .T. )

RETURN( aColors )
