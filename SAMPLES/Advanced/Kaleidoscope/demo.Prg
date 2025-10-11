#include <minigui.ch>

MEMVAR backcolor
MEMVAR akalai
MEMVAR x
MEMVAR y

/*
 * FUNCTION Main()
 *
 * Initializes the application, defines the main window, and sets up the UI elements for the Kaleidoscope application.
 *
 * Purpose:
 *   This is the entry point of the application. It performs the following tasks:
 *     1. Sets the default slideshow interval.
 *     2. Initializes the background color to black.
 *     3. Initializes an empty array to store the shapes drawn (akalai).
 *     4. Sets the center coordinates (x, y) for the kaleidoscope effect.
 *     5. Defines the main window with its controls (labels, spinners, checkboxes, buttons, hyperlinks).
 *     6. Sets the initial values and event handlers for the controls.
 *     7. Centers the main window on the screen.
 *     8. Activates the main window, making it visible to the user.
 *   The main window contains controls to adjust the number of iterations, select shapes to draw (triangles, circles, rectangles, ellipses, squares, polygons),
 *   change the background color, toggle the slideshow mode, save the current image, and display a stereo view.
 *   It also includes hyperlinks to external resources.
 *
 * Notes:
 *   The akalai array stores the points, colors, and type of each shape drawn, which is used for the stereo view.
 *   The drawkalai() function is called initially and by the timer to redraw the kaleidoscope.
 *   The iswindowactive(s) check prevents drawing the stereo window if it's not active, improving performance.
 */
FUNCTION Main

   LOCAL showinterval := 2000
   PRIVATE backcolor := { 0, 0, 0 }
   PRIVATE akalai := {}
   PRIVATE x := 250 // xcenter
   PRIVATE y := 250 // ycenter

   DEFINE WINDOW k AT 0, 0 WIDTH 650 HEIGHT 530 TITLE "Kaleidoscope - 3D v.1.1 by S. Rathinagiri" MAIN ICON "kalai" NOMAXIMIZE NOSIZE ON INIT drawkalai()
      DEFINE TIMER timer1 INTERVAL showinterval ACTION ( drawkalai(), iif( iswindowactive( s ), drawstereowindow(), ) )

      DEFINE LABEL iterationslabel
         ROW 10
         COL 510
         VALUE "Iterations"
         WIDTH 80
      END LABEL
      DEFINE SPINNER iterations
         ROW 10
         COL 590
         WIDTH 50
         RANGEMIN 1
         RANGEMAX 999
         VALUE 3
      END SPINNER
      DEFINE FRAME shapes
         ROW 40
         COL 505
         WIDTH 135
         HEIGHT 200
         CAPTION "Shapes"
      END FRAME
      DEFINE CHECKBOX triangles
         ROW 60
         COL 510
         WIDTH 120
         CAPTION "Triangles"
      END CHECKBOX
      DEFINE CHECKBOX circles
         ROW 90
         COL 510
         WIDTH 120
         CAPTION "Circles"
      END CHECKBOX
      DEFINE CHECKBOX rectangles
         ROW 120
         COL 510
         WIDTH 120
         CAPTION "Rectangles"
      END CHECKBOX
      DEFINE CHECKBOX ellipses
         ROW 150
         COL 510
         WIDTH 120
         CAPTION "Ellipses"
      END CHECKBOX
      DEFINE CHECKBOX squares
         ROW 180
         COL 510
         WIDTH 120
         CAPTION "Squares"
      END CHECKBOX
      DEFINE CHECKBOX polygons
         ROW 210
         COL 510
         WIDTH 120
         CAPTION "Polygons"
         VALUE .T.
      END CHECKBOX
      DEFINE BUTTON backcolor
         ROW 250
         COL 510
         WIDTH 130
         CAPTION "Background Colour"
         ACTION setbackcolor()
      END BUTTON
      DEFINE CHECKBOX show1
         ROW 280
         COL 510
         CAPTION "Slideshow (" + AllTrim( Str( showinterval / 1000, 5, 0 ) ) + " Sec)"
         WIDTH 130
         ONCHANGE show1changed()
      END CHECKBOX
      DEFINE BUTTON but1
         ROW 310
         COL 510
         WIDTH 60
         CAPTION "Show"
         ACTION drawkalai()
      END BUTTON
      DEFINE BUTTON save
         ROW 310
         COL 580
         WIDTH 60
         CAPTION "Save"
         ACTION savewindow1()
      END BUTTON
      DEFINE BUTTON showstereo
         ROW 340
         COL 510
         WIDTH 130
         CAPTION "Show Stereo"
         ACTION showstereowindow()
      END BUTTON
      DEFINE HYPERLINK email1
         ROW 400
         COL 510
         VALUE "Algorithm of Cliff"
         WIDTH 130
         HANDCURSOR .T.
         ADDRESS "http://sprott.physics.wisc.edu/pickover/ekscop.html"
      END HYPERLINK
      DEFINE HYPERLINK email
         ROW 430
         COL 510
         VALUE "E-Mail me"
         WIDTH 130
         HANDCURSOR .T.
         ADDRESS "srgiri@dataone.in"
      END HYPERLINK
      ON KEY ESCAPE ACTION thiswindow.release()
   END WINDOW
   k.show1.value := .T.
   k.center
   ACTIVATE WINDOW k

RETURN NIL

/*
 * FUNCTION drawkalai()
 *
 * Draws the kaleidoscope pattern on the main window.
 *
 * Purpose:
 *   This function generates and draws the kaleidoscope pattern based on the user's selections (shapes, iterations)
 *   and random color generation. It clears the window and then iterates based on the 'iterations' spinner value.
 *   For each iteration, it checks which shapes are selected (triangles, circles, rectangles, ellipses, squares, polygons)
 *   and draws them with random colors. The shapes are then flipped and drawn in 8 different orientations to create the kaleidoscope effect.
 *   The shape data (points, color, type) is stored in the akalai array for use in the stereo view.
 *
 * Notes:
 *   The akalai array is cleared at the beginning of the function to ensure that only the shapes from the current iteration are displayed.
 *   The flip() function is used to generate the different orientations of the shapes.
 *   The random() function is used to generate random coordinates and colors.
 *   The drawrect, drawellipse, and drawpolygon functions are HMG Extended functions for drawing shapes on a window.
 */
FUNCTION drawkalai

   LOCAL aPoints
   LOCAL apoints1
   LOCAL x1, x2, y1, y2, t
   LOCAL i, j := 0, r, g, b, r1
   LOCAL sides

   ASize( akalai, 0 )
   ERASE WINDOW k
   IF k.iterations.value < 1
      RETURN NIL
   ENDIF

   drawrect( "k", 0, 0, x * 2, y * 2, backcolor,, backcolor )

   FOR i = 1 TO k.iterations.value

      IF k.triangles.value
         sides := 3
         aPoints := {}
         FOR j := 1 TO sides
            x1 := random( x )
            y1 := random( y )
            IF ( x1 > y1 )
               t := x1
               x1 := y1
               y1 := t
            ENDIF
            AAdd( apoints, { x1, y1 } )
         NEXT j
         r := random( 255 )
         g := random( 255 )
         b := random( 255 )
         FOR j := 1 TO 8
            apoints1 := flip( AClone( apoints ), x, y, j )
            drawpolygon( "k", aPoints1, { r, g, b },, { r, g, b } )
            AAdd( akalai, { AClone( apoints1 ), { r, g, b }, "TRIANGLE" } )
         NEXT j
      ENDIF

      IF k.circles.value
         x1 = random( x )
         y1 = random( y )
         r1 := random( Min( x - x1, y - y1 ) )
         IF ( x1 > y1 )
            t := x1
            x1 := y1
            y1 := t
         ENDIF
         x2 := x1 + r1
         y2 := y1 + r1
         r := random( 255 )
         g := random( 255 )
         b := random( 255 )
         aPoints := { { x1, y1 }, { x2, y2 } }
         FOR j := 1 TO 8
            apoints1 := flip( AClone( apoints ), x, y, j )
            drawellipse( "k", apoints1[ 1, 1 ], apoints1[ 1, 2 ], apoints1[ 2, 1 ], apoints1[ 2, 2 ], { r, g, b },, { r, g, b } )
            AAdd( akalai, { AClone( apoints1 ), { r, g, b }, "CIRCLE" } )
         NEXT j
      ENDIF

      IF k.ellipses.value
         sides := 2
         aPoints := {}
         FOR j := 1 TO sides
            x1 := random( x )
            y1 := random( y )
            IF ( x1 > y1 )
               t := x1
               x1 := y1
               y1 := t
            ENDIF
            AAdd( apoints, { x1, y1 } )
         NEXT j
         r := random( 255 )
         g := random( 255 )
         b := random( 255 )
         FOR j := 1 TO 8
            apoints1 := flip( AClone( apoints ), x, y, j )
            drawellipse( "k", apoints1[ 1, 1 ], apoints1[ 1, 2 ], apoints1[ 2, 1 ], apoints1[ 2, 2 ], { r, g, b },, { r, g, b } )
            AAdd( akalai, { AClone( apoints1 ), { r, g, b }, "ELLIPSE" } )
         NEXT j
      ENDIF

      IF k.rectangles.value
         sides := 2
         aPoints := {}
         FOR j := 1 TO sides
            x1 := random( x )
            y1 := random( y )
            IF ( x1 > y1 )
               t := x1
               x1 := y1
               y1 := t
            ENDIF
            AAdd( apoints, { x1, y1 } )
         NEXT j
         r := random( 255 )
         g := random( 255 )
         b := random( 255 )

         FOR j := 1 TO 8
            apoints1 := flip( AClone( apoints ), x, y, j )
            drawrect( "k", apoints1[ 1, 1 ], apoints1[ 1, 2 ], apoints1[ 2, 1 ], apoints1[ 2, 2 ], { r, g, b },, { r, g, b } )
            AAdd( akalai, { AClone( apoints1 ), { r, g, b }, "RECTANGLE" } )
         NEXT j
      ENDIF

      IF k.squares.value
         x1 = random( x )
         y1 = random( y )
         r1 := random( Min( x - x1, y - y1 ) )
         IF ( x1 > y1 )
            t := x1
            x1 := y1
            y1 := t
         ENDIF
         x2 := x1 + r1
         y2 := y1 + r1
         r := random( 255 )
         g := random( 255 )
         b := random( 255 )
         aPoints := { { x1, y1 }, { x2, y2 } }
         FOR j := 1 TO 8
            apoints1 := flip( AClone( apoints ), x, y, j )
            drawrect( "k", apoints1[ 1, 1 ], apoints1[ 1, 2 ], apoints1[ 2, 1 ], apoints1[ 2, 2 ], { r, g, b },, { r, g, b } )
            AAdd( akalai, { AClone( apoints1 ), { r, g, b }, "SQUARE" } )
         NEXT j
      ENDIF

      IF k.polygons.value .OR. Empty( j )
         IF Empty( j )
            k.polygons.value := .T.
         ENDIF
         sides := 4 + random( 20 )
         aPoints := {}
         FOR j := 1 TO sides
            x1 := random( x )
            y1 := random( y )
            IF ( x1 > y1 )
               t := x1
               x1 := y1
               y1 := t
            ENDIF
            AAdd( apoints, { x1, y1 } )
         NEXT j
         r := random( 255 )
         g := random( 255 )
         b := random( 255 )
         FOR j := 1 TO 8
            apoints1 := flip( AClone( apoints ), x, y, j )
            drawpolygon( "k", aPoints1, { r, g, b },, { r, g, b } )
            AAdd( akalai, { AClone( apoints1 ), { r, g, b }, "POLYGON" } )
         NEXT j
      ENDIF
   NEXT i

RETURN NIL

/*
 * FUNCTION savewindow1()
 *
 * Saves the content of the main window to a bitmap file.
 *
 * Purpose:
 *   This function allows the user to save the current kaleidoscope image displayed in the main window to a BMP file.
 *   It first disables the timer (if the slideshow is active) to prevent the image from changing during the save process.
 *   Then, it prompts the user to select a file name and location using the PutFile() function.
 *   It ensures that the file name has a ".bmp" extension. If the file already exists, it asks the user for confirmation before overwriting.
 *   Finally, it calls the savewindow() function to save the window content to the specified file.
 *   After saving, it re-enables the timer if it was previously active.
 *
 * Notes:
 *   The PutFile() function is used to display a standard file save dialog.
 *   The savewindow() function is an HMG Extended function for saving the content of a window to a bitmap file.
 *   The msgyesno() function is used to display a confirmation dialog before overwriting an existing file.
 */
FUNCTION savewindow1

   LOCAL filename

   IF k.show1.value
      k.timer1.enabled := .F.
   ENDIF

   filename := PutFile ( { { "Bitmap Files (*.bmp)", "*.bmp" } }, "Save to bitmap file", , .F. )
   if ! Empty( filename )
      IF At( ".bmp", Lower( filename ) ) > 0
         IF .NOT. Right( Lower( filename ), 4 ) == ".bmp"
            filename := filename + ".bmp"
         ENDIF
      ELSE
         filename := filename + ".bmp"
      ENDIF

      IF File( filename )
         IF msgyesno( "Are you sure to overwrite?", "Save to bitmap" )
            savewindow( "k", filename, 0, 0, 500, 500 )
         ENDIF
      ELSE
         savewindow( "k", filename, 0, 0, 500, 500 )
      ENDIF
   ENDIF

   IF k.show1.value
      k.timer1.enabled := .T.
   ENDIF

RETURN NIL

/*
 * FUNCTION setbackcolor()
 *
 * Allows the user to select a new background color for the kaleidoscope.
 *
 * Purpose:
 *   This function opens a color selection dialog using the getcolor() function, allowing the user to choose a new background color.
 *   The selected color is then assigned to the backcolor variable, which is used by the drawkalai() function to draw the background.
 *
 * Notes:
 *   The getcolor() function is an HMG Extended function for displaying a color selection dialog.
 *   The backcolor variable is a global variable that stores the current background color.
 */
FUNCTION setbackcolor

   backcolor := getcolor( backcolor )

RETURN NIL

/*
 * FUNCTION show1changed()
 *
 * Toggles the state of the timer based on the value of the "Slideshow" checkbox.
 *
 * Purpose:
 *   This function is called when the state of the "Slideshow" checkbox changes.
 *   It enables or disables the timer based on the checkbox's value, effectively starting or stopping the slideshow.
 *
 * Notes:
 *   The iscontroldefined() function checks if the timer control is defined within the window.
 *   The k.timer1.enabled property is used to enable or disable the timer.
 */
FUNCTION show1changed

   IF .NOT. iscontroldefined( timer1, k )
      RETURN NIL
   ENDIF
   IF k.show1.value
      k.timer1.enabled := .T.
   ELSE
      k.timer1.enabled := .F.
   ENDIF

RETURN NIL

/*
 * FUNCTION showstereowindow()
 *
 * Displays a separate window showing a stereo (3D) view of the kaleidoscope pattern.
 *
 * Purpose:
 *   This function creates and displays a modal window that presents a stereo view of the kaleidoscope pattern.
 *   If the window is not already defined, it defines a new window named "s" with buttons to save the stereo image and close the window.
 *   It then centers and activates the window. If the window is already defined, it simply calls the drawstereowindow() function to redraw the stereo image.
 *
 * Notes:
 *   The iswindowdefined() function checks if the window is already defined.
 *   The DEFINE WINDOW block defines the properties and controls of the stereo window.
 *   The drawstereowindow() function is responsible for drawing the stereo image in the window.
 *   The stereo window is modal, meaning that the user must close it before interacting with the main window.
 */
FUNCTION showstereowindow

   IF .NOT. iswindowdefined( s )
      DEFINE WINDOW s AT 0, 0 WIDTH 1100 HEIGHT 580 TITLE "Stereo Window" MODAL NOSIZE backcolor { 0, 0, 0 } ON INIT drawstereowindow()
         DEFINE BUTTON save1
            ROW 520
            COL 10
            WIDTH 50
            CAPTION "Save"
            ACTION savestereowindow1()
         END BUTTON
         DEFINE BUTTON close1
            ROW 520
            COL 70
            WIDTH 50
            CAPTION "Close"
            ACTION s.release()
         END BUTTON

         ON KEY ESCAPE ACTION thiswindow.release()
      END WINDOW
      s.center
      s.activate
   ELSE
      drawstereowindow()
   ENDIF

RETURN NIL

/*
 * FUNCTION drawstereowindow()
 *
 * Draws the stereo (3D) view of the kaleidoscope pattern on the stereo window.
 *
 * Purpose:
 *   This function draws the stereo view of the kaleidoscope pattern based on the data stored in the akalai array.
 *   It calculates the stereo separation based on the stereobase and the number of shapes in the akalai array.
 *   It then iterates through the akalai array, converting the points of each shape to left and right eye views using the converttolpoints() and converttorpoints() functions.
 *   Finally, it draws the shapes for both the left and right eyes, creating the stereo effect.
 *
 * Notes:
 *   The akalai array contains the points, colors, and type of each shape drawn in the main window.
 *   The stereobase variable determines the overall stereo separation.
 *   The sdiff variable calculates the incremental stereo separation for each shape.
 *   The converttolpoints() and converttorpoints() functions adjust the y-coordinates of the points to create the left and right eye views.
 *   The drawrect, drawellipse, and drawpolygon functions are HMG Extended functions for drawing shapes on a window.
 */
FUNCTION drawstereowindow

   LOCAL stereobase := 30
   LOCAL sdiff, i
   LOCAL alpoints1
   LOCAL arpoints1

   IF Len( akalai ) == 0
      RETURN NIL
   ENDIF
   sdiff := stereobase / Len( akalai ) * 8

   ERASE WINDOW s
   drawrect( "s", 0, 0, 500, 1030, backcolor,, backcolor )

   FOR i := 1 TO Len( akalai )

      alpoints1 := converttolpoints( AClone( akalai[ i, 1 ] ), stereobase, sdiff * Int( ( i - 1 ) / 8 ) )
      arpoints1 := converttorpoints( AClone( akalai[ i, 1 ] ), stereobase )

      DO CASE
      CASE akalai[ i, 3 ] == "TRIANGLE" .OR. akalai[ i, 3 ] == "POLYGON"
         drawpolygon( "s", alPoints1, akalai[ i, 2 ],, akalai[ i, 2 ] )
         drawpolygon( "s", arPoints1, akalai[ i, 2 ],, akalai[ i, 2 ] )
      CASE akalai[ i, 3 ] == "CIRCLE" .OR. akalai[ i, 3 ] == "ELLIPSE"
         drawellipse( "s", alpoints1[ 1, 1 ], alpoints1[ 1, 2 ], alpoints1[ 2, 1 ], alpoints1[ 2, 2 ], akalai[ i, 2 ],, akalai[ i, 2 ] )
         drawellipse( "s", arpoints1[ 1, 1 ], arpoints1[ 1, 2 ], arpoints1[ 2, 1 ], arpoints1[ 2, 2 ], akalai[ i, 2 ],, akalai[ i, 2 ] )
      CASE akalai[ i, 3 ] == "RECTANGLE" .OR. akalai[ i, 3 ] == "SQUARE"
         drawrect( "s", alpoints1[ 1, 1 ], alpoints1[ 1, 2 ], alpoints1[ 2, 1 ], alpoints1[ 2, 2 ], akalai[ i, 2 ],, akalai[ i, 2 ] )
         drawrect( "s", arpoints1[ 1, 1 ], arpoints1[ 1, 2 ], arpoints1[ 2, 1 ], arpoints1[ 2, 2 ], akalai[ i, 2 ],, akalai[ i, 2 ] )
      ENDCASE

   NEXT i

RETURN NIL

/*
 * FUNCTION converttolpoints()
 *
 * Converts a set of points to the left eye view for stereo display.
 *
 * Parameters:
 *   apoints (Array): An array of points, where each point is an array containing the x and y coordinates {x, y}.
 *   stereobase (Numeric): The base stereo separation value.
 *   sdiff (Numeric): The incremental stereo separation value for the current shape.
 *
 * Returns:
 *   Array: A new array of points with the y-coordinates adjusted for the left eye view.
 *
 * Purpose:
 *   This function takes an array of points representing a shape and adjusts the y-coordinates of each point to create the left eye view for stereo display.
 *   The y-coordinate is adjusted by adding the stereobase and subtracting the sdiff value.
 *
 * Notes:
 *   The AClone() function is used to create a copy of the input array to avoid modifying the original array.
 */
FUNCTION converttolpoints( apoints, stereobase, sdiff )

   LOCAL alpoints := AClone( apoints )
   LOCAL i

   FOR i := 1 TO Len( alpoints )
      alpoints[ i, 2 ] := alpoints[ i, 2 ] + stereobase - sdiff
   NEXT i

RETURN alpoints

/*
 * FUNCTION converttorpoints()
 *
 * Converts a set of points to the right eye view for stereo display.
 *
 * Parameters:
 *   apoints (Array): An array of points, where each point is an array containing the x and y coordinates {x, y}.
 *   stereobase (Numeric): The base stereo separation value.
 *
 * Returns:
 *   Array: A new array of points with the y-coordinates adjusted for the right eye view.
 *
 * Purpose:
 *   This function takes an array of points representing a shape and adjusts the y-coordinates of each point to create the right eye view for stereo display.
 *   The y-coordinate is adjusted by adding stereobase and 2 * y (where y is the vertical center of the main window).
 *
 * Notes:
 *   The AClone() function is used to create a copy of the input array to avoid modifying the original array.
 */
FUNCTION converttorpoints( apoints, stereobase )

   LOCAL arpoints := AClone( apoints )
   LOCAL i

   FOR i := 1 TO Len( arpoints )
      arpoints[ i, 2 ] := arpoints[ i, 2 ] + ( 2 * y ) + stereobase
   NEXT i

RETURN arpoints

/*
 * FUNCTION savestereowindow1()
 *
 * Saves the content of the stereo window to a bitmap file.
 *
 * Purpose:
 *   This function allows the user to save the current stereo image displayed in the stereo window to a BMP file.
 *   It first disables the timer (if the slideshow is active) to prevent the image from changing during the save process.
 *   Then, it prompts the user to select a file name and location using the PutFile() function.
 *   It ensures that the file name has a ".bmp" extension. If the file already exists, it asks the user for confirmation before overwriting.
 *   Finally, it calls the savewindow() function to save the window content to the specified file.
 *   After saving, it re-enables the timer if it was previously active.
 *
 * Notes:
 *   The PutFile() function is used to display a standard file save dialog.
 *   The savewindow() function is an HMG Extended function for saving the content of a window to a bitmap file.
 *   The msgyesno() function is used to display a confirmation dialog before overwriting an existing file.
 */
FUNCTION savestereowindow1

   LOCAL filename

   IF k.show1.value
      k.timer1.enabled := .F.
   ENDIF

   filename := PutFile ( { { "Bitmap Files (*.bmp)", "*.bmp" } }, "Save to bitmap file", , .F. )
   if ! Empty( filename )
      IF At( ".bmp", Lower( filename ) ) > 0
         IF .NOT. Right( Lower( filename ), 4 ) == ".bmp"
            filename := filename + ".bmp"
         ENDIF
      ELSE
         filename := filename + ".bmp"
      ENDIF

      IF File( filename )
         IF msgyesno( "Are you sure to overwrite?", "Save to bitmap" )
            savewindow( "s", filename, 0, 0, 1030, 500 )
         ENDIF
      ELSE
         savewindow( "s", filename, 0, 0, 1030, 500 )
      ENDIF
   ENDIF

   IF k.show1.value
      k.timer1.enabled := .T.
   ENDIF

RETURN NIL

/*
 * FUNCTION flip()
 *
 * Flips and rotates a set of points around the center of the main window.
 *
 * Parameters:
 *   apoints (Array): An array of points, where each point is an array containing the x and y coordinates {x, y}.
 *   nx (Numeric): The x-coordinate of the center of the main window.
 *   ny (Numeric): The y-coordinate of the center of the main window.
 *   i (Numeric): The flip/rotation type (1-8).
 *
 * Returns:
 *   Array: A new array of points that have been flipped and/or rotated.
 *
 * Purpose:
 *   This function takes an array of points and performs a flip and/or rotation based on the value of the i parameter.
 *   It calculates the new coordinates for each point based on the selected flip/rotation type.
 *   This is used to create the kaleidoscope effect by generating different orientations of the shapes.
 *
 * Notes:
 *   The AClone() function is used to create a copy of the output array to avoid modifying the original array.
 *   The DO CASE statement handles the different flip/rotation types.
 */
FUNCTION flip( apoints, nx, ny, i )

   LOCAL aout := {}
   LOCAL j
   LOCAL x1 := 0
   LOCAL y1 := 0
   LOCAL xj
   LOCAL yj
   FOR j := 1 TO Len( apoints )
      xj := apoints[ j, 1 ]
      yj := apoints[ j, 2 ]
      DO CASE
      CASE i == 1
         x1 := nx + xj
         y1 := ny + yj
      CASE i == 2
         x1 := nx + xj
         y1 := ny - yj
      CASE i == 3
         x1 := nx - xj
         y1 := ny + yj
      CASE i == 4
         x1 := nx - xj
         y1 := ny - yj
      CASE i == 5
         x1 := ny + yj
         y1 := nx + xj
      CASE i == 6
         x1 := ny + yj
         y1 := nx - xj
      CASE i == 7
         x1 := ny - yj
         y1 := nx + xj
      CASE i == 8
         x1 := ny - yj
         y1 := nx - xj
      ENDCASE
      AAdd( aout, { x1, y1 } )
   NEXT j

RETURN AClone( aout )

/*
 * FUNCTION SAVEWINDOW ( cWindowName, cFileName, nRow, nCol, nWidth, nHeight )
 *
 * Saves the content of a window to a bitmap file.
 *
 * Parameters:
 *   cWindowName (Character): The name of the window to save.
 *   cFileName (Character): The name of the bitmap file to save to. If NIL, defaults to the window name with a .bmp extension in the startup folder.
 *   nRow (Numeric): The top row of the area to save. If NIL, saves the entire window.
 *   nCol (Numeric): The left column of the area to save. If NIL, saves the entire window.
 *   nWidth (Numeric): The width of the area to save. If NIL, saves the entire window.
 *   nHeight (Numeric): The height of the area to save. If NIL, saves the entire window.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function saves a portion or the entirety of a specified window to a bitmap file. It handles cases where the filename or dimensions are not provided,
 *   defaulting to saving the entire window to a file named after the window in the application's startup folder.
 *
 * Notes:
 *   This function relies on the SAVEWINDOWBYHANDLE function, which performs the actual saving operation.
 *   If any of the row, column, width, or height parameters are omitted, the entire window is saved.
 */
FUNCTION SAVEWINDOW ( cWindowName, cFileName, nRow, nCol, nWidth, nHeight )

   LOCAL ntop, nleft, nbottom, nright

   IF ValType ( cFileName ) = 'U'
      cFileName := GetStartupFolder() + "\" + cWindowName + '.bmp'
   ENDIF

   IF ValType ( nRow ) = 'U' ;
         .OR. ;
         ValType ( nCol ) = 'U' ;
         .OR. ;
         ValType ( nWidth ) = 'U' ;
         .OR. ;
         ValType ( nHeight ) = 'U'

      ntop := -1
      nleft := -1
      nbottom := -1
      nright := -1

   ELSE

      ntop := nRow
      nleft := nCol
      nbottom := nHeight + nRow
      nright := nWidth + nCol

   ENDIF

   SAVEWINDOWBYHANDLE ( GetFormHandle ( cWindowName ), cFileName, ntop, nleft, nbottom, nright )

RETURN NIL
