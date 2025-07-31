/*
 * Harbour MiniGUI Animation Demo
 */

#include "minigui.ch"
#include "Directry.ch"

/*
 * PROCEDURE Main()
 *
 * Defines the main window and its components for the animation demo.
 *
 * Purpose:
 *   This is the entry point of the application. It defines the main window,
 *   including its title, icon, menu, and image controls. It also sets up the
 *   window's initial properties and activates it. This procedure is crucial
 *   for setting up the user interface and initiating the animation.
 *
 * Notes:
 *   The window's MinButton and MaxButton properties are set to .F. to disable
 *   minimizing and maximizing. The ClientHeight is adjusted to accommodate the
 *   menu bar. The Image controls are initially defined with PICTURE NIL, which
 *   will be updated by the timers.
 */
PROCEDURE Main()

   DEFINE WINDOW Win_1 ;
         CLIENTAREA 220, 130 ;
         TITLE 'Animation Demo' ;
         ICON "MAIN.ICO" ;
         WINDOWTYPE MAIN ;
         ON INIT Image_OnInit( This.Name )

      DEFINE MAIN MENU
         DEFINE POPUP 'Info'
            ITEM 'About ..' ACTION MsgInfo( 'Animation Demo' )
            SEPARATOR
            ITEM 'Exit' ACTION Win_1.RELEASE
         END POPUP
      END MENU

      Win_1.MinButton := .F.
      Win_1.MaxButton := .F.
      Win_1.ClientHeight := ( Win_1.ClientHeight ) + GetMenuBarHeight()

      DEFINE IMAGE Image_1
         ROW 15
         COL 15
         WIDTH -1
         HEIGHT -1
         PICTURE NIL
         STRETCH .T.
      END IMAGE

      DEFINE IMAGE Image_2
         ROW 5
         COL 100
         WIDTH -1
         HEIGHT -1
         PICTURE NIL
         STRETCH .T.
      END IMAGE

   END WINDOW

   CENTER WINDOW Win_1
   ACTIVATE WINDOW Win_1

RETURN

#define IMG_PATH             'Images\'

/*
 * PROCEDURE Image_OnInit( cWin )
 *
 * Initializes the image animation by loading image files and setting up timers.
 *
 * Parameters:
 *   cWin: The name of the parent window.  This is passed by the ON INIT event of the window.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This procedure is called when the main window is initialized. It loads the
 *   image files for the animations from the 'Images\' directory, creates two
 *   timers, and sets their actions to update the PICTURE property of the Image
 *   controls, creating the animation effect. This procedure is essential for
 *   loading the animation frames and starting the animation sequences.
 *
 * Notes:
 *   The procedure uses STATIC variables aFrames, aImages, i1, and i2 to maintain
 *   the state of the animation sequences between timer events. The Directory()
 *   function is used to retrieve the list of image files. The AEval() function
 *   is used to populate the aFrames and aImages arrays with the full path to
 *   each image file. The timers are defined with different intervals to create
 *   different animation speeds. The iif() function is used to reset the animation
 *   sequence when it reaches the end of the array of images.
 */
PROCEDURE Image_OnInit( cWin )

   LOCAL aImgFiles
   STATIC aFrames := {}, aImages := {}, i1 := 1, i2 := 1

   // Retrieve the list of image files for the first animation sequence (WolkingMan).
   aImgFiles := Directory( IMG_PATH + 'WolkingMan?.png' )

   // Populate the aFrames array with the full path to each image file.
   AEval( aImgFiles, {| elem | AAdd( aFrames, ( IMG_PATH + elem[ F_NAME ] ) ) } )

   // Define the first timer to control the first animation sequence.
   DEFINE TIMER Timer_1
      PARENT &cWin
      INTERVAL 60 // Interval in milliseconds.  Lower values result in faster animation.
      ACTION ( Win_1.Image_1.PICTURE := aFrames[ i1++ ], i1 := iif( i1 > Len( aFrames ), 1, i1 ) )
      // The ACTION block updates the PICTURE property of Image_1 with the next frame
      // from the aFrames array. The i1 variable is incremented to move to the next frame.
      // The iif() function resets i1 to 1 when it exceeds the number of frames,
      // creating a looping animation.
   END TIMER

   // Retrieve the list of image files for the second animation sequence (User).
   aImgFiles := Directory( IMG_PATH + 'User?.png' )

   // Populate the aImages array with the full path to each image file.
   AEval( aImgFiles, {| elem | AAdd( aImages, ( IMG_PATH + elem[ F_NAME ] ) ) } )

   // Define the second timer to control the second animation sequence.
   DEFINE TIMER Timer_2
      PARENT &cWin
      INTERVAL 80 // Interval in milliseconds.
      ACTION ( Win_1.Image_2.PICTURE := aImages[ i2++ ], i2 := iif( i2 > Len( aImages ), 1, i2 ) )
      // The ACTION block updates the PICTURE property of Image_2 with the next frame
      // from the aImages array. The i2 variable is incremented to move to the next frame.
      // The iif() function resets i2 to 1 when it exceeds the number of frames,
      // creating a looping animation.
   END TIMER

RETURN
