/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * This program demonstrates how to access and capture images from a web camera.
 *
 * Copyright 2011-2022 Grigory Filatov <gfilatov@gmail.ru>
 */

#include "minigui.ch"
#include "BosTaurus.ch"

// Define constants for window and control sizes
#define WIN_WIDTH   440
#define WIN_HEIGHT  300
#define CAM_WIDTH   250
#define CAM_HEIGHT  210
#define IMG_WIDTH   150
#define IMG_HEIGHT  110

*-----------------------------------------------------------------------------*
PROCEDURE Main
*-----------------------------------------------------------------------------*
// This is the main function where the application starts.
// It defines the main window and its controls.

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH WIN_WIDTH + GetBorderWidth() ;
      HEIGHT WIN_HEIGHT + GetTitleHeight() + GetBorderHeight() ;
      TITLE 'WebCam Preview Demo' ;
      MAIN ;
      NOMAXIMIZE NOSIZE ;
      ON INIT ( ;
                 CaptureImage() ;  // capture initialization
              ) ;
      ON RELEASE ( ;
                 CloseWebCam() ;
                 ) ;
      ON RESTORE ( ;
                 CaptureImage() ;  // capture initialization
                 )
/*
   // Define a WEBCAM control using standard syntax.
   @ 60, 20 WEBCAM WebCam_1 ;
      WIDTH 250 HEIGHT 210 ;
      RATE 20 ;
      START
*/
   // Define the WEBCAM control to display the camera feed.
   DEFINE WEBCAM WebCam_1
      ROW 60
      COL 20
      WIDTH CAM_WIDTH
      HEIGHT CAM_HEIGHT
      CAM_RATE 20
      CAM_START .T.
   END WEBCAM

   // Define an IMAGE control to display captured images.
   DEFINE IMAGE Image_1
      ROW 120
      COL 280
      WIDTH   IMG_WIDTH
      HEIGHT  IMG_HEIGHT
      STRETCH .T. // Stretch the image to fit the control
   END IMAGE

   // Define a BUTTON to start the webcam.
   DEFINE BUTTON Button_1
      ROW 10
      COL 20
      WIDTH   120
      CAPTION 'Start WebCam'
      ACTION  ( CreateWebCam(), CaptureImage() )
   END BUTTON

   // Define a BUTTON to stop the webcam.
   DEFINE BUTTON Button_2
      ROW 10
      COL 150
      WIDTH   120
      CAPTION 'Stop WebCam'
      ACTION  CloseWebCam()
   END BUTTON

   // Define a BUTTON to capture an image.
   DEFINE BUTTON Button_3
      ROW 80
      COL 315
      WIDTH   80
      CAPTION 'Capture'
      ACTION  CaptureImage()
   END BUTTON

   // Define LABEL controls to provide visual boundaries.
   DEFINE LABEL Label_1
      ROW 59
      COL 19
      WIDTH   CAM_WIDTH + 2
      HEIGHT  CAM_HEIGHT + 2
      BORDER  .T.
   END LABEL

   DEFINE LABEL Label_2
      ROW 119
      COL 279
      WIDTH   IMG_WIDTH + 2
      HEIGHT  IMG_HEIGHT + 2
      BORDER  .T.
   END LABEL

   // Close the window on ESC key
   ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN

// This function initializes the webcam.
*-----------------------------------------------------------------------------*
PROCEDURE CreateWebCam
*-----------------------------------------------------------------------------*

   // Check if the webcam control is already defined.
   IF ! IsControlDefined( WebCam_1, Form_1 )
/*
      // Define the WEBCAM control using standard syntax.
      @ 60, 20 WEBCAM WebCam_1 OF Form_1 ;
         WIDTH 250 HEIGHT 210 ;
         RATE 20
*/
      // Define the WEBCAM control using the alternative syntax.
      DEFINE WEBCAM WebCam_1
         PARENT Form_1
         ROW 60
         COL 20
         WIDTH 250
         HEIGHT 210
         CAM_RATE 20
      END WEBCAM

      // Enable the capture button.
      Form_1.Button_3.Enabled := .T.

      // Start the webcam control.
      Form_1.WebCam_1.Start()
      DO EVENTS

   ENDIF

RETURN

// This function closes the webcam and releases resources.
*-----------------------------------------------------------------------------*
PROCEDURE CloseWebCam
*-----------------------------------------------------------------------------*

   // Check if the webcam control is defined.
   IF IsControlDefined( WebCam_1, Form_1 )

      Form_1.WebCam_1.Release()
      DO EVENTS

      // Clear the image control.
      Form_1.Image_1.hBitmap := Nil

      // Disable the capture button.
      Form_1.Button_3.Enabled := .F.

   ENDIF

RETURN

// This function captures an image from the webcam and displays it in the image control.
*-----------------------------------------------------------------------------*
PROCEDURE CaptureImage
*-----------------------------------------------------------------------------*
   LOCAL hBitmap
   LOCAL nWidth
   LOCAL nHeight

   // Check if the webcam control is defined.
   IF GetControlIndex( 'WebCam_1', 'Form_1' ) > 0

      // Copy the current frame from the webcam to the clipboard.
      IF cap_EditCopy( GetControlHandle ( 'WebCam_1', 'Form_1' ) )

         DO EVENTS

         // Get the dimensions of the image control.
         nWidth := GetProperty( "Form_1", "Image_1", "Width" )
         nHeight := GetProperty( "Form_1", "Image_1", "Height" )

         // Get the bitmap from the clipboard.
         hBitmap := BT_BitmapClipboardGet( 'Form_1' )

         // Check if the bitmap is valid.
         IF ! Empty( hBitmap )

            DO EVENTS

            // Resize the bitmap to fit the image control.
            Form_1.Image_1.hBitmap := BT_BitmapCopyAndResize( hBitmap, nWidth, nHeight, NIL, BT_RESIZE_COLORONCOLOR )

            // Save the bitmap to a file (optional).
            BT_BitmapSaveFile( hBitmap, "webcam.jpg", BT_FILEFORMAT_JPG )

            // Release the bitmap resources.
            BT_BitmapRelease( hBitmap )

            // Clean the clipboard.
            BT_BitmapClipboardClean( 'Form_1' )

            DO EVENTS

         ELSE

            MsgAlert( 'Could not retrieve bitmap from clipboard!', 'Error' )

         ENDIF

      ELSE

         MsgAlert( 'Capture is failure!', 'Error' )

      ENDIF

   ENDIF

RETURN
