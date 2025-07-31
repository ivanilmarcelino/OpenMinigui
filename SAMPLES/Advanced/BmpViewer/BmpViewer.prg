/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-06 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2003-2006 Grigory Filatov <gfilatov@inbox.ru>
 */

ANNOUNCE RDDSYS

#include "minigui.ch"
#include "winprint.ch"

/*
 * XCOMMAND ON KEY SPACE [ OF <parent> ] ACTION <action>
 *
 * Defines a hotkey for the SPACE key.
 *
 * Parameters:
 *   <parent>: The parent window or control to which the hotkey applies.
 *   <action>: A code block to execute when the SPACE key is pressed.
 *
 * Purpose:
 *   This XCOMMAND simplifies the process of defining hotkeys for specific keys.
 *   It encapsulates the _DefineHotKey function, making the code more readable and maintainable.
 *   In this specific application, it's used to trigger an action when the SPACE key is pressed within a specified window.
 */
#xcommand ON KEY SPACE [ OF <parent> ] ACTION <action> ;
      = > ;
      _DefineHotKey ( < "parent" >, 0, VK_SPACE, < { action } > )

/*
 * XCOMMAND ON KEY GRAYPLUS [ OF <parent> ] ACTION <action>
 *
 * Defines a hotkey for the GRAYPLUS (Add) key.
 *
 * Parameters:
 *   <parent>: The parent window or control to which the hotkey applies.
 *   <action>: A code block to execute when the GRAYPLUS key is pressed.
 *
 * Purpose:
 *   This XCOMMAND simplifies the process of defining hotkeys for specific keys.
 *   It encapsulates the _DefineHotKey function, making the code more readable and maintainable.
 *   In this specific application, it's used to trigger an action when the GRAYPLUS key is pressed within a specified window.
 */
#xcommand ON KEY GRAYPLUS [ OF <parent> ] ACTION <action> ;
      = > ;
      _DefineHotKey ( < "parent" >, 0, VK_ADD, < { action } > )

/*
 * XCOMMAND ON KEY GRAYMINUS [ OF <parent> ] ACTION <action>
 *
 * Defines a hotkey for the GRAYMINUS (Subtract) key.
 *
 * Parameters:
 *   <parent>: The parent window or control to which the hotkey applies.
 *   <action>: A code block to execute when the GRAYMINUS key is pressed.
 *
 * Purpose:
 *   This XCOMMAND simplifies the process of defining hotkeys for specific keys.
 *   It encapsulates the _DefineHotKey function, making the code more readable and maintainable.
 *   In this specific application, it's used to trigger an action when the GRAYMINUS key is pressed within a specified window.
 */
#xcommand ON KEY GRAYMINUS [ OF <parent> ] ACTION <action> ;
      = > ;
      _DefineHotKey ( < "parent" >, 0, VK_SUBTRACT, < { action } > )

#define PROGRAM 'BmpViewer'
#define VERSION ' version 1.0'
#define COPYRIGHT ' 2003-2006 by Grigory Filatov'

#define NTRIM( n ) LTrim( Str( n ) )
#define MsgAlert( c ) MsgEXCLAMATION( c, "Error", , .f. )

DECLARE WINDOW Form_Info

STATIC nWidth, nHeight, nColor, cFile := "", nKoef := 1, aFiles := {}, nItem := 1, nLoadTime

MEMVAR nScrWidth, nScrHeight

/*
 * PROCEDURE Main( fname )
 *
 * Initializes the application, defines the main window, and displays a BMP image.
 *
 * Parameters:
 *   fname (STRING, optional): The filename of the BMP image to open on startup. If omitted, the application starts without loading a file.
 *
 * Purpose:
 *   This is the main entry point of the application. It performs the following tasks:
 *     1. Sets up the main window with menus, a toolbar, an image control, and a status bar.
 *     2. Defines hotkeys for various actions (e.g., opening, printing, zooming).
 *     3. Loads the specified BMP image (if provided) and displays it in the image control.
 *     4. Handles user interactions, such as clicking on the image to view the next image in the directory.
 *     5. Manages the zoom level of the displayed image.
 *
 * Notes:
 *   The application assumes that all BMP files are located in the current directory.
 *   The zoom functionality is limited to a range of 0.11 to 9.9.
 */
PROCEDURE Main( fname )
   LOCAL nLen, aRect := GetDesktopArea()
   PRIVATE nScrWidth := aRect[ 3 ] - aRect[ 1 ], nScrHeight := aRect[ 4 ] - aRect[ 2 ]

   DEFAULT fname := ""

   cFile := fname
   aFiles := Directory( "*.BMP" )
   nLen := Len( aFiles )
   nItem := AScan( aFiles, {| e | Upper( e[ 1 ] ) = Upper( cFile ) } )

   SET CENTURY ON
   SET DATE GERMAN

   DEFINE WINDOW Form_Main ;
         WIDTH 433 HEIGHT 447 ;
         TITLE PROGRAM ;
         ICON 'IMAGE' ;
         MAIN ;
         ON INIT iif( Empty( fname ), , FileOpen( fname, .F. ) ) ;
         FONT 'MS Sans Serif' ;
         SIZE 9

      DEFINE MAIN MENU

         DEFINE POPUP "&File"
            MENUITEM "&Open" + Chr( 9 ) + "   Ctrl+O" ACTION FileOpen()
            SEPARATOR
            MENUITEM "&Print" + Chr( 9 ) + "   Ctrl+P" ACTION FilePrint()
            SEPARATOR
            MENUITEM "E&xit" + Chr( 9 ) + "   Esc" ACTION ReleaseAllWindows()
         END POPUP

         DEFINE POPUP "&View"
            MENUITEM "&Information" + Chr( 9 ) + "   Ctrl+I" ACTION PictInfo()
            SEPARATOR
            MENUITEM "&Zoom In" + Chr( 9 ) + "   Gray +" ACTION Zoom( 1 )
            MENUITEM "Zoom &Out" + Chr( 9 ) + "   Gray -" ACTION Zoom( -1 )
            MENUITEM "O&riginal size" + Chr( 9 ) + "   Ctrl+H" ACTION Zoom( 0 )
         END POPUP

         DEFINE POPUP "&Help"
            MENUITEM "&About" ACTION MsgAbout()
         END POPUP

      END MENU

      @ 6, 210 TEXTBOX Text_1 HEIGHT 20 WIDTH 80 VALUE IF( Empty( nLen ), "", NTRIM( nItem ) + "/" + NTRIM( nLen ) )

      DEFINE SPLITBOX

         DEFINE TOOLBAR ToolBar_1 BUTTONSIZE 20, 20 FLAT

            BUTTON Button_1 ;
               PICTURE 'Open' ;
               TOOLTIP 'Open' ;
               ACTION FileOpen()

            BUTTON Button_2 ;
               PICTURE 'Print' ;
               TOOLTIP 'Print' ;
               ACTION FilePrint() SEPARATOR

            BUTTON Button_3 ;
               PICTURE 'Info' ;
               TOOLTIP 'Info' ;
               ACTION PictInfo()

            BUTTON Button_4 ;
               PICTURE 'Zout' ;
               TOOLTIP 'Zoom out' ;
               ACTION Zoom( 1 )

            BUTTON Button_5 ;
               PICTURE 'Zin' ;
               TOOLTIP 'Zoom in' ;
               ACTION Zoom( -1 )

            BUTTON Button_6 ;
               PICTURE 'Left' ;
               TOOLTIP 'Previous' ;
               ACTION Next( -1 )

            BUTTON Button_7 ;
               PICTURE 'Right' ;
               TOOLTIP 'Next' ;
               ACTION Next( 1 )

         END TOOLBAR

      END SPLITBOX

      Form_Main.Button_2.Enabled := ! Empty( nLen )
      Form_Main.Button_3.Enabled := ! Empty( nLen )
      Form_Main.Button_4.Enabled := ! Empty( nLen )
      Form_Main.Button_5.Enabled := ! Empty( nLen )
      Form_Main.Button_6.Enabled := ( nLen > 1 )
      Form_Main.Button_7.Enabled := ( nLen > 1 )

      @ 35, 0 IMAGE Image_1 PICTURE "IMAGE" ;
         ON CLICK Next( 1 ) ;
         WIDTH 421 HEIGHT 330

      DEFINE STATUSBAR

         STATUSITEM "No file loaded" WIDTH 114
         STATUSITEM "" WIDTH 60
         STATUSITEM "" WIDTH 52 ACTION Zoom( 0 )
         STATUSITEM "" WIDTH 80
         STATUSITEM "" WIDTH 150

      END STATUSBAR

      ON KEY ESCAPE ACTION ReleaseAllWindows()

      ON KEY F12 ACTION Form_Main.MINIMIZE

      ON KEY CONTROL + O ACTION FileOpen()

      ON KEY CONTROL + P ACTION FilePrint()

      ON KEY GRAYPLUS ACTION Zoom( 1 )

      ON KEY GRAYMINUS ACTION Zoom( -1 )

      ON KEY CONTROL + I ACTION PictInfo()

      ON KEY CONTROL + H ACTION Zoom( 0 )

      ON KEY LEFT ACTION Next( -1 )

      ON KEY RIGHT ACTION Next( 1 )

      ON KEY UP ACTION ( nItem := 1, Next( 0 ) )

      ON KEY DOWN ACTION ( nItem := Len( aFiles ), Next( 0 ) )

      ON KEY BACK ACTION Next( -1 )

      ON KEY SPACE ACTION Next( 1 )

   END WINDOW

   CENTER WINDOW Form_Main

   ACTIVATE WINDOW Form_Main

RETURN

/*
 * STATIC PROCEDURE FileOpen( fname, lInit )
 *
 * Opens a BMP image file and displays it in the main window.
 *
 * Parameters:
 *   fname (STRING, optional): The filename of the BMP image to open. If omitted, a file open dialog is displayed.
 *   lInit (LOGICAL, optional): A flag indicating whether to re-initialize the file list. Defaults to .T. (true).
 *
 * Purpose:
 *   This procedure handles the opening of BMP image files. It performs the following steps:
 *     1. If fname is empty, it displays a file open dialog to allow the user to select a BMP file.
 *     2. If a valid filename is provided (either through the parameter or the dialog), it updates the cFile variable.
 *     3. If lInit is true, it rebuilds the list of BMP files in the current directory.
 *     4. Resets the zoom level to 1.
 *     5. Retrieves the dimensions and color depth of the BMP image.
 *     6. Calculates the optimal window size to fit the image within the screen boundaries, adjusting the zoom level if necessary.
 *     7. Updates the main window's title, status bar, and image control to reflect the newly opened image.
 *
 * Notes:
 *   The procedure assumes that all BMP files are located in the current directory.
 *   The window size calculation ensures that the image is fully visible within the screen boundaries.
 */
STATIC PROCEDURE FileOpen( fname, lInit )
   LOCAL cPath := "\" + CurDir() + IF( Empty( CurDir() ), "", "\" ), ;
      aSize, nLen := Len( aFiles ), nFormWidth, nFormHeight, nStart

   DEFAULT fname := "", lInit := .T.

   IF Empty( fname )
      fname := GetFile( { { "Image files (*.bmp)", "*.bmp" } }, "Open", cPath )
   ELSE
      fname := IF( At( '"', fname ) > 0, StrTran( fname, '"', '' ), fname )
   ENDIF

   IF ! Empty( fname ) .AND. File( fname )
      cFile := fname
      IF lInit
         aFiles := Directory( "*.BMP" )
         nLen := Len( aFiles )
         nItem := AScan( aFiles, {| e | Upper( e[ 1 ] ) = Upper( cFileNoPath( cFile ) ) } )
      ENDIF
      nKoef := 1

      aSize := BmpSize( cFile )
      nWidth := IFEMPTY( aSize[ 1 ], 640, aSize[ 1 ] )
      nHeight := IFEMPTY( aSize[ 2 ], 480, aSize[ 2 ] )
      nColor := aSize[ 3 ]

      DO WHILE .T.
         nFormWidth := Round( nWidth * nKoef, 0 ) + GetBorderWidth() + 4
         nFormHeight := Round( nHeight * nKoef, 0 ) + GetTitleHeight() + GetBorderHeight() + GetMenuBarHeight() + GetToolBarHeight() + Form_Main.Statusbar.HEIGHT + 2
         IF ( nFormWidth <= nScrWidth .AND. nFormHeight <= nScrHeight ) .OR. nKoef < 0.11
            EXIT
         ENDIF
         nKoef -= 0.01
      ENDDO

      Form_Main.WIDTH := IF( nFormWidth < 420, 420, nFormWidth )
      Form_Main.HEIGHT := IF( nFormHeight < 100, 100, nFormHeight )

      IF ! Form_Main.Button_2.Enabled .OR. lInit
         Form_Main.Button_2.Enabled := .T.
         Form_Main.Button_3.Enabled := .T.
         Form_Main.Button_4.Enabled := .T.
         Form_Main.Button_5.Enabled := .T.
         Form_Main.Button_6.Enabled := ( nLen > 1 )
         Form_Main.Button_7.Enabled := ( nLen > 1 )
      ENDIF
      Form_Main.Text_1.VALUE := NTRIM( nItem ) + "/" + NTRIM( nLen )

      Form_Main.TITLE := cFileNoPath( fname ) + ' - ' + PROGRAM + ;
         IF( nKoef # 1, " (Zoom: " + NTRIM( Round( nKoef * nWidth, 0 ) ) + " x " + NTRIM( Round( nKoef * nHeight, 0 ) ) + ")", "" )
      Form_Main.StatusBar.Item( 1 ) := NTRIM( nWidth ) + " x " + NTRIM( nHeight ) + " x " + NTRIM( nColor ) + " BPP"
      Form_Main.StatusBar.Item( 2 ) := NTRIM( nItem ) + "/" + NTRIM( nLen )
      Form_Main.StatusBar.Item( 3 ) := Str( nKoef * 100, 4 ) + " %"
      Form_Main.StatusBar.Item( 4 ) := NTRIM( aFiles[ nItem ][ 2 ] / 1024 ) + " KB"
      Form_Main.StatusBar.Item( 5 ) := DToC( aFiles[ nItem ][ 3 ] ) + " / " + aFiles[ nItem ][ 4 ]

      IF IsControlDefined( Image_1, Form_Main )
         Form_Main.Image_1.RELEASE
      ENDIF

      nStart := Seconds()

      @ 35, 0 IMAGE Image_1 OF Form_Main ;
         PICTURE cFile ;
         ON CLICK Next( 1 ) ;
         WIDTH nKoef * nWidth ;
         HEIGHT nKoef * nHeight

      nLoadTime := Seconds() - nStart

      Form_Main.Image_1.SetFocus
      Form_Main.Center()

   ENDIF

RETURN

/*
 * STATIC PROCEDURE Zoom( nOp )
 *
 * Zooms the displayed image in or out.
 *
 * Parameters:
 *   nOp (NUMERIC):  A value indicating the zoom operation:
 *     - Positive value (e.g., 1): Zoom in.
 *     - Negative value (e.g., -1): Zoom out.
 *     - Zero (0): Reset to original size (100% zoom).
 *
 * Purpose:
 *   This procedure allows the user to zoom in or out on the displayed image. It calculates the new zoom level based on the nOp parameter and updates the image control and window size accordingly.
 *   The zoom level is adjusted in increments of 0.1, with a minimum zoom level of 0.11 and a maximum zoom level of 9.9.
 *   If nOp is 0, the zoom level is reset to 1 (100%).
 *
 * Notes:
 *   The procedure checks if an image is currently loaded before performing the zoom operation.
 *   The window size is adjusted to fit the zoomed image within the screen boundaries.
 */
STATIC PROCEDURE Zoom( nOp )
   LOCAL nPictWidth, nPictHeight

   IF Form_Main.StatusBar.Item( 1 ) == "No file loaded"
      RETURN
   ENDIF

   IF Empty( nOp ) .AND. nKoef == 1
      RETURN
   ENDIF

   IF Int( nKoef * 10 ) # Round( nKoef, 1 ) * 10
      nKoef := Int( nKoef * 10 ) / 10
   ENDIF

   IF nOp < 0 .AND. nKoef > 0.11
      nKoef -= 0.1
   ELSEIF nOp > 0 .AND. nKoef < 9.9
      nKoef += 0.1
   ELSE
      nKoef := 1
   ENDIF

   nPictWidth := Round( nWidth * nKoef, 0 )
   nPictHeight := Round( nHeight * nKoef, 0 )

   Form_Main.WIDTH := nPictWidth + GetBorderWidth() + 4
   Form_Main.HEIGHT := nPictHeight + GetTitleHeight() + GetBorderHeight() + GetMenuBarHeight() + GetToolBarHeight() + Form_Main.Statusbar.HEIGHT + 2

   IF Form_Main.Width < 420
      Form_Main.WIDTH := 420
   ENDIF
   IF Form_Main.Height < 100
      Form_Main.HEIGHT := 100
   ENDIF

   Form_Main.TITLE := cFileNoPath( cFile ) + ' - ' + PROGRAM + ;
      IF( nKoef # 1, " (Zoom: " + NTRIM( nPictWidth ) + " x " + NTRIM( nPictHeight ) + ")", "" )
   Form_Main.StatusBar.Item( 3 ) := Str( nKoef * 100, 4 ) + " %"

   IF IsControlDefined( Image_1, Form_Main )
      Form_Main.Image_1.RELEASE
   ENDIF

   @ 35, 0 IMAGE Image_1 OF Form_Main ;
      PICTURE cFile ;
      ON CLICK Next( 1 ) ;
      WIDTH nKoef * nWidth ;
      HEIGHT nKoef * nHeight

   Form_Main.Image_1.SetFocus
   Form_Main.Center()

RETURN

/*
 * STATIC PROCEDURE PictInfo()
 *
 * Displays a modal window with information about the currently loaded image.
 *
 * Parameters:
 *   None
 *
 * Purpose:
 *   This procedure displays a modal window containing details about the currently loaded BMP image, such as:
 *     - Filename
 *     - Folder
 *     - Original size (width x height in pixels)
 *     - Color depth (bits per pixel)
 *     - Disk size (in KB and bytes)
 *     - Current folder index (position in the list of BMP files)
 *     - File date and time
 *     - Load time (in milliseconds)
 *
 * Notes:
 *   The information is displayed in a modal window, which prevents the user from interacting with the main window until the information window is closed.
 *   The procedure checks if an image is currently loaded before displaying the information.
 */
STATIC PROCEDURE PictInfo()
   LOCAL aLabel := {}, cLabel, aText := {}, cText, n

   IF ! Empty( cFile )

      AAdd( aLabel, "File name:" )
      AAdd( aLabel, "Folder:" )
      AAdd( aLabel, "Original size:" )
      AAdd( aLabel, "Current colors:" )
      AAdd( aLabel, "Disk size:" )
      AAdd( aLabel, "Current folder index:" )
      AAdd( aLabel, "File date/time:" )
      AAdd( aLabel, "Loaded in:" )

      AAdd( aText, cFileNoPath( cFile ) )
      AAdd( aText, CurDrive() + ":\" + CurDir() + IF( Empty( CurDir() ), "", "\" ) )
      AAdd( aText, NTRIM( nWidth ) + " x " + NTRIM( nHeight ) + " Pixels" )
      AAdd( aText, NTRIM( nColor ) + " Bit Per Pixel" )
      AAdd( aText, NTRIM( aFiles[ nItem ][ 2 ] / 1024 ) + " KB (" + NTRIM( aFiles[ nItem ][ 2 ] ) + " Bytes)" )
      AAdd( aText, NTRIM( nItem ) + " / " + NTRIM( Len( aFiles ) ) )
      AAdd( aText, DToC( aFiles[ nItem ][ 3 ] ) + " / " + aFiles[ nItem ][ 4 ] )
      AAdd( aText, NTRIM( Int( nLoadTime * 1000 ) ) + " milliseconds" )

      DEFINE WINDOW Form_Info AT 0, 0 ;
            WIDTH 360 HEIGHT 300 ;
            TITLE PROGRAM + " - Image properties" ;
            MODAL ;
            NOSIZE ;
            NOSYSMENU ;
            ON INIT Form_Info.Button_OK.SetFocus ;
            FONT 'MS Sans Serif' ;
            SIZE 9

         @ 8, 8 FRAME Frame_1 ;
            WIDTH 338 ;
            HEIGHT 212

         FOR n := 1 TO Len( aLabel )
            cLabel := "Label_" + NTRIM( n )
            @ (n - 1 ) * 24 + 23, 20 LABEL &cLabel ;
               VALUE aLabel[ n ] ;
               WIDTH 100 HEIGHT 20
         NEXT

         FOR n := 1 TO Len( aText )
            cText := "Text_" + NTRIM( n )
            @ (n - 1 ) * 24 + 21, 134 TEXTBOX &cText ;
               VALUE aText[ n ] ;
               HEIGHT 20 ;
               WIDTH 200 ;
               READONLY
         NEXT

         @ Form_Info.HEIGHT - 68, Form_Info.Width / 2 - 50 BUTTON Button_OK ;
            CAPTION "OK" ;
            ACTION Form_Info.RELEASE ;
            WIDTH 96 HEIGHT 32 - iif( IsThemed(), 6, 0 ) ;
            DEFAULT

      END WINDOW

      CENTER WINDOW Form_Info

      ACTIVATE WINDOW Form_Info

   ENDIF

RETURN

/*
 * STATIC PROCEDURE Next( nOp )
 *
 * Displays the next or previous image in the directory.
 *
 * Parameters:
 *   nOp (NUMERIC): A value indicating the direction to navigate:
 *     - Negative value (e.g., -1): Previous image.
 *     - Positive value (e.g., 1): Next image.
 *     - Zero (0): Reload current image.
 *
 * Purpose:
 *   This procedure allows the user to navigate through the list of BMP images in the current directory.
 *   It updates the nItem variable to point to the next or previous image in the aFiles array and then calls the FileOpen() procedure to display the selected image.
 *   If there is only one image in the directory, it updates the text box with the current item number.
 *
 * Notes:
 *   The procedure handles wrapping around the beginning and end of the file list.
 */
STATIC PROCEDURE Next( nOp )
   LOCAL nLen := Len( aFiles )

   IF nLen > 1
      IF nOp < 0
         nItem--
         nItem := IF( nItem < 1, nLen, nItem )
      ELSEIF nOp > 0
         nItem++
         nItem := IF( nItem > nLen, 1, nItem )
      ENDIF

      FileOpen( aFiles[ nItem ][ 1 ], .F. )
   ELSE
      Form_Main.Text_1.VALUE := NTRIM( nItem ) + "/" + NTRIM( nLen )
   ENDIF

RETURN

/*
 * STATIC PROCEDURE FilePrint()
 *
 * Prints the currently displayed image.
 *
 * Parameters:
 *   None
 *
 * Purpose:
 *   This procedure allows the user to print the currently displayed BMP image. It performs the following steps:
 *     1. Initializes the printing system.
 *     2. Displays a print dialog to allow the user to select a printer and configure print settings.
 *     3. Sets the print units to millimeters and the paper size to A4.
 *     4. Determines the optimal paper orientation (portrait or landscape) based on the image dimensions.
 *     5. Sets the print quality to high and the color mode to color.
 *     6. Enables print preview.
 *     7. Starts the print job and draws the image on the page, scaling it to fit within the printable area.
 *     8. Releases the printing system.
 *
 * Notes:
 *   The procedure uses the WINPRINT.CH library for printing functionality.
 *   The image is scaled to fit within the printable area while maintaining its aspect ratio.
 */
STATIC PROCEDURE FilePrint()
   LOCAL nScale := 1 / 3.937, nX, nY, nH, nW

   IF ! Empty( cFile ) .AND. File( cFile )

      INIT PRINTSYS

      SELECT BY DIALOG

      IF HBPRNERROR != 0
         RETURN
      ENDIF

      SET UNITS MM // Sets @... units to milimeters
      SET PAPERSIZE DMPAPER_A4 // Sets paper size to A4

      IF nHeight >= nWidth
         SET ORIENTATION PORTRAIT // Sets paper orientation to portrait
         nH := 250
         nW := 170
      ELSE
         SET ORIENTATION LANDSCAPE // Sets paper orientation to landscape
         nH := 170
         nW := 250
      ENDIF

      SET BIN DMBIN_FIRST // Use first bin
      SET QUALITY DMRES_HIGH // Sets print quality to high
      SET COLORMODE DMCOLOR_COLOR // Set print color mode to color
      SET PREVIEW ON // Enables print preview
      SET PREVIEW RECT 0, 0, nScrHeight, nScrWidth

      START DOC NAME Left( PROGRAM, 9 )

         START PAGE

         DO WHILE .T.

            nX := Round( nHeight * nScale, 0 )
            nY := Round( nWidth * nScale, 0 )

            IF ( nX <= nH .AND. nY <= nW ) .OR. nScale < 0.15
               EXIT
            ENDIF

            nScale -= 0.05

         ENDDO

         @ 15, 20 PICTURE cFile SIZE nX, nY

         END PAGE

      END DOC

      RELEASE PRINTSYS

   ENDIF

RETURN

/*
 * STATIC FUNCTION GetToolBarHeight()
 *
 * Returns the height of the toolbar in the main window.
 *
 * Parameters:
 *   None
 *
 * Return Value:
 *   NUMERIC: The height of the toolbar in pixels.
 *
 * Purpose:
 *   This function is used to calculate the height of the toolbar, which is needed to determine the correct size and position of other controls in the main window.
 *   It retrieves the handle of the toolbar control and then calls the GetSizeToolBar() function to get the toolbar's dimensions.
 *
 * Notes:
 *   The function adds twice the border height to the toolbar height to account for the toolbar's borders.
 */
STATIC FUNCTION GetToolBarHeight()
   LOCAL h := Form_Main.ToolBar_1.Handle

RETURN ( LoWord( GetSizeToolBar( h ) ) + 2 * GetBorderHeight() )

/*
 * STATIC FUNCTION MsgAbout()
 *
 * Displays an "About" message box with application information.
 *
 * Parameters:
 *   None
 *
 * Return Value:
 *   LOGICAL: The return value of the MsgInfo() function (typically .T.).
 *
 * Purpose:
 *   This function displays a message box containing information about the application, such as:
 *     - Program name and version
 *     - Copyright information
 *     - Compiler and Harbour version
 *     - MiniGUI version
 *     - A message indicating that the program is freeware
 *
 * Notes:
 *   The function uses the MsgInfo() function to display the message box.
 */
STATIC FUNCTION MsgAbout()
*

RETURN MsgInfo( PadC( PROGRAM + VERSION, 40 ) + CRLF + ;
      "Copyright " + Chr( 169 ) + COPYRIGHT + CRLF + CRLF + ;
      hb_Compiler() + CRLF + Version() + CRLF + ;
      Left( MiniGuiVersion(), 38 ) + CRLF + CRLF + ;
      PadC( "This program is Freeware!", 40 ), "About", , .F. )
