/*
 * Harbour TGif Class v1.4
 * Copyright 2009-2025 Grigory Filatov <gfilatov@gmail.com>
 *
 * Revised by Ivanil Marcelino <ivanil/at/linkbr.com.br>
 * Last revision 30.10.2020
 */

ANNOUNCE CLASS_TGIF

#include "minigui.ch"

/*-----------------------------------------------------------------------------*
FUNCTION _DefineAniGif ( cControlName, cParentForm, cFilename, nRow, nCol, nWidth, nHeight, nDelay, aBKColor )
*------------------------------------------------------------------------------*
*
*  Description:
*     Defines and initializes an animated GIF control within a MiniGUI form.
*
*  Parameters:
*     cControlName  - The name of the animated GIF control (CHARACTER).  Must be unique within the parent form. If "0", a unique name is generated.
*     cParentForm   - The name of the parent form or dialog where the control will be placed (CHARACTER).
*     cFilename     - The path to the GIF file (CHARACTER). Can be a disk file or a resource name.
*     nRow          - The row position of the control within the parent form (NUMERIC).
*     nCol          - The column position of the control within the parent form (NUMERIC).
*     nWidth        - The width of the control (NUMERIC).
*     nHeight       - The height of the control (NUMERIC).
*     nDelay        - The default delay (in milliseconds) between frames if not specified in the GIF file (NUMERIC).
*     aBKColor      - An array representing the background color of the control (ARRAY).
*
*  Return Value:
*     Returns an object of the TGif class, representing the created animated GIF control.
*
*  Purpose:
*     This function is the core of the animated GIF control integration within HMG Extended.
*     It handles the creation of the control, loading the GIF file, splitting it into individual frames,
*     and setting up a timer to animate the frames.  It also manages resource loading and temporary file creation
*     if the GIF is embedded as a resource.  The function ensures that the control is properly initialized
*     and linked to its parent form.
*
*  Notes:
*     - The function creates temporary files if the GIF is embedded as a resource. These files are deleted when the control is released.
*     - The function relies on the TGif class to handle the actual GIF animation.
*     - Error handling is performed to ensure that the control is properly defined and that the GIF file exists.
*
*/
FUNCTION _DefineAniGif ( cControlName, cParentForm, cFilename, nRow, nCol, nWidth, nHeight, nDelay, aBKColor )
   LOCAL nControlHandle, nParentFormHandle
   LOCAL mVar
   LOCAL k
   LOCAL oGif
   LOCAL cDiskFile
   LOCAL cResName := ""

   // If defined inside DEFINE WINDOW structure, determine cParentForm
   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      cParentForm := iif ( _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
   ENDIF

   IF .NOT. _IsWindowDefined ( cParentForm )
      MsgMiniGuiError ( "Window: " + cParentForm + " is not defined." )
   ENDIF

   IF ISCHAR ( cControlName ) .AND. cControlName == "0"
      cControlName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined ( cControlName, cParentForm )
      MsgMiniGuiError ( "Control: " + cControlName + " Of " + cParentForm + " Already defined." )
   ENDIF

   IF ! ISCHARACTER ( cFilename )
      MsgMiniGuiError ( "Control: " + cControlName + " Of " + cParentForm + " PICTURE Property Invalid Type." )
   ENDIF

   IF Empty ( cFilename )
      MsgMiniGuiError ( "Control: " + cControlName + " Of " + cParentForm + " PICTURE Can't Be Empty." )
   ENDIF

   IF ! hb_FileExists ( cFileName )
      cDiskFile := TempFile ( GetTempFolder(), "gif" )
      IF RCDataToFile ( cFilename, cDiskFile, "GIF" ) > 0
         IF hb_FileExists ( cDiskFile )
            cResName := cFileName
            cFilename := cDiskFile
         ENDIF
      ENDIF
   ENDIF

   // Define public variable associated with control
   mVar := '_' + cParentForm + '_' + cControlName

   nParentFormHandle := GetFormHandle ( cParentForm )

   k := _GetControlFree()

#ifdef _NAMES_LIST_
   _SetNameList( mVar, k )
#else
   PUBLIC &mVar. := k
#endif

   _HMG_aControlType[ k ] := "ANIGIF"
   _HMG_aControlNames[ k ] := cControlName
   _HMG_aControlParentHandles[ k ] := nParentFormHandle
   _HMG_aControlProcedures[ k ] := ""
   _HMG_aControlPageMap[ k ] := {}
   _HMG_aControlValue[ k ] := 0
   _HMG_aControlInputMask[ k ] := ""
   _HMG_aControllostFocusProcedure[ k ] := ""
   _HMG_aControlGotFocusProcedure[ k ] := ""
   _HMG_aControlChangeProcedure[ k ] := ""
   _HMG_aControlDeleted[ k ] := .F.
   _HMG_aControlBkColor[ k ] := aBKColor
   _HMG_aControlFontColor[ k ] := NIL
   _HMG_aControlDblClick[ k ] := ""
   _HMG_aControlHeadClick[ k ] := {}
   _HMG_aControlRow[ k ] := nRow
   _HMG_aControlCol[ k ] := nCol
   _HMG_aControlWidth[ k ] := nWidth
   _HMG_aControlHeight[ k ] := nHeight
   _HMG_aControlSpacing[ k ] := nDelay
   _HMG_aControlContainerRow[ k ] := -1
   _HMG_aControlContainerCol[ k ] := -1
   _HMG_aControlPicture[ k ] := cResName
   _HMG_aControlContainerHandle[ k ] := 0
   _HMG_aControlFontName[ k ] := NIL
   _HMG_aControlFontSize[ k ] := NIL
   _HMG_aControlFontAttributes[ k ] := {}
   _HMG_aControlToolTip[ k ] := ''
   _HMG_aControlRangeMin[ k ] := 0
   _HMG_aControlRangeMax[ k ] := 0
   _HMG_aControlCaption[ k ] := cFilename
   _HMG_aControlVisible[ k ] := .T.
   _HMG_aControlHelpId[ k ] := 0
   _HMG_aControlFontHandle[ k ] := NIL
   _HMG_aControlBrushHandle[ k ] := 0
   _HMG_aControlEnabled[ k ] := .T.
   _HMG_aControlMiscData1[ k ] := 0
   _HMG_aControlMiscData2[ k ] := ''

   oGif := TGif():New( cFilename, nRow, nCol, nHeight, nWidth, nDelay, aBKColor, cControlName, cParentForm )

   IF ISOBJECT ( oGif )
      nControlHandle := GetControlHandle ( oGif:hGif, cParentForm )
      _HMG_aControlHandles[ k ] := nControlHandle
      _HMG_aControlIds[ k ] := oGif

      IF _HMG_BeginTabActive
         AAdd ( _HMG_ActiveTabCurrentPageMap, nControlHandle )
      ENDIF
   ENDIF

   IF hb_FileExists ( cDiskFile )
      FErase ( cDiskFile )
   ENDIF

RETURN oGif

/*-----------------------------------------------------------------------------*
PROCEDURE _ReleaseAniGif ( GifName, FormName )
*------------------------------------------------------------------------------*
*
*  Description:
*     Releases the resources associated with an animated GIF control.
*
*  Parameters:
*     GifName  - The name of the animated GIF control to release (CHARACTER).
*     FormName - The name of the parent form containing the control (CHARACTER).
*
*  Return Value:
*     None. This procedure releases resources and modifies global arrays.
*
*  Purpose:
*     This procedure is responsible for properly releasing the resources associated with an animated GIF control
*     when it is no longer needed. This includes stopping the animation, deleting temporary files,
*     and removing the control from the internal HMG Extended control arrays.  This prevents memory leaks
*     and ensures that the application remains stable.
*
*  Notes:
*     - It's crucial to call this procedure when a form containing an animated GIF control is closed or when the control is no longer needed.
*     - The procedure iterates through the control arrays to find the correct GIF control to release.
*
*/
PROCEDURE _ReleaseAniGif ( GifName, FormName )
   LOCAL hWnd
   LOCAL oGif
   LOCAL i

   // Check if the GIF control name exists in the control names array
   IF AScan ( _HMG_aControlNames, GifName ) > 0

      hWnd := GetFormHandle ( FormName )

      // Iterate through the control handles array to find the GIF control
      FOR i := 1 TO Len ( _HMG_aControlHandles )

         // Check if the control is an ANIGIF and belongs to the specified form
         IF _HMG_aControlParentHandles[ i ] == hWnd .AND. _HMG_aControlType[ i ] == "ANIGIF"
            oGif := _HMG_aControlIds[ i ]
            oGif:End() // Call the End() method of the TGif class to release resources
            _EraseGifDef ( FormName, i ) // Erase the control definition from the HMG arrays
            EXIT
         ENDIF

      NEXT i

   ENDIF

RETURN

/*-----------------------------------------------------------------------------*
STATIC PROCEDURE _EraseGifDef ( FormName, i )
*------------------------------------------------------------------------------*
*
*  Description:
*     Erases the definition of an animated GIF control from the HMG Extended control arrays.
*
*  Parameters:
*     FormName - The name of the parent form containing the control (CHARACTER).
*     i        - The index of the control in the HMG Extended control arrays (NUMERIC).
*
*  Return Value:
*     None. This procedure modifies global arrays.
*
*  Purpose:
*     This procedure is a helper function for _ReleaseAniGif. It removes the control's information
*     from the HMG Extended control arrays, effectively deleting the control definition.  This is necessary
*     to prevent memory leaks and ensure that the control is no longer accessible.
*
*  Notes:
*     - This procedure is called by _ReleaseAniGif after the TGif object's End() method has been called.
*     - The procedure sets various control properties to NIL or empty values to release memory.
*
*/
STATIC PROCEDURE _EraseGifDef ( FormName, i )
   LOCAL mVar

   mVar := '_' + FormName + '_' + _HMG_aControlNames[ i ]

#ifdef _NAMES_LIST_
   _DelNameList( mVar )
#else
   IF __mvExist( mVar )
   #ifndef _PUBLIC_RELEASE_
      __mvPut( mVar, 0 )
   #else
      __mvXRelease( mVar )
   #endif
   ENDIF
#endif

   _HMG_aControlDeleted[ i ] := .T.
   _HMG_aControlType[ i ] := ""
   _HMG_aControlNames[ i ] := ""
   _HMG_aControlHandles[ i ] := 0
   _HMG_aControlParentHandles[ i ] := 0
   _HMG_aControlIds[ i ] := 0
   _HMG_aControlProcedures[ i ] := ""
   _HMG_aControlPageMap[ i ] := {}
   _HMG_aControlValue[ i ] := NIL
   _HMG_aControlInputMask[ i ] := ""
   _HMG_aControllostFocusProcedure[ i ] := ""
   _HMG_aControlGotFocusProcedure[ i ] := ""
   _HMG_aControlChangeProcedure[ i ] := ""
   _HMG_aControlBkColor[ i ] := NIL
   _HMG_aControlFontColor[ i ] := NIL
   _HMG_aControlDblClick[ i ] := ""
   _HMG_aControlHeadClick[ i ] := {}
   _HMG_aControlRow[ i ] := 0
   _HMG_aControlCol[ i ] := 0
   _HMG_aControlWidth[ i ] := 0
   _HMG_aControlHeight[ i ] := 0
   _HMG_aControlSpacing[ i ] := 0
   _HMG_aControlContainerRow[ i ] := 0
   _HMG_aControlContainerCol[ i ] := 0
   _HMG_aControlPicture[ i ] := ''
   _HMG_aControlContainerHandle[ i ] := 0
   _HMG_aControlFontName[ i ] := ''
   _HMG_aControlFontSize[ i ] := 0
   _HMG_aControlToolTip[ i ] := ''
   _HMG_aControlRangeMin[ i ] := 0
   _HMG_aControlRangeMax[ i ] := 0
   _HMG_aControlCaption[ i ] := ''
   _HMG_aControlVisible[ i ] := .F.
   _HMG_aControlHelpId[ i ] := 0
   _HMG_aControlFontHandle[ i ] := 0
   _HMG_aControlFontAttributes[ i ] := {}
   _HMG_aControlBrushHandle[ i ] := 0
   _HMG_aControlEnabled[ i ] := .F.
   _HMG_aControlMiscData1[ i ] := 0
   _HMG_aControlMiscData2[ i ] := ''

RETURN


#include "hbclass.ch"

// Define the TGif class, which manages GIF display and animation within the application
CLASS TGif

   // Define the data attributes
   DATA hGif           // Handle for the GIF control
   DATA cFilename      // Filename of the GIF to display
   DATA cParentName    // Name of the parent control or window containing this GIF
   DATA cControlName   // Control name for identifying this GIF in MiniGUI
   DATA aPictData      // Array to store each frame of the GIF as an image file
   DATA aImageData     // Array to store additional data for each frame (e.g., delay times)
   DATA nTotalFrames   // Total number of frames in the GIF
   DATA nCurrentFrame  // Current frame being displayed
   DATA nDelay         // Default delay between frames if not provided in GIF
   DATA aDelay         // Array of delays for each frame
   DATA cTimer         // Timer control name for managing GIF animation

   // Define methods for the TGif class
   METHOD New( cFileName, nTop, nLeft, nBottom, nRight, nDelay, aBKColor, cControlName, cParentName )
   METHOD PlayGif()       // PlayGif method to update the frame being displayed
   METHOD Play() INLINE GifPlay( Self )  // Shortcut method to start playing the GIF
   METHOD Update()        // Update GIF position and size if the control is moved or resized
   METHOD Stop() INLINE GifStop( Self )  // Shortcut method to stop GIF playback
   METHOD RestartGif()    // RestartGif method to reload and start the GIF from the beginning
   METHOD Restart() INLINE ::RestartGif() // Shortcut to restart the GIF
   METHOD IsRunning() INLINE GifIsRunning( Self )  // Check if GIF is currently running
   METHOD End()           // End method to release resources when the GIF control is closed

ENDCLASS


/*-----------------------------------------------------------------------------*
METHOD New( cFileName, nTop, nLeft, nBottom, nRight, nDelay, aBKColor, cControlName, cParentName ) CLASS TGif
*------------------------------------------------------------------------------*
*
*  Description:
*     Constructor for the TGif class. Initializes a new TGif object and sets up the GIF display.
*
*  Parameters:
*     cFileName     - The path to the GIF file (CHARACTER).
*     nTop          - The top position of the control within the parent form (NUMERIC).
*     nLeft         - The left position of the control within the parent form (NUMERIC).
*     nBottom       - The bottom position of the control within the parent form (NUMERIC).
*     nRight        - The right position of the control within the parent form (NUMERIC).
*     nDelay        - The default delay (in milliseconds) between frames if not specified in the GIF file (NUMERIC).
*     aBKColor      - An array representing the background color of the control (ARRAY).
*     cControlName  - The name of the animated GIF control (CHARACTER).
*     cParentName   - The name of the parent form containing the control (CHARACTER).
*
*  Return Value:
*     Returns a reference to the newly created TGif object (OBJECT).
*
*  Purpose:
*     This method is the constructor for the TGif class. It initializes the object's properties,
*     loads the GIF file, splits it into individual frames, creates an image control to display the GIF,
*     and sets up a timer to animate the frames.  It uses the LoadGif function to load the GIF data
*     and the HMG Extended control definition syntax to create the image control.
*
*  Notes:
*     - The method uses temporary files to store the individual frames of the GIF. These files are deleted when the control is released.
*     - The method relies on the LoadGif function to handle the actual GIF loading and splitting.
*
*/
METHOD New( cFileName, nTop, nLeft, nBottom, nRight, nDelay, aBKColor, cControlName, cParentName ) CLASS TGif

   LOCAL nId               // Unique ID for the control
   LOCAL aPictures := {}   // Array to hold individual frames from the GIF
   LOCAL aImageInfo := {}  // Array for storing each frame’s metadata (like delays)

   // Set default values for optional parameters
   hb_default( @cParentName, _HMG_ActiveFormName )
   hb_default( @nTop, 0 )
   hb_default( @nLeft, 0 )
   hb_default( @nBottom, 100 )
   hb_default( @nRight, 100 )
   hb_default( @nDelay, 10 )

   // Assign properties based on constructor arguments
   ::cParentName := cParentName
   ::cControlName := cControlName
   ::cFileName := cFileName
   ::nDelay := nDelay

   // Load GIF frames and metadata; if loading fails, set placeholders
   IF ! LoadGif( cFileName, @aPictures, @aImageInfo, Self )
      aPictures := { "" }
      aImageInfo := { "" }
   ENDIF

   ::nTotalFrames := Len( aPictures )  // Set total frames count
   ::nCurrentFrame := 1                // Start from the first frame

   // Store copies of frame data and metadata for display
   ::aPictData := AClone( aPictures )
   ::aImageData := AClone( aImageInfo )

   // Create a unique ID and handle for the GIF control
   nId := _GetId()
   ::hGif := cControlName + hb_ntos( nId )

   // Define the GIF display control with specified properties
   @ nTop, nLeft IMAGE ( ::hGif ) PARENT ( cParentName ) PICTURE cFileName ;
      WIDTH nRight HEIGHT nBottom STRETCH BACKGROUNDCOLOR aBKColor TRANSPARENT

   // If GIF has multiple frames, create a timer for animation control
   IF ::nTotalFrames > 1
      ::cTimer := "tgif_tmr_" + hb_ntos( nId )
      DEFINE TIMER ( ::cTimer ) ;
         OF ( cParentName ) ;
         INTERVAL ::aDelay[ ::nCurrentFrame ] ;
         ACTION ::PlayGif() // Timer triggers PlayGif to advance frames

      // Set the display to the first frame
      SetProperty( cParentName, ::hGif, "Picture", ::aPictData[ ::nCurrentFrame ] )
   ENDIF

RETURN Self


/*-----------------------------------------------------------------------------*
METHOD PlayGif() CLASS TGif
*------------------------------------------------------------------------------*
*
*  Description:
*     Advances the animated GIF to the next frame.
*
*  Parameters:
*     None.
*
*  Return Value:
*     NIL.
*
*  Purpose:
*     This method is called by the timer control to update the displayed frame of the animated GIF.
*     It increments the current frame counter, loops back to the first frame if necessary,
*     and updates the Picture property of the image control to display the next frame.  It also updates
*     the timer's interval to match the delay for the current frame.
*
*  Notes:
*     - This method is called automatically by the timer control.
*     - The method assumes that the GIF has been loaded and split into individual frames.
*
*/
METHOD PlayGif() CLASS TGif

   // Move to the next frame, looping back if needed
   IF ::nCurrentFrame < ::nTotalFrames
      ::nCurrentFrame++
   ELSE
      ::nCurrentFrame := 1
   ENDIF

   // Update the displayed frame and adjust timer for next frame’s delay
   SetProperty( ::cParentName, ::hGif, "Picture", ::aPictData[ ::nCurrentFrame ] )
   SetProperty( ::cParentName, ::cTimer, "Value", ::aDelay[ ::nCurrentFrame ] )

RETURN NIL


/*-----------------------------------------------------------------------------*
METHOD Update() CLASS TGif
*------------------------------------------------------------------------------*
*
*  Description:
*     Updates the position and size of the GIF control if the underlying control has been moved or resized.
*
*  Parameters:
*     None.
*
*  Return Value:
*     NIL.
*
*  Purpose:
*     This method is used to synchronize the position and size of the GIF image control with the
*     position and size of the control that was used to define the GIF.  This is necessary because
*     the GIF is actually displayed using an image control, but the user defines the GIF using a custom
*     control definition.  This method ensures that the image control is always in the correct location
*     and has the correct dimensions.
*
*  Notes:
*     - This method should be called whenever the parent control is moved or resized.
*
*/
METHOD Update() CLASS TGif

   IF ! Empty( ::hGif ) .AND. _IsControlDefined ( ::hGif, ::cParentName )
      IF GetProperty( ::cParentName, ::hGif, "Row" ) <> GetProperty( ::cParentName, ::cControlName, "Row" ) .OR. ;
            GetProperty( ::cParentName, ::hGif, "Col" ) <> GetProperty( ::cParentName, ::cControlName, "Col" ) .OR. ;
            GetProperty( ::cParentName, ::hGif, "Width" ) <> GetProperty( ::cParentName, ::cControlName, "Width" ) .OR. ;
            GetProperty( ::cParentName, ::hGif, "Height" ) <> GetProperty( ::cParentName, ::cControlName, "Height" )

         // Sync the GIF display’s properties with the control
         SetProperty( ::cParentName, ::hGif, "Row", GetProperty( ::cParentName, ::cControlName, "Row" ) )
         SetProperty( ::cParentName, ::hGif, "Col", GetProperty( ::cParentName, ::cControlName, "Col" ) )
         SetProperty( ::cParentName, ::hGif, "Width", GetProperty( ::cParentName, ::cControlName, "Width" ) )
         SetProperty( ::cParentName, ::hGif, "Height", GetProperty( ::cParentName, ::cControlName, "Height" ) )
      ENDIF
   ENDIF

RETURN NIL


/*-----------------------------------------------------------------------------*
METHOD RestartGif() CLASS TGif
*------------------------------------------------------------------------------*
*
*  Description:
*     Reloads the GIF file and restarts the animation from the beginning.
*
*  Parameters:
*     None.
*
*  Return Value:
*     NIL.
*
*  Purpose:
*     This method is used to reload the GIF file and restart the animation from the first frame.
*     This is useful if the GIF file has been modified or if the animation needs to be restarted for
*     any other reason.  The method stops the current animation, deletes the existing frame images,
*     reloads the GIF data, and restarts the animation.
*
*  Notes:
*     - This method deletes the temporary files created for the GIF frames.
*
*/
METHOD RestartGif() CLASS TGif

   LOCAL aPictures := {}, aImageInfo := {}

   GifStop( Self )  // Stop current animation

   // Remove any previously loaded frames
   AEval( ::aPictData, {| f | FErase( f ) } )

   // Reload GIF frames and metadata
   IF LoadGif( ::cFileName, @aPictures, @aImageInfo, Self )
      ::nTotalFrames := Len( aPictures )
      ::aPictData := AClone( aPictures )
      ::aImageData := AClone( aImageInfo )
      ::nCurrentFrame := 1
      ::Update()  // Sync display
   ENDIF

   ::Play()  // Restart animation

RETURN NIL


/*-----------------------------------------------------------------------------*
METHOD End() CLASS TGif
*------------------------------------------------------------------------------*
*
*  Description:
*     Releases the resources associated with the TGif object.
*
*  Parameters:
*     None.
*
*  Return Value:
*     NIL.
*
*  Purpose:
*     This method is responsible for releasing all resources associated with the TGif object,
*     including deleting temporary files, releasing the timer control, and releasing the image control.
*     This is crucial to prevent memory leaks and ensure that the application remains stable.
*
*  Notes:
*     - This method should be called when the form containing the GIF control is closed or when the control is no longer needed.
*
*/
METHOD End() CLASS TGif

   IF _IsControlDefined ( ::cControlName, ::cParentName )
      AEval( ::aPictData, {| f | FErase( f ) } )  // Delete frame images

      // Release timer if there is animation
      IF ::nTotalFrames > 1
         DoMethod( ::cParentName, ::cTimer, 'Release' )
      ENDIF

      // Release GIF control
      IF _IsControlDefined( ::hGif, ::cParentName )
         _ReleaseControl( ::hGif, ::cParentName )
      ENDIF
   ENDIF

RETURN NIL


/*
 *  Auxiliary Static Functions
 */

/*-----------------------------------------------------------------------------*
STATIC FUNCTION GifPlay( oGif )
*------------------------------------------------------------------------------*
*
*  Description:
*     Enables the timer to start the GIF animation.
*
*  Parameters:
*     oGif - A reference to the TGif object (OBJECT).
*
*  Return Value:
*     NIL.
*
*  Purpose:
*     This function is a helper function that enables the timer associated with the TGif object,
*     thereby starting the GIF animation.  It's used as an inline function for the Play() method.
*
*  Notes:
*     - This function only enables the timer if the GIF has more than one frame.
*
*/
STATIC FUNCTION GifPlay( oGif )

   IF oGif:nTotalFrames > 1
      SetProperty( oGif:cParentName, oGif:cTimer, 'Enabled', .T. )
   ENDIF

RETURN NIL


/*-----------------------------------------------------------------------------*
STATIC FUNCTION GifStop( oGif )
*------------------------------------------------------------------------------*
*
*  Description:
*     Disables the timer to stop the GIF animation.
*
*  Parameters:
*     oGif - A reference to the TGif object (OBJECT).
*
*  Return Value:
*     NIL.
*
*  Purpose:
*     This function is a helper function that disables the timer associated with the TGif object,
*     thereby stopping the GIF animation.  It's used as an inline function for the Stop() method.
*
*  Notes:
*     - This function only disables the timer if the GIF has more than one frame.
*
*/
STATIC FUNCTION GifStop( oGif )

   IF oGif:nTotalFrames > 1
      SetProperty( oGif:cParentName, oGif:cTimer, 'Enabled', .F. )
   ENDIF

RETURN NIL


/*-----------------------------------------------------------------------------*
STATIC FUNCTION GifIsRunning( oGif )
*------------------------------------------------------------------------------*
*
*  Description:
*     Checks if the GIF animation is currently running.
*
*  Parameters:
*     oGif - A reference to the TGif object (OBJECT).
*
*  Return Value:
*     .T. if the GIF animation is running, .F. otherwise (LOGICAL).
*
*  Purpose:
*     This function is a helper function that checks if the timer associated with the TGif object is enabled,
*     indicating whether the GIF animation is currently running.  It's used as an inline function for the IsRunning() method.
*
*  Notes:
*     - This function only checks the timer if the GIF has more than one frame.
*
*/
STATIC FUNCTION GifIsRunning( oGif )

   LOCAL lRunning := .F.

   IF oGif:nTotalFrames > 1
      lRunning := GetProperty( oGif:cParentName, oGif:cTimer, 'Enabled' )
   ENDIF

RETURN lRunning


/*
 * Author: P.Chornyj <myorg63@mail.ru>
 */

#include "fileio.ch"

/*-----------------------------------------------------------------------------*
FUNCTION LoadGif ( GIF, aFrames, aImgInfo, oGif )
*------------------------------------------------------------------------------*
*
*  Description:
*     Loads a GIF file, extracts individual frames, and retrieves frame delay information.
*
*  Parameters:
*     GIF     - The path to the GIF file (CHARACTER).  Specifies the location of the GIF file to be processed.
*     aFrames - An array to store the file names of the extracted GIF frames (ARRAY).  This array is passed by reference and will be populated with the paths to the extracted frame files.
*     aImgInfo - An array to store image information (ARRAY). This array is passed by reference and will be modified by the function. Currently not used.
*     oGif    - A TGif object (OBJECT).  The frame delays are stored in the oGif:aDelay property.
*
*  Return Value:
*     .T. if the GIF file was successfully loaded and split into frames, .F. otherwise (LOGICAL).
*
*  Purpose:
*     This function is the core of the GIF animation process. It reads a GIF file, identifies individual frames,
*     extracts each frame as a separate GIF file in the temporary folder, and stores the file names of the extracted frames in the aFrames array.
*     It also extracts the delay information for each frame and stores it in the oGif:aDelay array.
*     This information is then used to animate the GIF by displaying the frames in sequence with the correct delays.
*
*     Example Usage:
*       LOCAL aFrames := {}
*       LOCAL aImgInfo := {}
*       LOCAL oGif := TGif():New()
*       IF LoadGif( "MyAnimation.gif", aFrames, aImgInfo, oGif )
*          // Process the extracted frames and delay information
*          FOR i := 1 TO Len( aFrames )
*             // Display aFrames[i] with a delay of oGif:aDelay[i]
*          NEXT
*       ENDIF
*
*  Notes:
*     - The function creates temporary files for each frame of the GIF in the temporary folder. These files should be deleted after use to avoid cluttering the file system.
*     - The function assumes that the GIF file is in a valid format. Invalid GIF files may cause unexpected behavior or errors.
*     - The function uses a static variable nID to generate unique file names for the extracted frames. This ensures that the temporary files do not conflict with each other.
*     - The ReadFromStream function is used to read the GIF file into memory. This allows the function to process the GIF data more efficiently.
*     - The function relies on the GIF file format structure, specifically the GIF header and the Graphic Control Extension block (identified by Chr(0) + Chr(33) + Chr(249)).
*     - The aImgInfo parameter is currently not used but is included for potential future use.
*
*/
FUNCTION LoadGif ( GIF, aFrames, aImgInfo, oGif )
   LOCAL cPath := GetTempFolder()  // Get the path to the temporary folder.
   LOCAL cGifHeader                // Stores the GIF header.
   LOCAL cGifEnd := Chr( 0 ) + Chr( 33 ) + Chr( 249 )  // Marks the end of a GIF frame.
   LOCAL cStream                   // Stores the entire GIF file content as a string.
   LOCAL cFile                     // Stores the file name of the extracted GIF frame.
   LOCAL cPicBuf                   // Stores the content of a single GIF frame.
   LOCAL imgHeader                 // Stores the header of a single GIF frame.
   LOCAL nImgCount                 // Counts the number of extracted GIF frames.
   LOCAL nFileHandle               // File handle for file I/O operations.
   LOCAL i, j                      // Loop counters.

   STATIC nID := 0  // Static variable to generate unique file names for extracted frames.

   nID++  // Increment the static ID for each call to the function.

   oGif:aDelay := {}           // Initialize the delay array in the TGif object.
   hb_default( @aFrames, {} )  // Initialize aFrames to an empty array if it's not already initialized.
   hb_default( @aImgInfo, {} ) // Initialize aImgInfo to an empty array if it's not already initialized.

   IF ! ReadFromStream( GIF, @cStream )  // Read the GIF file into the cStream variable.
      RETURN FALSE  // Return .F. if the file could not be read.
   ENDIF

   nImgCount := 0  // Initialize the frame counter.
   i := 1          // Initialize the starting position for parsing the GIF file.
   j := At( cGifEnd, cStream, i ) + 1  // Find the first occurrence of the GIF frame end marker.
   cGifHeader = Left( cStream, j )     // Extract the GIF header.

   i := j + 2      // Update the starting position for the next frame.

   /* Split GIF Files at separate pictures and load them into ImageList */

   DO WHILE .T.    // Loop through the GIF file to extract frames.

      nImgCount++  // Increment the frame counter.

      j := At( cGifEnd, cStream, i ) + 3  // Find the end of the current frame.

      IF j > Len( cGifEnd )  // Check if a valid frame end marker was found.
         cFile := cPath + hb_ps() + cFileNoExt( GIF ) + "_frame_" + hb_ntos( nID ) + "_" + StrZero( nImgCount, 4 ) + ".gif"  // Construct the file name for the extracted frame.
         nFileHandle := FCreate( cFile, FC_NORMAL )  // Create the file.
         IF FError() <> 0  // Check for file creation errors.
            RETURN FALSE  // Return .F. if there was an error creating the file.
         ENDIF

         cPicBuf := cGifHeader + SubStr( cStream, i - 1, j - i )  // Extract the frame data.
         imgHeader = Left( SubStr ( cStream, i - 1, j - i ), 16 ) // Extract the image header.

         IF FWrite( nFileHandle, cPicBuf ) <> Len( cPicBuf )  // Write the frame data to the file.
            RETURN FALSE  // Return .F. if there was an error writing to the file.
         ENDIF

         IF .NOT. FClose( nFileHandle )  // Close the file.
            RETURN FALSE  // Return .F. if there was an error closing the file.
         ENDIF

         AAdd( aFrames, cFile )  // Add the file name to the aFrames array.
         AAdd( oGif:aDelay, GetFrameDelay( imgHeader, oGif:nDelay ) )  // Add the frame delay to the oGif:aDelay array.
      ENDIF

      DO EVENTS  // Process events to keep the application responsive.

      IF j == 3  // Check if the end of the GIF file has been reached.
         EXIT    // Exit the loop if the end of the file has been reached.
      ELSE
         i := j  // Update the starting position for the next frame.
      ENDIF

   ENDDO

   IF i < Len( cStream )  // Handle the case where there is remaining data after the loop.

      cFile := cPath + hb_ps() + cFileNoExt( GIF ) + "_frame_" + hb_ntos( nID ) + "_" + StrZero( ++nImgCount, 4 ) + ".gif"  // Construct the file name for the extracted frame.
      nFileHandle := FCreate( cFile, FC_NORMAL )  // Create the file.
      IF FError() <> 0  // Check for file creation errors.
         RETURN FALSE   // Return .F. if there was an error creating the file.
      ENDIF

      cPicBuf := cGifHeader + SubStr( cStream, i - 1, Len( cStream ) - i )  // Extract the frame data.
      imgHeader := Left( SubStr( cStream, i - 1, Len( cStream ) - i ), 16 ) // Extract the image header.

      IF FWrite( nFileHandle, cPicBuf ) <> Len( cPicBuf )  // Write the frame data to the file.
         RETURN FALSE  // Return .F. if there was an error writing to the file.
      ENDIF

      IF .NOT. FClose( nFileHandle )  // Close the file.
         RETURN FALSE  // Return .F. if there was an error closing the file.
      ENDIF

      AAdd( aFrames, cFile )  // Add the file name to the aFrames array.
      AAdd( oGif:aDelay, GetFrameDelay( imgHeader, oGif:nDelay ) )  // Add the frame delay to the oGif:aDelay array.

   ENDIF

RETURN TRUE  // Return .T. if the GIF file was successfully processed.

/*------------------------------------------------------------------------------*
STATIC FUNCTION ReadFromStream( cFile, cStream )
*------------------------------------------------------------------------------*
*
*  Description:
*     Reads the entire content of a file into a string variable.
*
*  Parameters:
*     cFile   - The path to the file to be read (CHARACTER).
*     cStream - A variable (passed by reference) to store the file content as a string (CHARACTER).
*
*  Return Value:
*     .T. if the file was successfully read, .F. otherwise (LOGICAL).
*
*  Purpose:
*     This function reads the entire content of a file into a string variable.
*     It is used to load the GIF file into memory for processing.
*     This approach allows for efficient parsing and manipulation of the GIF data.
*
*  Notes:
*     - The function reads the entire file into memory. This may not be suitable for very large files.
*     - The function checks for file I/O errors and returns .F. if an error occurs.
*
*/
STATIC FUNCTION ReadFromStream( cFile, cStream )
   LOCAL nFileSize                     // Stores the size of the file.
   LOCAL nFileHandle := FOpen( cFile ) // Open the file for reading.

   IF FError() == 0                    // Check if the file was opened successfully.
      nFileSize := FSeek( nFileHandle, 0, FS_END )  // Get the file size by seeking to the end of the file.
      cStream := Space( nFileSize )    // Allocate memory for the file content.
      FSeek( nFileHandle, 0, FS_SET )  // Seek back to the beginning of the file.
      FRead( nFileHandle, @cStream, nFileSize )  // Read the file content into the cStream variable.
      FClose( nFileHandle )            // Close the file.
   ENDIF

RETURN ( FError() == 0 .AND. .NOT. Empty( cStream ) )  // Return .T. if the file was successfully read and the content is not empty.

/*------------------------------------------------------------------------------*
FUNCTION GetFrameDelay( cImageInfo, nDelay )
*------------------------------------------------------------------------------*
*
*  Description:
*     Extracts the frame delay from the image header and applies a default multiplier.
*
*  Parameters:
*     cImageInfo - The image header (CHARACTER).  A string containing the first 16 bytes of the image data.
*     nDelay     - The default delay multiplier (NUMERIC).  An optional parameter that specifies a multiplier for the extracted delay value. Defaults to 10 if not provided.
*
*  Return Value:
*     The frame delay in milliseconds (NUMERIC).
*
*  Purpose:
*     This function extracts the frame delay information from the image header of a GIF frame.
*     The delay is stored as a 2-byte value in the image header.
*     The function converts this value to a numeric representation and multiplies it by a default value (typically 10) to get the delay in milliseconds.
*     This delay value is then used to control the speed of the GIF animation.
*
*  Notes:
*     - The function assumes that the image header is in a valid format and contains the delay information at the correct offset.
*     - The default delay multiplier (nDelay) can be adjusted to control the overall speed of the GIF animation.
*     - The delay value is in hundredths of a second, so multiplying by 10 converts it to milliseconds.
*
*/
FUNCTION GetFrameDelay( cImageInfo, nDelay )
// Extract the delay value from the image header, convert it to a number, and multiply it by the default delay.
RETURN ( Bin2W( SubStr( cImageInfo, 4, 2 ) ) * hb_defaultValue( nDelay, 10 ) )
