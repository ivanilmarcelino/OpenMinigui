/*
 * Harbour TGif Class v1.5
 * Copyright 2009-2026 Grigory Filatov <gfilatov@gmail.com>
 *
 * Revised by Ivanil Marcelino <ivanil/at/linkbr.com.br>
 * Last revision 30.10.2020
 */

ANNOUNCE CLASS_TGIF

#include "minigui.ch"

/*-----------------------------------------------------------------------------*
FUNCTION _DefineAniGif()
*------------------------------------------------------------------------------*
   Purpose:
     Creates and initializes an animated GIF control in an HMG Extended form.
     Registers the control in the framework's internal arrays and returns a TGif object.

   Parameters:
     cControlName  - Control name (if "0", auto-generated)
     cParentForm   - Parent window/dialog name
     cFilename     - GIF file path or resource name
     nRow, nCol    - Position on form
     nWidth, nHeight - Control dimensions
     nDelay        - Default frame delay (ms) if not specified in GIF
     aBKColor      - Background color array

   Returns:
     TGif object reference

   Side effects:
     Creates temporary files (if resource), registers control, starts animation timer.
*/
FUNCTION _DefineAniGif ( cControlName, cParentForm, cFilename, nRow, nCol, nWidth, nHeight, nDelay, aBKColor )
   LOCAL nControlHandle, nParentFormHandle
   LOCAL mVar
   LOCAL k
   LOCAL oGif
   LOCAL cDiskFile
   LOCAL cResName := ""

   // Resolve parent form when used inside DEFINE WINDOW / DEFINE DIALOG
   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      cParentForm := iif ( _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
   ENDIF

   IF .NOT. _IsWindowDefined ( cParentForm )
      MsgMiniGuiError ( "Window: " + cParentForm + " is not defined." )
   ENDIF

   // Auto-generate unique name if requested
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

   // Handle GIF embedded as resource by extracting to temp file
   IF ! hb_FileExists ( cFileName )
      cDiskFile := TempFile ( GetTempFolder(), "gif" )
      IF RCDataToFile ( cFilename, cDiskFile, "GIF" ) > 0
         IF hb_FileExists ( cDiskFile )
            cResName := cFileName
            cFilename := cDiskFile
         ENDIF
      ENDIF
   ENDIF

   // Register control in HMG internal arrays
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

   // Create the actual TGif object that handles animation
   oGif := TGif():New( cFilename, nRow, nCol, nHeight, nWidth, nDelay, aBKColor, cControlName, cParentForm )

   IF ISOBJECT ( oGif )
      nControlHandle := GetControlHandle ( oGif:hGif, cParentForm )
      _HMG_aControlHandles[ k ] := nControlHandle
      _HMG_aControlIds[ k ] := oGif

      IF _HMG_BeginTabActive
         AAdd ( _HMG_ActiveTabCurrentPageMap, nControlHandle )
      ENDIF
   ENDIF

   // Clean up temporary extraction file if created from resource
   IF hb_FileExists ( cDiskFile )
      FErase ( cDiskFile )
   ENDIF

RETURN oGif


/*-----------------------------------------------------------------------------*
PROCEDURE _ReleaseAniGif()
*------------------------------------------------------------------------------*
   Purpose:
     Releases all resources associated with an ANIGIF control.

   Parameters:
     GifName   - Name of the GIF control
     FormName  - Parent form name

   Side effects:
     Stops timer, deletes temporary frame files, removes control from HMG arrays.
*/
PROCEDURE _ReleaseAniGif ( GifName, FormName )
   LOCAL hWnd
   LOCAL oGif
   LOCAL i

   IF AScan ( _HMG_aControlNames, GifName ) > 0
      hWnd := GetFormHandle ( FormName )

      FOR i := 1 TO Len ( _HMG_aControlHandles )
         IF _HMG_aControlParentHandles[ i ] == hWnd .AND. _HMG_aControlType[ i ] == "ANIGIF"
            oGif := _HMG_aControlIds[ i ]
            oGif:End()
            _EraseGifDef ( FormName, i )
            EXIT
         ENDIF
      NEXT i
   ENDIF

RETURN


/*-----------------------------------------------------------------------------*
STATIC PROCEDURE _EraseGifDef()
*------------------------------------------------------------------------------*
   Purpose:
     Removes control definition from HMG internal arrays to free memory.

   Parameters:
     FormName - Parent form name
     i        - Index in _HMG arrays
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

   // Clear all control metadata
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
#include "fileio.ch"


CLASS TGif

   DATA hGif           // Internal IMAGE control name used for display
   DATA cFileName      // Original GIF filename
   DATA cParentName    // Parent form name
   DATA cControlName   // Public control name
   DATA cTimer         // Timer control name for animation

   DATA aPictData      // Array of temporary frame file paths
   DATA aImageData     // Frame metadata (currently unused)
   DATA aDelay         // Delay in ms for each frame

   DATA nTotalFrames   // Total number of frames
   DATA nCurrentFrame  // Current frame index (1-based)
   DATA nDelay         // Default delay multiplier

   METHOD New( cFileName, nTop, nLeft, nBottom, nRight, ;
               nDelay, aBKColor, cControlName, cParentName )

   METHOD PlayGif()
   METHOD Play()       INLINE GifPlay( Self )

   METHOD Update()

   METHOD Stop()       INLINE GifStop( Self )

   METHOD RestartGif()
   METHOD Restart()    INLINE ::RestartGif()

   METHOD IsRunning()  INLINE GifIsRunning( Self )

   METHOD End()

ENDCLASS


/*-----------------------------------------------------------------------------*
METHOD TGif:New()
*------------------------------------------------------------------------------*
   Purpose:
     Constructor. Loads GIF, splits into frames, creates HMG IMAGE control and timer.
*/
METHOD New( cFileName, nTop, nLeft, nBottom, nRight, ;
            nDelay, aBKColor, cControlName, cParentName ) CLASS TGif

   LOCAL nId
   LOCAL aPictures  := {}
   LOCAL aImageInfo := {}

   hb_default( @cParentName, _HMG_ActiveFormName )
   hb_default( @nTop,        0 )
   hb_default( @nLeft,       0 )
   hb_default( @nBottom,     100 )
   hb_default( @nRight,      100 )
   hb_default( @nDelay,      10 )

   ::cParentName  := cParentName
   ::cControlName := cControlName
   ::cFileName    := cFileName
   ::nDelay       := nDelay

   ::aPictData    := {}
   ::aImageData   := {}
   ::aDelay       := {}

   ::nTotalFrames := 0
   ::nCurrentFrame := 1

   // Load and split GIF into individual frame files
   IF ! LoadGif( cFileName, @aPictures, @aImageInfo, Self )
      aPictures  := { "" }
      aImageInfo := { "" }
      ::aDelay   := { nDelay }
   ENDIF

   ::aPictData    := AClone( aPictures )
   ::aImageData   := AClone( aImageInfo )
   ::nTotalFrames := Len( ::aPictData )

   nId    := _GetId()
   ::hGif := cControlName + hb_ntos( nId )

   // Create underlying IMAGE control (visible frame holder)
   @ nTop, nLeft IMAGE ( ::hGif ) ;
      PARENT ( cParentName ) ;
      PICTURE cFileName ;
      WIDTH nRight ;
      HEIGHT nBottom ;
      STRETCH ;
      BACKGROUNDCOLOR aBKColor ;
      TRANSPARENT

   // Setup animation timer only for multi-frame GIFs
   IF ::nTotalFrames > 1
      ::cTimer := "tgif_tmr_" + hb_ntos( nId )

      DEFINE TIMER ( ::cTimer ) ;
         OF ( cParentName ) ;
         INTERVAL ::aDelay[ ::nCurrentFrame ] ;
         ACTION ::PlayGif()

      // Show first frame
      SetProperty( ::cParentName, ::hGif, "Picture", ::aPictData[ ::nCurrentFrame ] )
   ENDIF

RETURN Self


/*-----------------------------------------------------------------------------*
METHOD TGif:PlayGif()
*------------------------------------------------------------------------------*
   Purpose:
     Timer callback. Advances to next frame and updates timer interval.
*/
METHOD PlayGif() CLASS TGif

   IF ::nCurrentFrame < ::nTotalFrames
      ::nCurrentFrame++
   ELSE
      ::nCurrentFrame := 1
   ENDIF

   SetProperty( ::cParentName, ::hGif, "Picture", ::aPictData[ ::nCurrentFrame ] )
   SetProperty( ::cParentName, ::cTimer, "Value", ::aDelay[ ::nCurrentFrame ] )

RETURN NIL


/*-----------------------------------------------------------------------------*
METHOD TGif:Update()
*------------------------------------------------------------------------------*
   Purpose:
     Synchronizes internal image control position/size with public control.
     Required because ANIGIF is a virtual control wrapping an IMAGE.
*/
METHOD Update() CLASS TGif

   LOCAL nGifRow, nGifCol, nGifWidth, nGifHeight
   LOCAL nCtlRow, nCtlCol, nCtlWidth, nCtlHeight

   IF Empty( ::hGif ) .OR. ! _IsControlDefined( ::hGif, ::cParentName )
      RETURN NIL
   ENDIF

   nGifRow    := GetProperty( ::cParentName, ::hGif,         "Row" )
   nGifCol    := GetProperty( ::cParentName, ::hGif,         "Col" )
   nGifWidth  := GetProperty( ::cParentName, ::hGif,         "Width" )
   nGifHeight := GetProperty( ::cParentName, ::hGif,         "Height" )

   nCtlRow    := GetProperty( ::cParentName, ::cControlName, "Row" )
   nCtlCol    := GetProperty( ::cParentName, ::cControlName, "Col" )
   nCtlWidth  := GetProperty( ::cParentName, ::cControlName, "Width" )
   nCtlHeight := GetProperty( ::cParentName, ::cControlName, "Height" )

   IF nGifRow    != nCtlRow    .OR. ;
      nGifCol    != nCtlCol    .OR. ;
      nGifWidth  != nCtlWidth  .OR. ;
      nGifHeight != nCtlHeight

      SetProperty( ::cParentName, ::hGif, "Row",    nCtlRow )
      SetProperty( ::cParentName, ::hGif, "Col",    nCtlCol )
      SetProperty( ::cParentName, ::hGif, "Width",  nCtlWidth )
      SetProperty( ::cParentName, ::hGif, "Height", nCtlHeight )
   ENDIF

RETURN NIL


/*-----------------------------------------------------------------------------*
METHOD TGif:RestartGif()
*------------------------------------------------------------------------------*
   Purpose:
     Reloads GIF from disk and restarts animation from first frame.
*/
METHOD RestartGif() CLASS TGif

   LOCAL aPictures  := {}
   LOCAL aImageInfo := {}

   ::Stop()

   // Delete old temporary frame files
   AEval( ::aPictData, ;
      {| cFile | IF( File( cFile ), FErase( cFile ), NIL ) } )

   IF LoadGif( ::cFileName, @aPictures, @aImageInfo, Self )
      ::aPictData     := AClone( aPictures )
      ::aImageData    := AClone( aImageInfo )
      ::nTotalFrames  := Len( aPictures )
      ::nCurrentFrame := 1
      ::Update()
   ENDIF

   ::Play()

RETURN NIL


/*-----------------------------------------------------------------------------*
METHOD TGif:End()
*------------------------------------------------------------------------------*
   Purpose:
     Releases all resources: temporary files, timer, and image control.
*/
METHOD End() CLASS TGif

   IF _IsControlDefined( ::cControlName, ::cParentName )
      // Cleanup temporary frame files
      AEval( ::aPictData, ;
         {| cFile | IF( File( cFile ), FErase( cFile ), NIL ) } )

      IF ::nTotalFrames > 1 .AND. _IsControlDefined( ::cTimer, ::cParentName )
         DoMethod( ::cParentName, ::cTimer, "Release" )
      ENDIF

      IF _IsControlDefined( ::hGif, ::cParentName )
         _ReleaseControl( ::hGif, ::cParentName )
      ENDIF
   ENDIF

RETURN NIL


// Helper functions for Play/Stop/IsRunning (used via INLINE methods)

STATIC FUNCTION GifPlay( oGif )
   IF oGif:nTotalFrames > 1
      SetProperty( oGif:cParentName, oGif:cTimer, "Enabled", .T. )
   ENDIF
RETURN NIL


STATIC FUNCTION GifStop( oGif )
   IF oGif:nTotalFrames > 1
      SetProperty( oGif:cParentName, oGif:cTimer, "Enabled", .F. )
   ENDIF
RETURN NIL


STATIC FUNCTION GifIsRunning( oGif )
   LOCAL lRunning := .F.
   IF oGif:nTotalFrames > 1
      lRunning := GetProperty( oGif:cParentName, oGif:cTimer, "Enabled" )
   ENDIF
RETURN lRunning


/*
 * GIF Frame Extraction Engine
 * Author: P.Chornyj <myorg63@mail.ru>
 */

/*-----------------------------------------------------------------------------*
FUNCTION LoadGif()
*------------------------------------------------------------------------------*
   Purpose:
     Reads GIF file and splits it into individual frame files by detecting
     Graphic Control Extension blocks (0x00 0x21 0xF9).

   Parameters:
     cGifFile   - Source GIF path
     aFrames    - OUT: array of temporary frame file paths
     aImgInfo   - OUT: frame metadata (currently limited use)
     oGif       - TGif object (receives aDelay array)

   Returns:
     .T. on success, .F. on failure
*/
FUNCTION LoadGif( cGifFile, aFrames, aImgInfo, oGif )

   LOCAL cTempPath := GetTempFolder()
   LOCAL cGifHeader
   LOCAL cGifMark := Chr( 0 ) + Chr( 33 ) + Chr( 249 )   // Graphic Control Extension marker

   LOCAL cStream
   LOCAL cFrameFile
   LOCAL cFrameData
   LOCAL cImgHeader

   LOCAL nFrameCount
   LOCAL nHandle

   LOCAL i, j

   STATIC nId := 0

   nId++

   hb_default( @aFrames,   {} )
   hb_default( @aImgInfo,  {} )

   oGif:aDelay := {}

   IF ! ReadFromStream( cGifFile, @cStream )
      RETURN .F.
   ENDIF

   nFrameCount := 0
   i := 1
   j := At( cGifMark, cStream, i ) + 1

   cGifHeader := Left( cStream, j )   // Global header + Logical Screen Descriptor + first GCE

   i := j + 2

   DO WHILE .T.
      nFrameCount++

      j := At( cGifMark, cStream, i ) + 3

      IF j > Len( cGifMark )
         cFrameFile := cTempPath + hb_ps() + ;
                       cFileNoExt( cGifFile ) + ;
                       "_frame_" + ;
                       hb_ntos( nId ) + "_" + ;
                       StrZero( nFrameCount, 4 ) + ;
                       ".gif"

         nHandle := FCreate( cFrameFile, FC_NORMAL )
         IF FError() != 0
            RETURN .F.
         ENDIF

         // Reconstruct valid single-frame GIF: global header + frame data
         cFrameData := cGifHeader + SubStr( cStream, i - 1, j - i )
         cImgHeader := Left( SubStr( cStream, i - 1, j - i ), 16 )

         IF FWrite( nHandle, cFrameData ) != Len( cFrameData )
            FClose( nHandle )
            RETURN .F.
         ENDIF

         FClose( nHandle )

         AAdd( aFrames, cFrameFile )
         AAdd( oGif:aDelay, GetFrameDelay( cImgHeader, oGif:nDelay ) )
      ENDIF

      DO EVENTS

      IF j == 3
         EXIT
      ENDIF

      i := j
   ENDDO

   // Handle last frame if any data remains
   IF i < Len( cStream )
      cFrameFile := cTempPath + hb_ps() + ;
                    cFileNoExt( cGifFile ) + ;
                    "_frame_" + ;
                    hb_ntos( nId ) + "_" + ;
                    StrZero( ++nFrameCount, 4 ) + ;
                    ".gif"

      nHandle := FCreate( cFrameFile, FC_NORMAL )
      IF FError() != 0
         RETURN .F.
      ENDIF

      cFrameData := cGifHeader + SubStr( cStream, i - 1, Len( cStream ) - i )
      cImgHeader := Left( SubStr( cStream, i - 1, Len( cStream ) - i ), 16 )

      IF FWrite( nHandle, cFrameData ) != Len( cFrameData )
         FClose( nHandle )
         RETURN .F.
      ENDIF

      FClose( nHandle )

      AAdd( aFrames, cFrameFile )
      AAdd( oGif:aDelay, GetFrameDelay( cImgHeader, oGif:nDelay ) )
   ENDIF

RETURN .T.


/*-----------------------------------------------------------------------------*
STATIC FUNCTION ReadFromStream()
*------------------------------------------------------------------------------*
   Purpose:
     Reads entire file content into a memory string for binary parsing.
*/
STATIC FUNCTION ReadFromStream( cFile, cStream )
   LOCAL nSize
   LOCAL nHandle := FOpen( cFile )

   IF FError() == 0
      nSize := FSeek( nHandle, 0, FS_END )
      cStream := Space( nSize )
      FSeek( nHandle, 0, FS_SET )
      FRead( nHandle, @cStream, nSize )
      FClose( nHandle )
   ENDIF

RETURN ( FError() == 0 .AND. ! Empty( cStream ) )


/*-----------------------------------------------------------------------------*
FUNCTION GetFrameDelay()
*------------------------------------------------------------------------------*
   Purpose:
     Extracts delay from Graphic Control Extension block.
     Delay is stored as 2-byte little-endian value (hundredths of a second).
*/
FUNCTION GetFrameDelay( cImageInfo, nDelay )
RETURN Bin2W( SubStr( cImageInfo, 4, 2 ) ) * hb_defaultValue( nDelay, 10 )
