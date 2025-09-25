#include "minigui.ch"

/*
 * FUNCTION: DefinePDFViewer()
 *
 * DESCRIPTION:
 *   Initializes and embeds a PDF viewer control into an HMG form using the Shell.Explorer.2 ActiveX browser.
 *   This enables viewing PDF documents directly within the application's interface.
 *
 * PARAMETERS:
 *   cControl   (CHAR)  - Name of the control. If "0", a unique name is auto-generated.
 *   cParent    (CHAR)  - Parent form name where the control will be placed.
 *   nRow       (NUM)   - Vertical position of the control.
 *   nCol       (NUM)   - Horizontal position of the control.
 *   nWidth     (NUM)   - Optional. Width of the control. Default is 400.
 *   nHeight    (NUM)   - Optional. Height of the control. Default is 300.
 *   cFile      (CHAR)  - Path to the PDF file to be displayed.
 *   lInvisible (LOGIC) - Optional. If .T., the control is initially invisible. Default is .F.
 *
 * RETURNS:
 *   NIL
 *
 * NOTES:
 *   - Shell.Explorer.2 must be available on the system.
 */
FUNCTION DefinePDFViewer( cControl, cParent, nRow, nCol, nWidth, nHeight, cFile, lInvisible )

   LOCAL mVar, hWndCtrl

   // Assign default values if parameters are missing
   hb_default( @nWidth, 400 )
   hb_default( @nHeight, 300 )
   hb_default( @lInvisible, .F. )

   // Auto-generate a control name if requested
   IF ISCHAR( cControl ) .AND. cControl == "0"
      cControl := HMG_GetUniqueName()
   ENDIF

   // Avoid redefining an existing control
   IF _IsControlDefined( cControl, cParent )
      MsgStop( "Control already defined: " + cControl )
      RETURN NIL
   ENDIF

   mVar := "_" + cParent + "_" + cControl
   PUBLIC &mVar. := Len( _HMG_aControlNames ) + 1

   // Register control in HMG's internal arrays
   AAdd( _HMG_aControlType, "PDFVIEWER" )
   AAdd( _HMG_aControlNames, cControl )
   AAdd( _HMG_aControlHandles, NIL )  // Will be set after creation
   AAdd( _HMG_aControlParentHandles, GetFormHandle( cParent ) )
   AAdd( _HMG_aControlIds, 0 )
   AAdd( _HMG_aControlProcedures, "" )
   AAdd( _HMG_aControlPageMap, {} )
   AAdd( _HMG_aControlValue, cFile )
   AAdd( _HMG_aControlInputMask, "" )
   AAdd( _HMG_aControllostFocusProcedure, "" )
   AAdd( _HMG_aControlGotFocusProcedure, "" )
   AAdd( _HMG_aControlChangeProcedure, "" )
   AAdd( _HMG_aControlDeleted, .F. )
   AAdd( _HMG_aControlBkColor, NIL )
   AAdd( _HMG_aControlFontColor, NIL )
   AAdd( _HMG_aControlDblClick, "" )
   AAdd( _HMG_aControlHeadClick, {} )
   AAdd( _HMG_aControlRow, nRow )
   AAdd( _HMG_aControlCol, nCol )
   AAdd( _HMG_aControlWidth, nWidth )
   AAdd( _HMG_aControlHeight, nHeight )
   AAdd( _HMG_aControlSpacing, 0 )
   AAdd( _HMG_aControlContainerRow, -1 )
   AAdd( _HMG_aControlContainerCol, -1 )
   AAdd( _HMG_aControlPicture, "" )
   AAdd( _HMG_aControlContainerHandle, 0 )
   AAdd( _HMG_aControlFontName, "" )
   AAdd( _HMG_aControlFontSize, 0 )
   AAdd( _HMG_aControlFontAttributes, { .F., .F., .F., .F. } )
   AAdd( _HMG_aControlToolTip, "" )
   AAdd( _HMG_aControlRangeMin, 0 )
   AAdd( _HMG_aControlRangeMax, 0 )
   AAdd( _HMG_aControlCaption, "" )
   AAdd( _HMG_aControlVisible, !lInvisible )
   AAdd( _HMG_aControlHelpId, 0 )
   AAdd( _HMG_aControlFontHandle, 0 )
   AAdd( _HMG_aControlBrushHandle, 0 )
   AAdd( _HMG_aControlEnabled, .T. )
   AAdd( _HMG_aControlMiscData1, "" )
   AAdd( _HMG_aControlMiscData2, "" )

   // Create the embedded browser control (ActiveX)
   @ nRow, nCol ACTIVEX (cControl) ;
      OF (cParent) ;
      WIDTH nWidth ;
      HEIGHT nHeight ;
      PROGID "Shell.Explorer.2"

   hWndCtrl := GetControlHandle( cControl, cParent )
   _HMG_aControlHandles[ GetControlIndex( cControl, cParent ) ] := hWndCtrl

   // Load the initial PDF if specified
   IF !Empty( cFile )
      IF File( cFile )
         ActXNavigate( cParent, cControl, cFile )
         SetFocus( hWndCtrl )
      ELSE
         MsgStop( "PDF file not found: " + cFile )
      ENDIF
   ENDIF

RETURN NIL

/*
 * FUNCTION: ActXNavigate()
 *
 * DESCRIPTION:
 *   Navigates the ActiveX control to a specified URL or file path (e.g., a local PDF).
 *
 * PARAMETERS:
 *   cForm    (CHAR) - Name of the form containing the control.
 *   cControl (CHAR) - Name of the ActiveX control.
 *   cURL     (CHAR) - Target URL or file path.
 *
 * RETURNS:
 *   NIL
 *
 * NOTES:
 *   - Calls Navigate on the ActiveX web browser control.
 *   - Uses DoEvents() to ensure UI responsiveness.
 */
FUNCTION ActXNavigate( cForm, cControl, cURL )
   LOCAL oBrowser := GetActiveXObject( cForm, cControl )

   IF oBrowser != NIL
      oBrowser:Navigate( cURL )
      DoEvents()
   ELSE
      MsgStop( "Failed to access ActiveX object: " + cControl )
   ENDIF

RETURN NIL

/*
 * FUNCTION: ReleasePDFViewer()
 *
 * DESCRIPTION:
 *   Properly disposes of a PDF Viewer control by releasing its resources and unregistering it from HMG.
 *
 * PARAMETERS:
 *   cForm    (CHAR) - Name of the parent form.
 *   cControl (CHAR) - Name of the PDF Viewer control to remove.
 *
 * RETURNS:
 *   NIL
 *
 * NOTES:
 *   - Calls the control's Release method.
 *   - Removes the control from internal structures to avoid memory/resource leaks.
 */
FUNCTION ReleasePDFViewer( cForm, cControl )
   LOCAL i := GetControlIndex( cControl, cForm )

   IF i > 0
      DoMethod( cForm, cControl, "Release" )
      _EraseControl( i, AScan( _HMG_aFormHandles, _HMG_aControlParentHandles[i] ) )
   ELSE
      MsgStop( "Control not found: " + cControl )
   ENDIF

RETURN NIL
