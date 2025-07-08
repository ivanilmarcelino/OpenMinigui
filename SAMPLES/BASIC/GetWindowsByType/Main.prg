/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by Edward 18/Dec/2019
 *
 */

#include <hmg.ch>

SET PROCEDURE TO TL_MSG

/*
 *  PROCEDURE Main()
 *
 *  Description: This is the main entry point of the application. It loads and activates the main window.
 */
PROCEDURE Main()

   LOAD WINDOW Main
   Main.Activate()

RETURN

/*
 *  PROCEDURE ChildForm()
 *
 *  Description: This procedure creates and activates a new child form. It uses a unique name for each child form
 *               and positions it slightly offset from the previous one.
 */
PROCEDURE ChildForm()

   LOCAL ChildForm := HMG_GetUniqueName( "ChildForm_" )
   LOCAL nForm := Val( SubStr( ChildForm, RAt( "_", ChildForm ) + 1 ) )

   LOAD WINDOW Child AS &ChildForm

   SetProperty( ChildForm, "Title", "Child Form #" + hb_ntos( nForm ) )
   SetProperty( ChildForm, "Col", 25 * nForm )
   SetProperty( ChildForm, "Row", 25 * nForm )

   ACTIVATE WINDOW (ChildForm)

RETURN

/*
 *  PROCEDURE StdForm()
 *
 *  Description: This procedure creates and activates a new standard form. It uses a unique name for each standard form
 *               and positions it slightly offset from the previous one, with an additional horizontal offset.
 */
PROCEDURE StdForm()

   LOCAL StdForm := HMG_GetUniqueName( "StandardForm_" )
   LOCAL nForm := Val( SubStr( StdForm, RAt( "_", StdForm ) + 1 ) )

   LOAD WINDOW Standard AS &StdForm

   SetProperty ( StdForm, "Title", "Standard Form #" + hb_ntos( nForm ) )
   SetProperty ( StdForm, "Col", ( 25 * nForm ) + 400 )
   SetProperty ( StdForm, "Row", 25 * nForm )

   ACTIVATE WINDOW (StdForm)

RETURN

/*
 *  FUNCTION GetWindowsByType( cType )
 *
 *  Description: This function retrieves a list of windows based on their type. It iterates through the internal HMG window arrays
 *               and filters the windows based on the provided type.
 *
 *  Parameters:
 *      cType (STRING):  A string representing the window type to filter by.
 *                       - Empty string: returns all window types.
 *                       - "A": Main window
 *                       - "S": Standard window
 *                       - "C": Child window
 *                       - "M": Modal window
 *                       - "P": Panel
 *                       - "X": Spitchild window
 *
 *  Return Value:
 *      aForms (ARRAY): An array of arrays, where each inner array contains information about a window that matches the specified type.
 *                      The inner array contains the following elements:
 *                      { WindowName, WindowType, WindowIsDeleted, WindowIsActive, WindowHandle, WindowParentHandle }
 *
 *  Note: This function relies on internal HMG variables (_HMG_aFormType, _HMG_aFormNames, etc.).
 */
FUNCTION GetWindowsByType( cType )

   LOCAL aForms := {}

   DEFAULT cType := ''

   IF Upper( Left( cType, 1 ) ) $ "ASCMPX" .OR. Empty( cType )
      AEval( _HMG_aFormType, {| cWndType, nPos | IF( Empty( cType ) .OR. cWndType == Upper( Left(cType, 1 ) ), ;
         AAdd( aForms, ;
         { _HMG_aFormNames[ nPos ], ;
         _HMG_aFormType[ nPos ], ;
         _HMG_aFormDeleted[ nPos ], ;
         _HMG_aFormActive[ nPos ], ;
         _HMG_aFormHandles[ nPos ], ;
         _HMG_aFormParentHandle[ nPos ] } ), ;
         Nil ) } )
   ELSE
      MsgStop( 'Function GetWindowsByType(): invalid window type "' + cType + '"' )
   ENDIF

RETURN aForms
