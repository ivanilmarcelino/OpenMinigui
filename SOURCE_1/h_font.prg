/*----------------------------------------------------------------------------
 MINIGUI - Harbour Win32 GUI library source code

 Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 http://harbourminigui.googlepages.com/

 This program is free software; you can redistribute it and/or modify it under
 the terms of the GNU General Public License as published by the Free Software
 Foundation; either version 2 of the License, or (at your option) any later
 version.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this software; see the file COPYING. If not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
   visit the web site http://www.gnu.org/).

   As a special exception, you have permission for additional uses of the text
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   Harbour-Minigui library code into it.

   Parts of this project are based upon:

   "Harbour GUI framework for Win32"
   Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
   Copyright 2001 Antonio Linares <alinares@fivetech.com>
   www - https://harbour.github.io/

   "Harbour Project"
   Copyright 1999-2026, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>
---------------------------------------------------------------------------*/

#include "minigui.ch"

// Standard Windows Message for setting a control's font
#define WM_SETFONT         0x0030
// Internal identifier for the main form context in font management
#define _FORMNAME_         "Main"
// Internal constant to identify font-type entries in the control arrays
#define CONTROL_TYPE_FONT  "FONT"


/*
 * PROCEDURE: _ResetControlSlot
 * Purpose:   Clears all data for a specific index in the global HMG control arrays.
 * Params:    i (Numeric) - The index (slot) in the global arrays to be reset.
 * Reasoning: When a font or control is released, its slot must be zeroed to prevent 
 *            the framework from attempting to access stale handles or invalid memory.
 * Side Effects: Modifies global HMG state arrays (e.g., _HMG_aControlNames, _HMG_aControlHandles).
 */
STATIC PROCEDURE _ResetControlSlot( i )

   _HMG_aControlDeleted[i]               := .T.
   _HMG_aControlType[i]                  := ""
   _HMG_aControlNames[i]                 := ""
   _HMG_aControlHandles[i]               := 0
   _HMG_aControlParentHandles[i]         := 0
   _HMG_aControlIds[i]                   := 0
   _HMG_aControlProcedures[i]            := ""
   _HMG_aControlPageMap[i]               := {}
   _HMG_aControlValue[i]                 := NIL
   _HMG_aControlInputMask[i]             := ""
   _HMG_aControllostFocusProcedure[i]    := ""
   _HMG_aControlGotFocusProcedure[i]     := ""
   _HMG_aControlChangeProcedure[i]       := ""
   _HMG_aControlBkColor[i]               := NIL
   _HMG_aControlFontColor[i]             := NIL
   _HMG_aControlDblClick[i]              := ""
   _HMG_aControlHeadClick[i]             := {}
   _HMG_aControlRow[i]                   := 0
   _HMG_aControlCol[i]                   := 0
   _HMG_aControlWidth[i]                 := 0
   _HMG_aControlHeight[i]                := 0
   _HMG_aControlSpacing[i]               := 0
   _HMG_aControlContainerRow[i]          := 0
   _HMG_aControlContainerCol[i]          := 0
   _HMG_aControlPicture[i]               := ""
   _HMG_aControlContainerHandle[i]       := 0
   _HMG_aControlFontName[i]              := ""
   _HMG_aControlFontSize[i]              := 0
   _HMG_aControlFontAttributes[i]        := {}
   _HMG_aControlToolTip[i]               := ""
   _HMG_aControlRangeMin[i]              := 0
   _HMG_aControlRangeMax[i]              := 0
   _HMG_aControlCaption[i]               := ""
   _HMG_aControlVisible[i]               := .F.
   _HMG_aControlHelpId[i]                := 0
   _HMG_aControlFontHandle[i]            := 0
   _HMG_aControlBrushHandle[i]           := 0
   _HMG_aControlEnabled[i]               := .F.
   _HMG_aControlMiscData1[i]             := 0
   _HMG_aControlMiscData2[i]             := ""

RETURN


/*
 * FUNCTION: _BuildFontAttr
 * Purpose:  Constructs a standardized array of font style attributes.
 * Params:   lBold, lItalic, lUnderline, lStrikeOut (Logical) - Style flags.
 *           nAngle (Numeric) - Rotation angle.
 *           nCharset (Numeric) - Windows character set identifier.
 * Returns:  An array containing the mapped font attributes.
 */
STATIC FUNCTION _BuildFontAttr( lBold, lItalic, lUnderline, ;
                                lStrikeOut, nAngle, nCharset )

RETURN { ;
   lBold, ;
   lItalic, ;
   lUnderline, ;
   lStrikeOut, ;
   nAngle, ;
   hb_defaultValue( nCharset, DEFAULT_CHARSET ) ;
}


/*
 * FUNCTION: _FontExists
 * Purpose:  Verifies if a specific font face name is installed on the system.
 * Params:   cFontName (String) - The name of the font to check.
 * Returns:  .T. if the font is found, .F. otherwise.
 * Reasoning: Prevents GDI errors by allowing the framework to fall back to 
 *            safe defaults (like Arial) if a requested font is missing.
 */
STATIC FUNCTION _FontExists( cFontName )

   LOCAL aFontList := {}
   LOCAL aSymbolFonts := {}

   // Retrieve standard and symbol font lists from the OS
   GetFontList( NIL, NIL, NIL, NIL, NIL, NIL, @aFontList )
   GetFontList( NIL, NIL, SYMBOL_CHARSET, NIL, NIL, NIL, @aSymbolFonts )

   // Merge symbol fonts into the main list for comprehensive checking
   AEval( aSymbolFonts, {| cFont | AAdd( aFontList, cFont ) } )

RETURN ;
   ! Empty( ;
      AScan( aFontList, ;
         {| cName | Upper( cName ) == Upper( cFontName ) } ) )


/*
 * PROCEDURE: _DefineFont
 * Purpose:   Registers and creates a new font object within the HMG framework.
 * Params:    FontName (String) - Internal HMG name for the font.
 *            fName (String) - Font face name (e.g., "Courier New").
 *            fSize (Numeric) - Font size in points.
 *            lBold, lItalic, lUnderline, lStrikeOut (Logical) - Style flags.
 *            nAngle (Numeric) - Text rotation angle.
 *            lDefault (Logical) - If .T., sets this as the framework's default font.
 *            nCharset (Numeric) - Character set ID.
 * Reasoning: This is the core implementation of the DEFINE FONT command. It handles
 *            resource allocation, global registration, and default management.
 */
PROCEDURE _DefineFont( FontName, fName, fSize, ;
                       lBold, lItalic, lUnderline, ;
                       lStrikeOut, nAngle, lDefault, nCharset )

   LOCAL hFont
   LOCAL cMemVar
   LOCAL k

   // If a font with this name already exists, release it to avoid resource leaks
   IF _IsControlDefined( FontName, _FORMNAME_ )
      _ReleaseFont( FontName )
   ENDIF

   // Apply framework-level defaults if parameters are omitted
   hb_default( @fName, _HMG_DefaultFontName )
   hb_default( @fSize, _HMG_DefaultFontSize )
   hb_default( @lBold, .F. )
   hb_default( @lItalic, .F. )
   hb_default( @lUnderline, .F. )
   hb_default( @lStrikeOut, .F. )
   hb_default( @nAngle, 0 )

   // Fallback to Arial if the requested font doesn't exist and no specific charset is forced
   IF nCharset == NIL
      IF ! _FontExists( fName )
         fName := "Arial"
      ENDIF
   ENDIF

   // Update global framework defaults if requested
   IF hb_defaultValue( lDefault, .F. )
      _HMG_DefaultFontName := fName
      _HMG_DefaultFontSize := fSize
   ENDIF

   // Construct the internal variable name used for name-based access
   cMemVar := "_" + _FORMNAME_ + "_" + FontName
   k := _GetControlFree()

   // Register the font in the HMG name management system
#ifdef _NAMES_LIST_
   _SetNameList( cMemVar, k )
#else
   PUBLIC &cMemVar. := k
#endif

   // Create the actual Windows GDI Font object. 
   // Note: Windows API expects angle in tenths of a degree.
   hFont := InitFont( ;
      fName, ;
      fSize, ;
      lBold, ;
      lItalic, ;
      lUnderline, ;
      lStrikeOut, ;
      nAngle * 10, ;
      nCharset )

   // Populate the global control arrays with font metadata
   _HMG_aControlDeleted[k]               := .F.
   _HMG_aControlType[k]                  := CONTROL_TYPE_FONT
   _HMG_aControlNames[k]                 := FontName
   _HMG_aControlHandles[k]               := hFont
   _HMG_aControlParentHandles[k]         := 0
   _HMG_aControlIds[k]                   := _GetId()
   _HMG_aControlPageMap[k]               := {}

   // Calculate and store base text metrics for layout engine calculations
   _HMG_aControlWidth[k]                 := GetTextWidth( NIL, "B", hFont )
   _HMG_aControlHeight[k]                := GetTextHeight( NIL, "B", hFont )

   // Track container nesting levels for proper coordinate calculation
   _HMG_aControlContainerRow[k]          := ;
      iif( _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameRow[_HMG_FrameLevel], -1 )

   _HMG_aControlContainerCol[k]          := ;
      iif( _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameCol[_HMG_FrameLevel], -1 )

   _HMG_aControlFontName[k]              := fName
   _HMG_aControlFontSize[k]              := fSize

   _HMG_aControlFontAttributes[k]        := ;
      _BuildFontAttr( ;
         lBold, ;
         lItalic, ;
         lUnderline, ;
         lStrikeOut, ;
         nAngle, ;
         nCharset )

   _HMG_aControlVisible[k]               := .T.
   _HMG_aControlEnabled[k]               := .T.
   _HMG_aControlFontHandle[k]            := hFont

   // If Object-Oriented mode is enabled, trigger the initialization callback
   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, k, cMemVar )
   ENDIF

RETURN


/*
 * PROCEDURE: _ReleaseFont
 * Purpose:   Public interface to destroy a font by its HMG name.
 * Params:    FontName (String) - The name used during DEFINE FONT.
 */
PROCEDURE _ReleaseFont( FontName )

   LOCAL i := AScan( _HMG_aControlNames, FontName )

   // Ensure the index exists and is actually a font before erasing
   IF i > 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_FONT
      _EraseFontDef( i )
   ENDIF

RETURN


/*
 * PROCEDURE: _EraseFontDef
 * Purpose:   Low-level cleanup of font resources.
 * Params:    i (Numeric) - Index in the global control arrays.
 * Reasoning: This handles the actual destruction of the GDI object and 
 *            cleans up the Harbour memory variables to prevent leaks.
 */
PROCEDURE _EraseFontDef( i )

   LOCAL cMemVar

   // Delete the Windows GDI font handle
   DeleteObject( _HMG_aControlFontHandle[i] )

   // Trigger OOP destruction event if applicable
   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlDestroy, i )
   ENDIF

   cMemVar := "_" + _FORMNAME_ + "_" + _HMG_aControlNames[i]

   // Remove the font from the name list or release the public variable
#ifdef _NAMES_LIST_
   _DelNameList( cMemVar )
#else
   IF __mvExist( cMemVar )
#ifndef _PUBLIC_RELEASE_
      __mvPut( cMemVar, 0 )
#else
      __mvXRelease( cMemVar )
#endif
   ENDIF
#endif

   // Clear the array slot for future reuse
   _ResetControlSlot( i )

RETURN


/*
 * FUNCTION: GetFontHandle
 * Purpose:  Retrieves the Windows HFONT handle for a given HMG font name.
 * Params:   FontName (String) - The name of the font.
 * Returns:  Numeric handle (HFONT) or 0 if not found/invalid.
 * Reasoning: Validates that the handle is still a valid GDI object before returning.
 */
FUNCTION GetFontHandle( FontName )

   LOCAL i := AScan( _HMG_aControlNames, FontName )

   IF i > 0
      // Verify the handle is still valid via GetFontParamByRef
      IF GetFontParamByRef( _HMG_aControlHandles[i] )
         RETURN _HMG_aControlHandles[i]
      ENDIF
      // If handle is invalid but entry exists, perform emergency cleanup
      IF _HMG_aControlType[i] == CONTROL_TYPE_FONT
         _EraseFontDef( i )
      ENDIF
   ENDIF

RETURN 0


/*
 * FUNCTION: GetFontParam
 * Purpose:  Retrieves all properties of a font based on its Windows handle.
 * Params:   FontHandle (Numeric) - The HFONT handle.
 * Returns:  Array containing {Name, Size, Bold, Italic, Underline, Strikeout, Angle, Width, Height, HMGName}.
 */
FUNCTION GetFontParam( FontHandle )

   LOCAL aFontAttr
   LOCAL i := AScan( _HMG_aControlHandles, FontHandle )

   // Default return values if handle is not found
   aFontAttr := { ;
      _HMG_DefaultFontName, ;
      _HMG_DefaultFontSize, ;
      .F., .F., .F., .F., ;
      0, 0, 0, "" }

   // If handle is found in our registry, extract the stored attributes
   IF i > 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_FONT
      aFontAttr := { ;
         _HMG_aControlFontName[i], ;
         _HMG_aControlFontSize[i], ;
         _HMG_aControlFontAttributes[i, FONT_ATTR_BOLD], ;
         _HMG_aControlFontAttributes[i, FONT_ATTR_ITALIC], ;
         _HMG_aControlFontAttributes[i, FONT_ATTR_UNDERLINE], ;
         _HMG_aControlFontAttributes[i, FONT_ATTR_STRIKEOUT], ;
         iif( Len( _HMG_aControlFontAttributes[i] ) > 4, ;
            _HMG_aControlFontAttributes[i, FONT_ATTR_ANGLE], 0 ), ;
         _HMG_aControlWidth[i], ;
         _HMG_aControlHeight[i], ;
         _HMG_aControlNames[i] }
   ENDIF

RETURN aFontAttr


/*
 * FUNCTION: _GetFontAttr
 * Purpose:  Internal helper to get a specific font attribute for a control.
 * Params:   ControlName (String), ParentForm (String), nType (Numeric - Attribute ID).
 * Returns:  The requested attribute value or NIL.
 */
FUNCTION _GetFontAttr( ControlName, ParentForm, nType )

   LOCAL i

   IF ( i := GetControlIndex( ControlName, ParentForm ) ) > 0
      DO CASE
      CASE nType == FONT_ATTR_NAME
         RETURN _HMG_aControlFontName[i]
      CASE nType == FONT_ATTR_SIZE
         RETURN _HMG_aControlFontSize[i]
      CASE nType >= FONT_ATTR_BOLD .AND. ;
           nType <= FONT_ATTR_ANGLE
         RETURN _HMG_aControlFontAttributes[i][nType]
      ENDCASE
   ENDIF

RETURN NIL


/*
 * FUNCTION: _SetFontAttr
 * Purpose:  Dynamically updates a font attribute for an existing control at runtime.
 * Params:   ControlName (String), ParentForm (String), Value (Mixed), nType (Numeric).
 * Reasoning: This function handles the complexity of re-creating GDI objects and 
 *            notifying Windows controls of the change via WM_SETFONT.
 * Side Effects: Re-creates GDI font objects; may trigger control resizing (e.g., MonthCal).
 */
FUNCTION _SetFontAttr( ControlName, ParentForm, Value, nType )

   LOCAL i
   LOCAL hWnd
   LOCAL cFontName
   LOCAL nFontSize
   LOCAL lBold
   LOCAL lItalic
   LOCAL lUnderline
   LOCAL lStrikeOut
   LOCAL nAngle
   LOCAL cType

   // Validate attribute type range
   IF nType < FONT_ATTR_BOLD .OR. ;
      nType > FONT_ATTR_NAME
      RETURN .F.
   ENDIF

   i := GetControlIndex( ControlName, ParentForm )

   IF i == 0
      RETURN .F.
   ENDIF

   // Delete the old font object before creating the new one to prevent GDI leaks
   DeleteObject( _HMG_aControlFontHandle[i] )

   // Update the internal state with the new value
   DO CASE
   CASE nType == FONT_ATTR_NAME
      _HMG_aControlFontName[i] := Value
   CASE nType == FONT_ATTR_SIZE
      _HMG_aControlFontSize[i] := Value
   OTHERWISE
      _HMG_aControlFontAttributes[i][nType] := Value
   ENDCASE

   // Prepare variables for the new font creation
   hWnd        := _HMG_aControlHandles[i]
   cFontName   := _HMG_aControlFontName[i]
   nFontSize   := _HMG_aControlFontSize[i]

   lBold       := _HMG_aControlFontAttributes[i][FONT_ATTR_BOLD]
   lItalic     := _HMG_aControlFontAttributes[i][FONT_ATTR_ITALIC]
   lUnderline  := _HMG_aControlFontAttributes[i][FONT_ATTR_UNDERLINE]
   lStrikeOut  := _HMG_aControlFontAttributes[i][FONT_ATTR_STRIKEOUT]

   nAngle      := ;
      iif( Len( _HMG_aControlFontAttributes[i] ) > 4, ;
         _HMG_aControlFontAttributes[i][FONT_ATTR_ANGLE], 0 )

   cType := _HMG_aControlType[i]

   // Handle control-specific font application logic
   DO CASE
   CASE cType == "SPINNER"
      // Spinner controls are composite; apply font to the primary handle
      _HMG_aControlFontHandle[i] := ;
         _SetFont( ;
            hWnd[1], ;
            cFontName, ;
            nFontSize, ;
            lBold, ;
            lItalic, ;
            lUnderline, ;
            lStrikeOut, ;
            nAngle )

   CASE cType == "RADIOGROUP"
      // RadioGroups contain multiple buttons; apply font to all sub-handles
      _HMG_aControlFontHandle[i] := ;
         _SetFont( ;
            hWnd[1], ;
            cFontName, ;
            nFontSize, ;
            lBold, ;
            lItalic, ;
            lUnderline, ;
            lStrikeOut, ;
            nAngle )

      AEval( ;
         hWnd, ;
         {| x | ;
            SendMessage( ;
               x, ;
               WM_SETFONT, ;
               _HMG_aControlFontHandle[i], ;
               1 ) }, ;
         2 )

   OTHERWISE
      // Standard control font application
      IF IsWindowHandle( hWnd )
         _HMG_aControlFontHandle[i] := ;
            _SetFont( ;
               hWnd, ;
               cFontName, ;
               nFontSize, ;
               lBold, ;
               lItalic, ;
               lUnderline, ;
               lStrikeOut, ;
               nAngle )

         // MonthCalendar controls change size based on font; must recalculate layout
         IF cType == "MONTHCAL"
            SetPosMonthCal( ;
               hWnd, ;
               _HMG_aControlCol[i], ;
               _HMG_aControlRow[i] )

            _HMG_aControlWidth[i]  := GetWindowWidth( hWnd )
            _HMG_aControlHeight[i] := GetWindowHeight( hWnd )
         ENDIF
      ENDIF
   ENDCASE

   // If the control is a Label with an InputMask, refresh its value to apply formatting
   IF "LABEL" $ _HMG_aControlType[i] .AND. ;
      ISLOGICAL( _HMG_aControlInputMask[i] )

      IF _HMG_aControlInputMask[i]
         _SetValue( ;
            ControlName, ;
            ParentForm, ;
            _GetValue( , , i ) )
      ENDIF
   ENDIF

RETURN .T.


/*
 * FUNCTION: GetFontParamByRef
 * Purpose:  Retrieves font details and populates variables passed by reference.
 * Params:   FontHandle (Numeric) - The HFONT to inspect.
 *           FontName, FontSize, bold, italic, underline, strikeout, angle (By Reference).
 * Returns:  .T. if the handle is a valid GDI font object, .F. otherwise.
 * Reasoning: Uses Harbour/xHarbour specific "ByRef" detection to safely populate 
 *            caller variables without requiring an array return.
 */
FUNCTION GetFontParamByRef( FontHandle, ;
                            FontName, FontSize, ;
                            bold, italic, ;
                            underline, strikeout, angle )

   LOCAL lValid
   LOCAL i := ;
      iif( HB_ISNUMERIC( FontHandle ), ;
         AScan( _HMG_aControlHandles, FontHandle ), 0 )

   // Verify the handle exists in HMG and is recognized by Windows as a font object
   lValid := ;
      ( i > 0 .AND. ;
        GetObjectType( _HMG_aControlHandles[i] ) == OBJ_FONT )

#ifdef __XHARBOUR__
   // xHarbour specific reference check
   IF HB_IsByRef( @FontName )
      FontName := iif( lValid, ;
         _HMG_aControlFontName[i], ;
         _HMG_DefaultFontName )
   ENDIF

   IF HB_IsByRef( @FontSize )
      FontSize := iif( lValid, ;
         _HMG_aControlFontSize[i], ;
         _HMG_DefaultFontSize )
   ENDIF
#else
   // Harbour specific reference check
   IF hb_PIsByRef( 2 )
      FontName := iif( lValid, ;
         _HMG_aControlFontName[i], ;
         _HMG_DefaultFontName )
   ENDIF

   IF hb_PIsByRef( 3 )
      FontSize := iif( lValid, ;
         _HMG_aControlFontSize[i], ;
         _HMG_DefaultFontSize )
   ENDIF
#endif

   // Populate remaining style flags
   bold      := iif( lValid, _HMG_aControlFontAttributes[i, FONT_ATTR_BOLD], .F. )
   italic    := iif( lValid, _HMG_aControlFontAttributes[i, FONT_ATTR_ITALIC], .F. )
   underline := iif( lValid, _HMG_aControlFontAttributes[i, FONT_ATTR_UNDERLINE], .F. )
   strikeout := iif( lValid, _HMG_aControlFontAttributes[i, FONT_ATTR_STRIKEOUT], .F. )

   angle := iif( lValid .AND. ;
      Len( _HMG_aControlFontAttributes[i] ) > 4, ;
      _HMG_aControlFontAttributes[i, FONT_ATTR_ANGLE], ;
      0 )

RETURN lValid


/*
 * FUNCTION: GetFontList
 * Purpose:  Enumerates all available system fonts based on criteria.
 * Params:   hDC (Handle) - Device context (optional).
 *           cFontFamilyName (String) - Filter by family.
 *           nCharSet (Numeric) - Filter by charset.
 *           nPitch, nFontType (Numeric) - Filter by technical specs.
 *           lSortCaseSensitive (Logical) - Sorting preference.
 *           aFontName (Array By Ref) - Receives the list of font names.
 * Returns:  The result of the Windows API enumeration.
 */
FUNCTION GetFontList( hDC, cFontFamilyName, nCharSet, ;
                      nPitch, nFontType, ;
                      lSortCaseSensitive, aFontName )

   LOCAL bSort

   // Define sorting logic based on case sensitivity requirement
   IF hb_defaultValue( lSortCaseSensitive, .F. )
      bSort := { |x, y| x[1] < y[1] }
   ELSE
      bSort := { |x, y| ;
         Upper( x[1] ) < Upper( y[1] ) }
   ENDIF

RETURN ;
   EnumFontsEx( ;
      hDC, ;
      cFontFamilyName, ;
      nCharSet, ;
      nPitch, ;
      nFontType, ;
      bSort, ;
      @aFontName )
