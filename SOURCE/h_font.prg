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
   Copyright 1999-2025, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>
---------------------------------------------------------------------------*/

#include 'minigui.ch'

#define WM_SETFONT	0x0030
#define _FORMNAME_	'Main'

/*
 * PROCEDURE _DefineFont( FontName, fName, fSize, bold, italic, underline, strikeout, nAngle, default, charset )
 *
 * Defines a font within the HMG environment, making it available for use by controls.
 *
 * Parameters:
 *   FontName  : The name to assign to the font definition. This name is used to reference the font later. (STRING)
 *   fName     : The name of the font face (e.g., "Arial", "Times New Roman"). (STRING, Optional, Default: _HMG_DefaultFontName)
 *   fSize     : The size of the font in points. (NUMERIC, Optional, Default: _HMG_DefaultFontSize)
 *   bold      : A logical value indicating whether the font should be bold. (.T. for bold, .F. for normal). (LOGICAL, Optional, Default: .F.)
 *   italic    : A logical value indicating whether the font should be italic. (.T. for italic, .F. for normal). (LOGICAL, Optional, Default: .F.)
 *   underline : A logical value indicating whether the font should be underlined. (.T. for underlined, .F. for normal). (LOGICAL, Optional, Default: .F.)
 *   strikeout : A logical value indicating whether the font should be struck out. (.T. for strikeout, .F. for normal). (LOGICAL, Optional, Default: .F.)
 *   nAngle    : The angle of the font in tenths of a degree. (NUMERIC, Optional, Default: 0)
 *   default   : A logical value indicating whether this font should be set as the default font for the application. (LOGICAL, Optional, Default: .F.)
 *   charset   : The character set to use for the font. If NIL, the function attempts to find a suitable font from the system's font list. (NUMERIC, Optional, Default: Determined by system fonts)
 *
 * Returns:
 *   None. This procedure defines a font and stores its information in internal HMG arrays.
 *
 * Purpose:
 *   This procedure allows developers to define fonts with specific attributes (name, size, style)
 *   and then use these fonts in various controls within their HMG applications.  It centralizes
 *   font definition, making it easier to manage and modify fonts throughout the application.
 *   For example, you might use this to define a specific font for all labels in your application
 *   to ensure a consistent look and feel.
 *
 * Notes:
 *   - The font is stored in internal HMG arrays, indexed by a unique key.
 *   - If a font with the same FontName already exists, it will be released (deleted) before the new font is defined.
 *   - The _FORMNAME_ macro is used to scope the font definition to the current form.
 *   - The _HMG_lOOPEnabled flag determines whether an "OnControlInit" event is triggered after the font is defined.
 */
PROCEDURE _DefineFont( FontName, fName, fSize, bold, italic, underline, strikeout, nAngle, default, charset )

   LOCAL FontHandle
   LOCAL aFontList := {}
   LOCAL aFontSymb := {}
   LOCAL mVar
   LOCAL k

   IF _IsControlDefined( FontName, _FORMNAME_ )
      _ReleaseFont( FontName )
   ENDIF

   hb_default( @fName, _HMG_DefaultFontName )
   hb_default( @fSize, _HMG_DefaultFontSize )
   hb_default( @bold, .F. )
   hb_default( @italic, .F. )
   hb_default( @underline, .F. )
   hb_default( @strikeout, .F. )
   hb_default( @nAngle, 0 )

   IF charset == NIL
      GetFontList( NIL, NIL, NIL, NIL, NIL, NIL, @aFontList )

      GetFontList( NIL, NIL, SYMBOL_CHARSET, NIL, NIL, NIL, @aFontSymb )
      AEval( aFontSymb, {| cFont | AAdd( aFontList, cFont ) } )

      IF Empty( AScan( aFontList, {| cName | Upper( cName ) == Upper( fName ) } ) )
         fName := "Arial"
      ENDIF
   ENDIF

   IF hb_defaultValue( default, .F. )
      _HMG_DefaultFontName := fName
      _HMG_DefaultFontSize := fSize
   ENDIF

   mVar := '_' + _FORMNAME_ + '_' + FontName

   k := _GetControlFree()

#ifdef _NAMES_LIST_
   _SetNameList( mVar , k )
#else
   Public &mVar. := k
#endif

   FontHandle := InitFont( fName, fSize, bold, italic, underline, strikeout, nAngle * 10, charset )

   _HMG_aControlType [k] := "FONT"
   _HMG_aControlNames [k] := FontName
   _HMG_aControlHandles [k] := FontHandle
   _HMG_aControlParenthandles [k] := 0
   _HMG_aControlIds [k] := _GetId()
   _HMG_aControlProcedures [k] := ""
   _HMG_aControlPageMap [k] := {}
   _HMG_aControlValue [k] := 0
   _HMG_aControlInputMask [k] := ""
   _HMG_aControllostFocusProcedure [k] := ""
   _HMG_aControlGotFocusProcedure [k] := ""
   _HMG_aControlChangeProcedure [k] := ""
   _HMG_aControlDeleted [k] := .F.
   _HMG_aControlBkColor [k] := Nil
   _HMG_aControlFontColor [k] := Nil
   _HMG_aControlDblClick [k] := ""
   _HMG_aControlHeadClick [k] := {}
   _HMG_aControlRow [k] := 0
   _HMG_aControlCol [k] := 0
   _HMG_aControlWidth [k] :=  GetTextWidth ( NIL, "B", FontHandle )
   _HMG_aControlHeight [k] := GetTextHeight( NIL, "B", FontHandle )
   _HMG_aControlSpacing [k] := 0
   _HMG_aControlContainerRow [k] :=  iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel ], -1 )
   _HMG_aControlContainerCol [k] :=  iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel ], -1 )
   _HMG_aControlPicture [k] := ""
   _HMG_aControlContainerHandle [k] := 0
   _HMG_aControlFontName [k] := fName
   _HMG_aControlFontSize [k] := fSize
   _HMG_aControlFontAttributes [k] := { bold, italic, underline, strikeout, nAngle, hb_defaultValue( charset, DEFAULT_CHARSET ) }
   _HMG_aControlToolTip [k] := ''
   _HMG_aControlRangeMin [k] := 0
   _HMG_aControlRangeMax [k] := 0
   _HMG_aControlCaption [k] := ''
   _HMG_aControlVisible [k] := .T.
   _HMG_aControlHelpId [k] := 0
   _HMG_aControlFontHandle [k] := FontHandle
   _HMG_aControlBrushHandle [k] := 0
   _HMG_aControlEnabled [k] := .T.
   _HMG_aControlMiscData1 [k] := 0
   _HMG_aControlMiscData2 [k] := ''

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, k, mVar )
   ENDIF

RETURN

/*
 * PROCEDURE _ReleaseFont( FontName )
 *
 * Releases (deletes) a previously defined font from the HMG environment.
 *
 * Parameters:
 *   FontName : The name of the font to release. (STRING)
 *
 * Returns:
 *   None. This procedure removes the font definition from internal HMG arrays.
 *
 * Purpose:
 *   This procedure is used to free up resources associated with a font that is no longer needed.
 *   It's important to release fonts when they are no longer in use to prevent memory leaks and
 *   improve application performance.  For example, you might release a font when a form is closed
 *   or when the application is exiting.
 *
 * Notes:
 *   - The font is identified by its name within the current form's scope (_FORMNAME_).
 *   - The procedure calls _EraseFontDef to perform the actual deletion of the font definition.
 *   - If the specified FontName does not exist or is not a font, the procedure does nothing.
 */
PROCEDURE _ReleaseFont( FontName )

   LOCAL i := AScan( _HMG_aControlNames, FontName )

   IF i > 0 .AND. _HMG_aControlType [i] == "FONT"
      _EraseFontDef( i )
   ENDIF

RETURN

/*
 * PROCEDURE _EraseFontDef( i )
 *
 * Erases a font definition from the internal HMG arrays, given its index.
 *
 * Parameters:
 *   i : The index of the font definition in the _HMG_aControlNames array. (NUMERIC)
 *
 * Returns:
 *   None. This procedure modifies the internal HMG arrays to remove the font definition.
 *
 * Purpose:
 *   This procedure is the core logic for removing a font definition from the HMG environment.
 *   It's called by _ReleaseFont after verifying that the specified font exists and is of the correct type.
 *   It performs the following actions:
 *     1. Deletes the font object using DeleteObject.
 *     2. Triggers an "OnControlDestroy" event if _HMG_lOOPEnabled is true.
 *     3. Releases the variable associated with the font name.
 *     4. Clears the font's data from the HMG control arrays.
 */
PROCEDURE _EraseFontDef( i )

   LOCAL mVar

   DeleteObject( _HMG_aControlFontHandle [i] )

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlDestroy, i )
   ENDIF

   mVar := '_' + _FORMNAME_ + '_' + _HMG_aControlNames [i]

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

   _HMG_aControlDeleted [i] := .T.
   _HMG_aControlType [i] := ""
   _HMG_aControlNames [i] := ""
   _HMG_aControlHandles [i] := 0
   _HMG_aControlParentHandles [i] := 0
   _HMG_aControlIds [i] := 0
   _HMG_aControlProcedures [i] := ""
   _HMG_aControlPageMap [i] := {}
   _HMG_aControlValue [i] := Nil
   _HMG_aControlInputMask [i] := ""
   _HMG_aControllostFocusProcedure [i] := ""
   _HMG_aControlGotFocusProcedure [i] := ""
   _HMG_aControlChangeProcedure [i] := ""
   _HMG_aControlBkColor [i] := Nil
   _HMG_aControlFontColor [i] := Nil
   _HMG_aControlDblClick [i] := ""
   _HMG_aControlHeadClick [i] := {}
   _HMG_aControlRow [i] := 0
   _HMG_aControlCol [i] := 0
   _HMG_aControlWidth [i] := 0
   _HMG_aControlHeight [i] := 0
   _HMG_aControlSpacing [i] := 0
   _HMG_aControlContainerRow [i] := 0
   _HMG_aControlContainerCol [i] := 0
   _HMG_aControlPicture [i] := ''
   _HMG_aControlContainerHandle[ i ] := 0
   _HMG_aControlFontName [i] := ''
   _HMG_aControlFontSize [i] := 0
   _HMG_aControlToolTip [i] := ''
   _HMG_aControlRangeMin [i] := 0
   _HMG_aControlRangeMax [i] := 0
   _HMG_aControlCaption [i] := ''
   _HMG_aControlVisible [i] := .F.
   _HMG_aControlHelpId [i] := 0
   _HMG_aControlFontHandle [i] := 0
   _HMG_aControlFontAttributes [i] := {}
   _HMG_aControlBrushHandle [i] := 0
   _HMG_aControlEnabled [i] := .F.
   _HMG_aControlMiscData1 [i] := 0
   _HMG_aControlMiscData2 [i] := ''

RETURN

/*
 * FUNCTION GetFontHandle( FontName )
 *
 * Retrieves the handle of a previously defined font.
 *
 * Parameters:
 *   FontName : The name of the font to retrieve the handle for. (STRING)
 *
 * Returns:
 *   The handle of the font if found and valid. (NUMERIC)
 *   Returns 0 if the font is not found or is invalid.
 *
 * Purpose:
 *   This function allows developers to obtain the Windows font handle associated with a font
 *   defined using _DefineFont. This handle can then be used in other Windows API functions
 *   that require a font handle.  For example, you might use this to set the font of a control
 *   directly using SendMessage.
 *
 * Notes:
 *   - The function searches for the font by its name in the _HMG_aControlNames array.
 *   - It calls GetFontParamByRef to validate the font handle. If the handle is invalid,
 *     the font definition is erased using _EraseFontDef.
 */
FUNCTION GetFontHandle( FontName )

   LOCAL i := AScan( _HMG_aControlNames, FontName )

   IF i > 0
      IF GetFontParamByRef( _HMG_aControlHandles [i] )
         RETURN _HMG_aControlHandles [i]
      ELSEIF _HMG_aControlType [i] == "FONT"
         _EraseFontDef( i )
      ENDIF
   ENDIF

RETURN 0

/*
 * FUNCTION GetFontParam( FontHandle )
 *
 * Retrieves the font attributes of a font, given its handle.
 *
 * Parameters:
 *   FontHandle : The handle of the font to retrieve the attributes for. (NUMERIC)
 *
 * Returns:
 *   An array containing the font attributes: { FontName, FontSize, Bold, Italic, Underline, Strikeout, Angle, Width, Height, FontName }. (ARRAY)
 *   If the font is not found or is invalid, returns an array containing default values.
 *
 * Purpose:
 *   This function allows developers to retrieve the attributes of a font, such as its name, size, and style.
 *   This can be useful for inspecting the properties of a font or for creating a new font based on an existing one.
 *
 * Notes:
 *   - The function searches for the font by its handle in the _HMG_aControlHandles array.
 *   - If the font is not found, it returns an array containing default font attributes.
 */
FUNCTION GetFontParam( FontHandle )

   LOCAL aFontAttr
   LOCAL i := AScan( _HMG_aControlHandles, FontHandle )

   aFontAttr := { _HMG_DefaultFontName, _HMG_DefaultFontSize, .F., .F., .F., .F., 0, 0, 0, "" }

   IF i > 0 .AND. _HMG_aControlType[ i ] == "FONT"
      aFontAttr := { ;
         _HMG_aControlFontName[ i ], ;
         _HMG_aControlFontSize[ i ], ;
         _HMG_aControlFontAttributes[ i, FONT_ATTR_BOLD ], ;
         _HMG_aControlFontAttributes[ i, FONT_ATTR_ITALIC ], ;
         _HMG_aControlFontAttributes[ i, FONT_ATTR_UNDERLINE ], ;
         _HMG_aControlFontAttributes[ i, FONT_ATTR_STRIKEOUT ], ;
         iif( Len( _HMG_aControlFontAttributes[ i ] ) == 5, _HMG_aControlFontAttributes[ i, FONT_ATTR_ANGLE ], 0 ), ;
         _HMG_aControlWidth[ i ], _HMG_aControlHeight[ i ], _HMG_aControlNames[ i ] }
   ENDIF

RETURN aFontAttr

/*
 * FUNCTION _GetFontAttr( ControlName, ParentForm, nType )
 *
 * Retrieves a specific font attribute of a control.
 *
 * Parameters:
 *   ControlName : The name of the control to retrieve the font attribute from. (STRING)
 *   ParentForm  : The name of the parent form of the control. (STRING)
 *   nType       : A numeric code indicating the font attribute to retrieve. (NUMERIC)
 *                 Possible values:
 *                   - FONT_ATTR_NAME: Font name.
 *                   - FONT_ATTR_SIZE: Font size.
 *                   - FONT_ATTR_BOLD: Bold attribute.
 *                   - FONT_ATTR_ITALIC: Italic attribute.
 *                   - FONT_ATTR_UNDERLINE: Underline attribute.
 *                   - FONT_ATTR_STRIKEOUT: Strikeout attribute.
 *                   - FONT_ATTR_ANGLE: Angle attribute.
 *
 * Returns:
 *   The value of the requested font attribute. (STRING, NUMERIC, or LOGICAL, depending on the attribute)
 *   Returns NIL if the control is not found.
 *
 * Purpose:
 *   This function provides a convenient way to access individual font attributes of a control.
 *   It's used internally by HMG to get font information for various purposes, such as drawing text
 *   or setting the font of a control.
 *
 * Notes:
 *   - The function uses GetControlIndex to find the index of the control in the HMG control arrays.
 *   - The nType parameter determines which font attribute is retrieved.
 */
FUNCTION _GetFontAttr( ControlName, ParentForm, nType )

   LOCAL i

   IF ( i := GetControlIndex( ControlName, ParentForm ) ) > 0

      DO CASE
      CASE nType == FONT_ATTR_NAME
         RETURN _HMG_aControlFontName[ i ]

      CASE nType == FONT_ATTR_SIZE
         RETURN _HMG_aControlFontSize[ i ]

      CASE nType >= FONT_ATTR_BOLD .AND. nType <= FONT_ATTR_ANGLE
         RETURN _HMG_aControlFontAttributes[ i ][ nType ]

      ENDCASE

   ENDIF

RETURN NIL

/*
 * FUNCTION _SetFontAttr( ControlName, ParentForm, Value, nType )
 *
 * Sets a specific font attribute of a control.
 *
 * Parameters:
 *   ControlName : The name of the control to set the font attribute for. (STRING)
 *   ParentForm  : The name of the parent form of the control. (STRING)
 *   Value       : The new value for the font attribute. (STRING, NUMERIC, or LOGICAL, depending on the attribute)
 *   nType       : A numeric code indicating the font attribute to set. (NUMERIC)
 *                 Possible values:
 *                   - FONT_ATTR_NAME: Font name.
 *                   - FONT_ATTR_SIZE: Font size.
 *                   - FONT_ATTR_BOLD: Bold attribute.
 *                   - FONT_ATTR_ITALIC: Italic attribute.
 *                   - FONT_ATTR_UNDERLINE: Underline attribute.
 *                   - FONT_ATTR_STRIKEOUT: Strikeout attribute.
 *                   - FONT_ATTR_ANGLE: Angle attribute.
 *
 * Returns:
 *   .T. if the font attribute was successfully set. (LOGICAL)
 *   .F. if the control is not found or the nType is invalid.
 *
 * Purpose:
 *   This function allows developers to modify individual font attributes of a control at runtime.
 *   It's used to dynamically change the appearance of controls based on user input or other application logic.
 *   For example, you might use this to change the font size of a label when the user clicks a button.
 *
 * Notes:
 *   - The function uses GetControlIndex to find the index of the control in the HMG control arrays.
 *   - It recreates the font handle after changing the font attributes.
 *   - The function handles different control types (e.g., SPINNER, RADIOGROUP, MONTHCAL) and updates their font accordingly.
 *   - If the control is a LABEL and its InputMask is set to .T., the function updates the label's value to reflect the new font.
 */
FUNCTION _SetFontAttr( ControlName, ParentForm, Value, nType )

   LOCAL i, h, n, s, ab, ai, au, as, aa, t

   IF nType < FONT_ATTR_BOLD .OR. nType > FONT_ATTR_NAME
      RETURN .F.
   ENDIF

   i := GetControlIndex ( ControlName, ParentForm )

   IF i == 0
      RETURN .F.
   ENDIF

   DeleteObject ( _HMG_aControlFontHandle[ i ] )

   DO CASE
   CASE nType == FONT_ATTR_NAME
      _HMG_aControlFontName[ i ] := Value

   CASE nType == FONT_ATTR_SIZE
      _HMG_aControlFontSize[ i ] := Value

   OTHERWISE
      _HMG_aControlFontAttributes[ i ][ nType ] := Value

   ENDCASE

   h  := _HMG_aControlHandles[ i ]
   n  := _HMG_aControlFontName[ i ]
   s  := _HMG_aControlFontSize[ i ]
   ab := _HMG_aControlFontAttributes[ i ][ FONT_ATTR_BOLD ]
   ai := _HMG_aControlFontAttributes[ i ][ FONT_ATTR_ITALIC ]
   au := _HMG_aControlFontAttributes[ i ][ FONT_ATTR_UNDERLINE ]
   as := _HMG_aControlFontAttributes[ i ][ FONT_ATTR_STRIKEOUT ]
   aa := iif( Len( _HMG_aControlFontAttributes[ i ] ) == 5, _HMG_aControlFontAttributes[ i ][ FONT_ATTR_ANGLE ], 0 )

   t := _HMG_aControlType[ i ]

   DO CASE
   CASE t == "SPINNER"
      _HMG_aControlFontHandle[ i ] := _SetFont( h[ 1 ], n, s, ab, ai, au, as, aa )

   CASE t == "RADIOGROUP"
      _HMG_aControlFontHandle[ i ] := _SetFont( h[ 1 ], n, s, ab, ai, au, as, aa )
      AEval( h, {|x| SendMessage ( x, WM_SETFONT, _HMG_aControlFontHandle[ i ], 1 ) }, 2 )

   OTHERWISE
      IF IsWindowHandle( h )
         _HMG_aControlFontHandle[ i ] := _SetFont( h, n, s, ab, ai, au, as, aa )
         IF t == "MONTHCAL"
            SetPosMonthCal ( h, _HMG_aControlCol[ i ], _HMG_aControlRow[ i ] )
            _HMG_aControlWidth[ i ] := GetWindowWidth ( h )
            _HMG_aControlHeight[ i ] := GetWindowHeight ( h )
         ENDIF
      ENDIF

   ENDCASE

   IF "LABEL" $ _HMG_aControlType[ i ] .AND. ISLOGICAL ( _HMG_aControlInputMask[ i ] )
      IF _HMG_aControlInputMask[ i ]
         _SetValue ( ControlName, ParentForm, _GetValue ( , , i ) )
      ENDIF
   ENDIF

RETURN .T.

/*
 * FUNCTION GetFontParamByRef( FontHandle, FontName, FontSize, bold, italic, underline, strikeout, angle )
 *
 * Retrieves font attributes by reference, given a font handle.
 *
 * Parameters:
 *   FontHandle : The handle of the font to retrieve the attributes for. (NUMERIC)
 *   FontName   : A variable to store the font name. (STRING, Passed by Reference)
 *   FontSize   : A variable to store the font size. (NUMERIC, Passed by Reference)
 *   bold       : A variable to store the bold attribute. (LOGICAL, Passed by Reference)
 *   italic     : A variable to store the italic attribute. (LOGICAL, Passed by Reference)
 *   underline  : A variable to store the underline attribute. (LOGICAL, Passed by Reference)
 *   strikeout  : A variable to store the strikeout attribute. (LOGICAL, Passed by Reference)
 *   angle      : A variable to store the angle attribute. (NUMERIC, Passed by Reference)
 *
 * Returns:
 *   .T. if the font is found and its attributes are successfully retrieved. (LOGICAL)
 *   .F. if the font is not found or is invalid.
 *
 * Purpose:
 *   This function allows developers to retrieve font attributes by reference, which means that the
 *   values of the variables passed as parameters are directly modified by the function. This is
 *   more efficient than returning an array of attributes, especially when only a few attributes are needed.
 *
 * Notes:
 *   - The function searches for the font by its handle in the _HMG_aControlHandles array.
 *   - The parameters FontName, FontSize, bold, italic, underline, strikeout, and angle must be passed by reference
 *     (using the @ operator in Harbour).
 *   - If the font is not found, the function assigns default values to the output variables.
 */
FUNCTION GetFontParamByRef( FontHandle, FontName, FontSize, bold, italic, underline, strikeout, angle )

   LOCAL lExpr
   LOCAL i := iif( HB_ISNUMERIC( FontHandle ), AScan( _HMG_aControlHandles, FontHandle ), 0 )

   lExpr := ( i > 0 .AND. GetObjectType( _HMG_aControlHandles[ i ] ) == OBJ_FONT )

#ifdef __XHARBOUR__
   IF HB_IsByRef( @FontName )
#else
   IF hb_PIsByRef( 2 )
#endif
      FontName := iif( lExpr, _HMG_aControlFontName[ i ], _HMG_DefaultFontName )
   ENDIF
#ifdef __XHARBOUR__
   IF HB_IsByRef( @FontSize )
#else
   IF hb_PIsByRef( 3 )
#endif
      FontSize := iif( lExpr, _HMG_aControlFontSize[ i ], _HMG_DefaultFontSize )
   ENDIF
#ifdef __XHARBOUR__
   IF HB_IsByRef( @bold )
#else
   IF hb_PIsByRef( 4 )
#endif
      bold := iif( lExpr, _HMG_aControlFontAttributes[ i, FONT_ATTR_BOLD ], .F. )
   ENDIF
#ifdef __XHARBOUR__
   IF HB_IsByRef( @italic )
#else
   IF hb_PIsByRef( 5 )
#endif
      italic := iif( lExpr, _HMG_aControlFontAttributes[ i, FONT_ATTR_ITALIC ], .F. )
   ENDIF
#ifdef __XHARBOUR__
   IF HB_IsByRef( @underline )
#else
   IF hb_PIsByRef( 6 )
#endif
      underline := iif( lExpr, _HMG_aControlFontAttributes[ i, FONT_ATTR_UNDERLINE ], .F. )
   ENDIF
#ifdef __XHARBOUR__
   IF HB_IsByRef( @strikeout )
#else
   IF hb_PIsByRef( 7 )
#endif
      strikeout := iif( lExpr, _HMG_aControlFontAttributes[ i, FONT_ATTR_STRIKEOUT ], .F. )
   ENDIF
#ifdef __XHARBOUR__
   IF HB_IsByRef( @angle )
#else
   IF hb_PIsByRef( 8 )
#endif
      angle := iif( lExpr, ;
        iif( Len( _HMG_aControlFontAttributes[ i ] ) > 4, _HMG_aControlFontAttributes[ i, FONT_ATTR_ANGLE ], 0 ), ;
        0 )
   ENDIF

RETURN lExpr

/*
   Copyright Dr. Claudio Soto (January 2014)
/*

/*
 * FUNCTION GetFontList( hDC, cFontFamilyName, nCharSet, nPitch, nFontType, lSortCaseSensitive, aFontName )
 *
 * Retrieves a list of available fonts on the system.
 *
 * Parameters:
 *   hDC                 : A handle to a device context. If NIL, the function uses the default device context. (NUMERIC, Optional)
 *   cFontFamilyName     : The name of a specific font family to filter the list. If NIL, all font families are included. (STRING, Optional)
 *   nCharSet            : The character set to filter the list. If NIL, all character sets are included. (NUMERIC, Optional)
 *   nPitch              : The pitch to filter the list. If NIL, all pitches are included. (NUMERIC, Optional)
 *   nFontType           : The font type to filter the list. If NIL, all font types are included. (NUMERIC, Optional)
 *   lSortCaseSensitive  : A logical value indicating whether the font list should be sorted case-sensitively. (LOGICAL, Optional, Default: .F.)
 *   aFontName           : An array to store the list of font names. (ARRAY, Passed by Reference)
 *
 * Returns:
 *   The function returns the result of the EnumFontsEx function, which is typically the number of fonts enumerated. (NUMERIC)
 *   The aFontName parameter is modified to contain an array of arrays, where each inner array contains font information:
 *   { { cFontName, nCharSet, nPitchAndFamily, nFontType } , ... }
 *
 * Purpose:
 *   This function allows developers to retrieve a list of available fonts on the system. This can be useful for
 *   allowing users to select a font from a list or for programmatically determining which fonts are available.
 *
 * Notes:
 *   - The function uses the EnumFontsEx Windows API function to retrieve the font list.
 *   - The lSortCaseSensitive parameter controls whether the font list is sorted case-sensitively.
 *   - The aFontName parameter must be passed by reference.
 */
FUNCTION GetFontList( hDC, cFontFamilyName, nCharSet, nPitch, nFontType, lSortCaseSensitive, aFontName )

   LOCAL SortCodeBlock

   IF hb_defaultValue ( lSortCaseSensitive , .F. )
      SortCodeBlock := { |x, y| x[1] < y[1] }
   ELSE
      SortCodeBlock := { |x, y| Upper( x[1] ) < Upper( y[1] ) }
   ENDIF

RETURN EnumFontsEx( hDC, cFontFamilyName, nCharSet, nPitch, nFontType, SortCodeBlock, @aFontName )
