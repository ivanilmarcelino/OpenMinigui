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
/*
 * Author: P.Chornyj <myorg63@mail.ru>
 * revised for build 16.10
*/

#include "minigui.ch"

#ifdef __XHARBOUR__
#xcommand END SWITCH => END
#xcommand OTHERWISE  => DEFAULT
#endif

/*-----------------------------------------------------------------------------*
FUNCTION _SetWindowBKBrush( cWindow, lNoDelete, cBrushStyle, nHatch, aColor, xImage )
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets the background brush for a specified window, allowing for different brush styles (solid, hatched, pattern).
*
*  Parameters:
*     cWindow     : Character string. The name of the window to modify. This name must correspond to a window
*                   previously created and registered within the HMG environment (e.g., a form name).
*     lNoDelete   : Logical.  Optional.  If .T., the function does *not* delete the existing background brush of the window.
*                   Instead, it returns the handle to the old brush. This allows the caller to manage the old brush.
*                   If .F. (default), the function deletes the existing brush.
*     cBrushStyle : Character string. Optional. Specifies the style of the new brush to create.  Valid values are:
*                   "SOLID" (default): Creates a solid color brush.
*                   "HATCH": Creates a hatched brush.  Requires the nHatch parameter to specify the hatch style.
*                   "PATTERN": Creates a pattern brush using the image specified by the xImage parameter.
*     nHatch      : Numeric. Optional.  Specifies the hatch style for hatched brushes.  This parameter is only relevant
*                   if cBrushStyle is set to "HATCH".  Use predefined HMG constants like HS_VERTICAL, HS_HORIZONTAL, etc.
*                   Default value is HS_VERTICAL.
*     aColor      : Array. Optional. An array containing three numeric values representing the RGB color components (Red, Green, Blue)
*                   for the brush.  Each value should be between 0 and 255.  This parameter is used for "SOLID" and "HATCH" brush styles.
*                   Default value is { 255, 0, 255 } (magenta).
*     xImage      : Character string. Optional. The name of the image to use for a pattern brush. This parameter is only relevant
*                   if cBrushStyle is set to "PATTERN". The image must be loaded and available within the HMG environment.
*                   Default value is "MINIGUI_EDIT_DELETE".
*
*  Return Value:
*     hBrush      : Numeric.  The handle to the newly created brush.  Returns 0 if the window is not found or if an error occurs during brush creation.
*                   If lNoDelete is .T., the function returns the handle to the *old* brush that was previously associated with the window.
*
*  Purpose:
*     This function provides a way to dynamically change the background appearance of a window in an HMG application.
*     It allows developers to set different background styles (solid color, hatched, or patterned) based on application logic or user preferences.
*     The function manages the creation and assignment of the new brush to the window, and optionally handles the deletion of the old brush.
*     This is useful for visually highlighting certain windows, providing visual cues to the user, or customizing the application's look and feel.
*
*     Example Usage:
*     To set the background of a form named "MyForm" to a solid blue color:
*        _SetWindowBKBrush( "MyForm", .F., "SOLID", , { 0, 0, 255 } )
*
*     To set the background of a form named "MyForm" to a hatched brush with a vertical pattern and a red color, and keep the old brush:
*        hOldBrush := _SetWindowBKBrush( "MyForm", .T., "HATCH", HS_VERTICAL, { 255, 0, 0 } )
*        // ... later, when the old brush is no longer needed:
*        DELETE BRUSH hOldBrush
*
*  Notes:
*     - The function relies on the global arrays _HMG_aFormHandles and _HMG_aFormBrushHandle to store window handles and brush handles, respectively.
*     - Ensure that the window specified by cWindow exists and is properly registered within the HMG environment before calling this function.
*     - When using a pattern brush, ensure that the image specified by xImage is loaded and available.
*     - If lNoDelete is set to .T., the caller is responsible for deleting the old brush to prevent memory leaks.
*     - Error handling is limited.  The function returns 0 if the window is not found, but it does not explicitly handle errors during brush creation.
*/
FUNCTION _SetWindowBKBrush( cWindow, lNoDelete, cBrushStyle, nHatch, aColor, xImage )
   LOCAL hWnd
   LOCAL hOldBrush
   LOCAL hBrush := 0
   LOCAL nIndex

   __defaultNIL( @lNoDelete, .F. )
   __defaultNIL( @cBrushStyle, "SOLID" )
   __defaultNIL( @nHatch, HS_VERTICAL )
   __defaultNIL( @aColor, { 255, 0, 255 } )
   __defaultNIL( @xImage, "MINIGUI_EDIT_DELETE" )

   nIndex := GetFormIndex ( cWindow )

   IF nIndex > 0
      hWnd := _HMG_aFormHandles[ nIndex ]

      SWITCH Left ( cBrushStyle, 1 )
      CASE "S"
         hBrush := CreateSolidBrush ( aColor[ 1 ], aColor[ 2 ], aColor[ 3 ] )
         EXIT
      CASE "H"
         hBrush := CreateHatchBrush ( nHatch, RGB( aColor[ 1 ], aColor[ 2 ], aColor[ 3 ] ) )
         EXIT
      CASE "P"
         hBrush := CreatePatternBrush ( xImage )
         EXIT
      OTHERWISE
         hBrush := GetWindowBrush ( hWnd )
      END SWITCH

      IF GetObjectType ( hBrush ) == OBJ_BRUSH
         hOldBrush := SetWindowBrush ( hWnd, hBrush )
         _HMG_aFormBrushHandle[ nIndex ] := hBrush

         IF lNoDelete
            RETURN hOldBrush
         ELSE
            DELETE BRUSH hOldBrush
         ENDIF
      ENDIF
   ENDIF

RETURN hBrush
