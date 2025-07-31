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
// Define Internet Explorer version (5.01) to ensure compatibility with older Windows components
#define _WIN32_IE 0x0501

// Include necessary header files
#include <mgdefs.h>                 // MiniGUI-specific definitions
#include <windowsx.h>               // Windows macros for message handling
#include <commctrl.h>               // Common controls, like combo boxes and image lists

// Check for old versions of Borland Compiler and define ComboBox class name for compatibility
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_COMBOBOX  "ComboBox"     // Define ComboBox class name
#endif

// Declare external functions for image list and string conversion
HIMAGELIST  HMG_ImageListLoadFirst( const char *FileName, int cGrow, int Transparent, int *nWidth, int *nHeight );
void        HMG_ImageListAdd( HIMAGELIST himl, char *FileName, int Transparent );

#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );   // Convert ANSI string to wide string for Unicode support
LPSTR       WideToAnsi( LPWSTR );   // Convert wide string to ANSI for compatibility
#endif

// Retrieve instance handle for the application
HINSTANCE   GetInstance( void );
HINSTANCE   GetResources( void );

/*
 * FUNCTION INITCOMBOBOX()
 *
 * Creates a standard ComboBox control with specified styles and properties.
 *
 * Parameters:
 *   1: Parent window handle (HWND). The handle of the parent window to which the ComboBox will be attached.
 *   2: Menu handle (HMENU). The menu handle associated with the ComboBox.
 *   3: X position (Numeric). The horizontal position of the ComboBox in pixels, relative to the parent window.
 *   4: Y position (Numeric). The vertical position of the ComboBox in pixels, relative to the parent window.
 *   5: Width (Numeric). The width of the ComboBox in pixels.
 *   6: Uppercase (Logical). If .T., the ComboBox will force input to uppercase.
 *   7: Lowercase (Logical). If .T., the ComboBox will force input to lowercase.
 *   8: Height (Numeric). The height of the ComboBox in pixels.
 *   9: Visible (Logical). If .F., the ComboBox will be visible. If .T., it will be hidden.
 *  10: Tabstop (Logical). If .F., the ComboBox will be included in the tab order. If .T., it will be skipped.
 *  11: Sort (Logical). If .T., the ComboBox will automatically sort its items.
 *  12: Dropdown (Logical). If .T., the ComboBox will be a dropdown ComboBox. If .F., it will be a dropdown list.
 *  13: NoIntegralHeight (Logical). If .T., the ComboBox will not resize to avoid partial item display.
 *
 * Returns:
 *   HWND: The handle of the newly created ComboBox control.
 *
 * Purpose:
 *   This function provides a convenient way to create a ComboBox control with various styles
 *   and properties directly from Harbour code. It encapsulates the Windows API CreateWindow function
 *   with specific parameters for ComboBox creation. This simplifies the process of creating
 *   ComboBox controls and reduces code duplication.
 *
 * Notes:
 *   - The function uses the WC_COMBOBOX class name, which is defined based on the compiler version.
 *   - The function returns the raw HWND, which can be used for further interaction with the ComboBox control
 *     using Windows API functions.
 */
HB_FUNC( INITCOMBOBOX )
{
   DWORD Style = WS_CHILD | WS_VSCROLL | ( hb_parl( 12 ) ? CBS_DROPDOWN : CBS_DROPDOWNLIST );   // Set base styles
   if( !hb_parl( 9 ) )
   {
      Style |= WS_VISIBLE;             // Make ComboBox visible if param 9 is false
   }

   if( !hb_parl( 10 ) )
   {
      Style |= WS_TABSTOP;             // Allow tabbing to ComboBox if param 10 is false
   }

   if( hb_parl( 11 ) )
   {
      Style |= CBS_SORT;               // Enable sorting if param 11 is true
   }

   if( hb_parl( 13 ) )
   {
      Style |= CBS_NOINTEGRALHEIGHT;   // Disable resizing if param 13 is true
   }

   if( hb_parl( 6 ) )
   {
      Style |= CBS_UPPERCASE;          // Force uppercase if param 6 is true
   }

   if( hb_parl( 7 ) )
   {
      Style |= CBS_LOWERCASE;          // Force lowercase if param 7 is true
   }

   hmg_ret_raw_HWND
   (
      CreateWindow
         (
            WC_COMBOBOX,               // ComboBox class name
            TEXT( "" ),                // Default text
            Style,                     // Styles determined by parameters
            hb_parni( 3 ),             // X position
            hb_parni( 4 ),             // Y position
            hb_parni( 5 ),             // Width
            hb_parni( 8 ),             // Height
            hmg_par_raw_HWND( 1 ),     // Parent window handle
            hmg_par_raw_HMENU( 2 ),    // Menu handle
            GetInstance(),             // Application instance handle
            NULL                       // Additional parameters
         )
   );
}

/*
 * FUNCTION INITCOMBOBOXEX()
 *
 * Creates an extended ComboBox control (ComboBoxEx) with support for images and additional styles.
 *
 * Parameters:
 *   1: Parent window handle (HWND). The handle of the parent window to which the ComboBoxEx will be attached.
 *   2: Menu handle (HMENU). The menu handle associated with the ComboBoxEx.
 *   3: X position (Numeric). The horizontal position of the ComboBoxEx in pixels, relative to the parent window.
 *   4: Y position (Numeric). The vertical position of the ComboBoxEx in pixels, relative to the parent window.
 *   5: Width (Numeric). The width of the ComboBoxEx in pixels.
 *   6: Uppercase (Logical).  Not used in this function.  Included for consistency with INITCOMBOBOX.
 *   7: Transparent (Logical). If .T., the images in the ImageList will be transparent.
 *   8: Height (Numeric). The height of the ComboBoxEx in pixels.
 *   9: Visible (Logical). If .F., the ComboBoxEx will be visible. If .T., it will be hidden.
 *  10: Tabstop (Logical). If .F., the ComboBoxEx will be included in the tab order. If .T., it will be skipped.
 *  11: Sort (Logical). Not used in this function. Included for consistency with INITCOMBOBOX.
 *  12: Dropdown (Logical). If .T., the ComboBoxEx will be a dropdown ComboBox. If .F., it will be a dropdown list.
 *  13: NoIntegralHeight (Logical). If .T., the ComboBoxEx will not resize to avoid partial item display.
 *  14: Image Array (Array). An array of image file names to be used in the ComboBoxEx.
 *  15: ImageList Handle (HIMAGELIST). An existing ImageList handle to be used in the ComboBoxEx.  If this is provided, parameter 14 is ignored.
 *  16: Image Width (Numeric). The width of the images in the ImageList.
 *  17: Image Height (Numeric). The height of the images in the ImageList.
 *
 * Returns:
 *   Array: An array containing the handles of the ComboBoxEx and the ImageList.
 *          Index 1: HWND of the ComboBoxEx control.
 *          Index 2: HIMAGELIST of the ImageList associated with the ComboBoxEx.
 *
 * Purpose:
 *   This function creates an extended ComboBox control (ComboBoxEx) that supports displaying images
 *   along with the text in the dropdown list. It allows specifying an array of image file names or
 *   an existing ImageList handle to be used for the ComboBoxEx. This function is useful for creating
 *   ComboBox controls with visual cues, making them more user-friendly and informative.
 *
 * Notes:
 *   - The function uses the WC_COMBOBOXEX class name, which is a standard Windows class for extended ComboBox controls.
 *   - The function initializes the common controls library to ensure that the ComboBoxEx class is available.
 *   - If both an image array (parameter 14) and an ImageList handle (parameter 15) are provided, the ImageList handle takes precedence.
 *   - The function stores the handles of the ComboBoxEx and the ImageList in a local variable for later use.
 */
HB_FUNC( INITCOMBOBOXEX )
{
   HWND                 hCombo;
   PHB_ITEM             hArray;
   HIMAGELIST           himl = ( HIMAGELIST ) NULL;
   char                 *FileName;
   int                  nCount, s, nWidth, nHeight;
   DWORD                Style = WS_CHILD | WS_VSCROLL | ( hb_parl( 12 ) ? CBS_DROPDOWN : CBS_DROPDOWNLIST );

   INITCOMMONCONTROLSEX icex;       // Structure to initialize common controls
   icex.dwSize = sizeof( INITCOMMONCONTROLSEX );
   icex.dwICC = ICC_USEREX_CLASSES; // Enable ComboBoxEx class
   InitCommonControlsEx( &icex );   // Initialize controls
   if( !hb_parl( 9 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 10 ) )
   {
      Style |= WS_TABSTOP;
   }

   if( hb_parl( 13 ) )
   {
      Style |= CBS_NOINTEGRALHEIGHT;
   }

   hCombo = CreateWindowEx
      (
         0,
         WC_COMBOBOXEX,
         TEXT( "" ),
         Style,
         hb_parni( 3 ),
         hb_parni( 4 ),
         hb_parni( 5 ),
         hb_parni( 8 ),
         hmg_par_raw_HWND( 1 ),
         hmg_par_raw_HMENU( 2 ),
         GetInstance(),
         NULL
      );

   // Create ImageList if array of images is provided
   nCount = ( int ) hb_parinfa( 14, 0 );  // Get number of images
   if( nCount > 0 )
   {
      int   Transparent = hb_parl( 7 ) ? 0 : 1;
      hArray = hb_param( 14, HB_IT_ARRAY );
      nWidth = hb_parni( 16 );
      nHeight = hb_parni( 17 );

      // Loop to add each image file to ImageList
      for( s = 1; s <= nCount; s++ )
      {
         FileName = ( char * ) hb_arrayGetCPtr( hArray, s );
         if( himl == NULL )
         {
            himl = HMG_ImageListLoadFirst( FileName, nCount, Transparent, &nWidth, &nHeight );
         }
         else
         {
            HMG_ImageListAdd( himl, FileName, Transparent );
         }
      }
   }

   // If no ImageList is provided, use one from parameter 15
   if( himl == NULL && HB_PARNL( 15 ) > 0 )
   {
      himl = hmg_par_raw_HIMAGELIST( 15 );
   }

   // Set ImageList for ComboBoxEx or apply style without images
   if( himl != NULL )
   {
      SendMessage( hCombo, CBEM_SETIMAGELIST, 0, ( LPARAM ) himl );
   }
   else
   {
      SendMessage( hCombo, CBEM_SETEXTENDEDSTYLE, ( WPARAM ) 0, ( LPARAM ) CBES_EX_NOEDITIMAGE );
   }

   hb_reta( 2 );
   hmg_storvnl_HANDLE( hCombo, -1, 1 );
   hmg_storvnl_HANDLE( himl, -1, 2 );
}

/*
 * FUNCTION COMBOSETITEMHEIGHT()
 *
 * Sets the height of the items in a ComboBox control.
 *
 * Parameters:
 *   1: ComboBox handle (HWND). The handle of the ComboBox control.
 *   2: Height (Numeric). The desired height of the items in pixels.
 *
 * Returns:
 *   LRESULT: The return value of the ComboBox_SetItemHeight function.
 *
 * Purpose:
 *   This function allows you to customize the height of the items displayed in a ComboBox control.
 *   This can be useful for adjusting the appearance of the ComboBox to fit the content or to
 *   improve readability.
 *
 * Notes:
 *   - The function uses the ComboBox_SetItemHeight macro, which is a wrapper around the CB_SETITEMHEIGHT message.
 *   - The height is applied to all items in the ComboBox.
 */
HB_FUNC( COMBOSETITEMHEIGHT )
{
   hmg_ret_LRESULT( ComboBox_SetItemHeight( hmg_par_raw_HWND( 1 ), -1, hb_parni( 2 ) ) );
}

/*
 * FUNCTION COMBOSHOWDROPDOWN()
 *
 * Displays or hides the dropdown list of a ComboBox control.
 *
 * Parameters:
 *   1: ComboBox handle (HWND). The handle of the ComboBox control.
 *
 * Returns:
 *   Logical: .T. if the dropdown list is displayed, .F. otherwise.
 *
 * Purpose:
 *   This function allows you to programmatically control the visibility of the dropdown list of a ComboBox.
 *   This can be useful for implementing custom behavior or for synchronizing the dropdown list with other
 *   UI elements.
 *
 * Notes:
 *   - The function uses the ComboBox_ShowDropdown macro, which is a wrapper around the CB_SHOWDROPDOWN message.
 *   - The function always displays the dropdown list (TRUE is passed as the second parameter to ComboBox_ShowDropdown).
 */
HB_FUNC( COMBOSHOWDROPDOWN )
{
   hb_retl( ComboBox_ShowDropdown( hmg_par_raw_HWND( 1 ), TRUE ) );
}

/*
 * FUNCTION COMBOEDITSETSEL()
 *
 * Sets the selection range in the edit control of a ComboBox.
 *
 * Parameters:
 *   1: ComboBox handle (HWND). The handle of the ComboBox control.
 *   2: Start position (Numeric). The starting position of the selection.
 *   3: End position (Numeric). The ending position of the selection.
 *
 * Returns:
 *   LRESULT: The return value of the ComboBox_SetEditSel function.
 *
 * Purpose:
 *   This function allows you to programmatically select a portion of the text in the edit control of a ComboBox.
 *   This can be useful for highlighting specific parts of the text or for implementing custom editing behavior.
 *
 * Notes:
 *   - The function uses the ComboBox_SetEditSel macro, which is a wrapper around the CB_SETEDITSEL message.
 *   - The start and end positions are zero-based.
 */
HB_FUNC( COMBOEDITSETSEL )
{
   hmg_ret_LRESULT( ComboBox_SetEditSel( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * FUNCTION COMBOGETEDITSEL()
 *
 * Retrieves the selection range in the edit control of a ComboBox.
 *
 * Parameters:
 *   1: ComboBox handle (HWND). The handle of the ComboBox control.
 *
 * Returns:
 *   Array: An array containing the starting and ending positions of the selection.
 *          Index 1: Starting position.
 *          Index 2: Ending position.
 *
 * Purpose:
 *   This function allows you to determine the currently selected portion of the text in the edit control of a ComboBox.
 *   This can be useful for implementing custom editing behavior or for retrieving the selected text.
 *
 * Notes:
 *   - The function uses the ComboBox_GetEditSel macro, which is a wrapper around the CB_GETEDITSEL message.
 *   - The starting and ending positions are zero-based.
 */
HB_FUNC( COMBOGETEDITSEL )
{
   DWORD pos = ComboBox_GetEditSel( hmg_par_raw_HWND( 1 ) );
   hb_reta( 2 );
   HB_STORNI( LOWORD( pos ), -1, 1 );
   HB_STORNI( HIWORD( pos ), -1, 2 );
}

/*
 * FUNCTION COMBOSELECTSTRING()
 *
 * Selects an item in a ComboBox based on a string.
 *
 * Parameters:
 *   1: ComboBox handle (HWND). The handle of the ComboBox control.
 *   2: String (Character). The string to search for in the ComboBox list.
 *
 * Returns:
 *   LRESULT: The index of the selected item, or CB_ERR if the string is not found.
 *
 * Purpose:
 *   This function allows you to programmatically select an item in a ComboBox based on its text.
 *   This can be useful for setting the initial selection or for responding to user input.
 *
 * Notes:
 *   - The function uses the ComboBox_SelectString macro, which is a wrapper around the CB_SELECTSTRING message.
 *   - The search is case-insensitive.
 */
HB_FUNC( COMBOSELECTSTRING )
{
#ifndef UNICODE
   LPCTSTR  lpText = ( LPCTSTR ) hb_parc( 2 );
#else
   LPCWSTR  lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   hmg_ret_LRESULT( ComboBox_SelectString( hmg_par_raw_HWND( 1 ), -1, lpText ) );
}

/*
 * FUNCTION COMBOFINDSTRING()
 *
 * Finds the first item in a ComboBox that starts with a specified string.
 *
 * Parameters:
 *   1: ComboBox handle (HWND). The handle of the ComboBox control.
 *   2: String (Character). The string to search for in the ComboBox list.
 *
 * Returns:
 *   LRESULT: The index of the first item that matches the string, or CB_ERR if no match is found.
 *            The index is incremented by 1 before being returned to be 1-based.
 *
 * Purpose:
 *   This function allows you to search for an item in a ComboBox based on a partial string match.
 *   This can be useful for implementing auto-completion or for filtering the list of items.
 *
 * Notes:
 *   - The function uses the ComboBox_FindString macro, which is a wrapper around the CB_FINDSTRING message.
 *   - The search is case-insensitive.
 *   - The returned index is incremented by 1 to be 1-based for Harbour compatibility.
 */
HB_FUNC( COMBOFINDSTRING )
{
#ifndef UNICODE
   LPCTSTR  lpText = ( LPCTSTR ) hb_parc( 2 );
#else
   LPWSTR   lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   hmg_ret_LRESULT( ComboBox_FindString( hmg_par_raw_HWND( 1 ), -1, lpText ) + 1 );
#ifdef UNICODE
   hb_xfree( lpText );
#endif
}

/*
 * FUNCTION COMBOFINDSTRINGEXACT()
 *
 * Finds the first item in a ComboBox that exactly matches a specified string.
 *
 * Parameters:
 *   1: ComboBox handle (HWND). The handle of the ComboBox control.
 *   2: String (Character). The string to search for in the ComboBox list.
 *
 * Returns:
 *   LRESULT: The index of the first item that exactly matches the string, or CB_ERR if no match is found.
 *            The index is incremented by 1 before being returned to be 1-based.
 *
 * Purpose:
 *   This function allows you to search for an item in a ComboBox based on an exact string match.
 *   This can be useful for validating user input or for selecting a specific item from the list.
 *
 * Notes:
 *   - The function uses the ComboBox_FindStringExact macro, which is a wrapper around the CB_FINDSTRINGEXACT message.
 *   - The search is case-insensitive.
 *   - The returned index is incremented by 1 to be 1-based for Harbour compatibility.
 */
HB_FUNC( COMBOFINDSTRINGEXACT )
{
#ifndef UNICODE
   LPCTSTR  lpText = ( LPCTSTR ) hb_parc( 2 );
#else
   LPWSTR   lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   hmg_ret_LRESULT( ComboBox_FindStringExact( hmg_par_raw_HWND( 1 ), -1, lpText ) + 1 );
#ifdef UNICODE
   hb_xfree( lpText );
#endif
}

/*
 * FUNCTION COMBOGETSTRING()
 *
 * Retrieves the string at a specified index in a ComboBox.
 *
 * Parameters:
 *   1: ComboBox handle (HWND). The handle of the ComboBox control.
 *   2: Index (Numeric). The index of the string to retrieve (1-based).
 *
 * Returns:
 *   Character: The string at the specified index, or NULL if the index is invalid.
 *
 * Purpose:
 *   This function allows you to access the text of an item in a ComboBox based on its index.
 *   This can be useful for iterating through the list of items or for retrieving the text of the
 *   selected item.
 *
 * Notes:
 *   - The function uses the CB_GETLBTEXTLEN and CB_GETLBTEXT messages to retrieve the string.
 *   - The index is 1-based, so it is decremented by 1 before being passed to the Windows API.
 */
HB_FUNC( COMBOGETSTRING )
{
#ifdef UNICODE
   LPSTR lpString;
#endif
   int   iLen = ( int ) SendMessage( hmg_par_raw_HWND( 1 ), CB_GETLBTEXTLEN, ( WPARAM ) hb_parni( 2 ) - 1, ( LPARAM ) 0 );
   TCHAR *cString;
   if( iLen > 0 && NULL != ( cString = ( TCHAR * ) hb_xgrab( ( iLen + 1 ) * sizeof( TCHAR ) ) ) )
   {
      SendMessage( hmg_par_raw_HWND( 1 ), CB_GETLBTEXT, ( WPARAM ) hb_parni( 2 ) - 1, ( LPARAM ) cString );
#ifdef UNICODE
      lpString = WideToAnsi( cString );
      hb_retc( lpString );
      hb_xfree( lpString );
#else
      hb_retclen_buffer( cString, iLen );
#endif
   }
   else
   {
      hb_retc_null();
   }
}

/*
 * FUNCTION COMBOADDSTRING()
 *
 * Adds a string to the end of the list in a ComboBox.
 *
 * Parameters:
 *   1: ComboBox handle (HWND). The handle of the ComboBox control.
 *   2: String (Character). The string to add to the ComboBox list.
 *
 * Returns:
 *   LRESULT: The index of the newly added string, or CB_ERR if an error occurred.
 *
 * Purpose:
 *   This function allows you to dynamically add items to a ComboBox.
 *   This can be useful for populating the ComboBox with data from a database or other source.
 *
 * Notes:
 *   - The function uses the ComboBox_AddString macro, which is a wrapper around the CB_ADDSTRING message.
 */
HB_FUNC( COMBOADDSTRING )
{
#ifndef UNICODE
   LPCTSTR  lpString = ( LPCTSTR ) hb_parc( 2 );
#else
   LPWSTR   lpString = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   hmg_ret_LRESULT( ComboBox_AddString( hmg_par_raw_HWND( 1 ), lpString ) );
#ifdef UNICODE
   hb_xfree( lpString );
#endif
}

/*
 * FUNCTION COMBOINSERTSTRING()
 *
 * Inserts a string into the list of a ComboBox at a specified position.
 *
 * Parameters:
 *   1: ComboBox handle (HWND). The handle of the ComboBox control.
 *   2: String (Character). The string to insert into the ComboBox list.
 *   3: Index (Numeric). The index at which to insert the string (1-based).
 *
 * Returns:
 *   LRESULT: The index where the string was inserted, or CB_ERR if an error occurred.
 *
 * Purpose:
 *   This function allows you to dynamically insert items into a ComboBox at a specific position.
 *   This can be useful for maintaining a sorted list or for inserting items based on user input.
 *
 * Notes:
 *   - The function uses the ComboBox_InsertString macro, which is a wrapper around the CB_INSERTSTRING message.
 *   - The index is 1-based, so it is decremented by 1 before being passed to the Windows API.
 */
HB_FUNC( COMBOINSERTSTRING )
{
#ifndef UNICODE
   LPCTSTR  lpString = ( LPCTSTR ) hb_parc( 2 );
#else
   LPWSTR   lpString = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   hmg_ret_LRESULT( ComboBox_InsertString( hmg_par_raw_HWND( 1 ), hb_parni( 3 ) - 1, lpString ) );
#ifdef UNICODE
   hb_xfree( lpString );
#endif
}

/*
 * FUNCTION COMBOADDSTRINGEX()
 *
 * Adds a string with an associated image to the end of the list in a ComboBoxEx control.
 *
 * Parameters:
 *   1: ComboBoxEx handle (HWND). The handle of the ComboBoxEx control.
 *   2: String (Character). The string to add to the ComboBoxEx list.
 *   3: Image Index (Numeric). The index of the image to associate with the string.  This index is used to calculate the image, selected image, and overlay image indices within the ImageList.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   This function allows you to add items to a ComboBoxEx control with associated images.
 *   The image indices are calculated based on the provided image index, assuming that the ImageList
 *   is structured with three images per item (normal, selected, and overlay).
 *
 * Notes:
 *   - The function uses the CBEM_INSERTITEM message to insert the item into the ComboBoxEx control.
 *   - The image index is used to calculate the image, selected image, and overlay image indices.
 *   - The image index is 1-based, so it is decremented by 1 before being used in the calculations.
 *   - The ImageList should be structured with three images per item: normal, selected, and overlay.
 */
HB_FUNC( COMBOADDSTRINGEX )
{
#ifndef UNICODE
   LPTSTR         lpText = ( LPTSTR ) hb_parc( 2 );
#else
   LPWSTR         lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   int            nImage = hb_parni( 3 );
   COMBOBOXEXITEM cbei;

   cbei.mask = CBEIF_TEXT | CBEIF_INDENT | CBEIF_IMAGE | CBEIF_SELECTEDIMAGE | CBEIF_OVERLAY;
   cbei.iItem = -1;
   cbei.pszText = lpText;
   cbei.cchTextMax = ( int ) hb_parclen( 2 );
   cbei.iImage = ( nImage - 1 ) * 3;
   cbei.iSelectedImage = ( nImage - 1 ) * 3 + 1;
   cbei.iOverlay = ( nImage - 1 ) * 3 + 2;
   cbei.iIndent = 0;

   SendMessage( hmg_par_raw_HWND( 1 ), CBEM_INSERTITEM, 0, ( LPARAM ) & cbei );
#ifdef UNICODE
   hb_xfree( lpText );
#endif
}

/*
 * FUNCTION COMBOINSERTSTRINGEX()
 *
 * Inserts a string with an associated image into the list of a ComboBoxEx control at a specified position.
 *
 * Parameters:
 *   1: ComboBoxEx handle (HWND). The handle of the ComboBoxEx control.
 *   2: String (Character). The string to insert into the ComboBoxEx list.
 *   3: Image Index (Numeric). The index of the image to associate with the string. This index is used to calculate the image, selected image, and overlay image indices within the ImageList.
 *   4: Insertion Index (Numeric). The index at which to insert the string (1-based).
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   This function allows you to insert items into a ComboBoxEx control with associated images at a specific position.
 *   The image indices are calculated based on the provided image index, assuming that the ImageList
 *   is structured with three images per item (normal, selected, and overlay).
 *
 * Notes:
 *   - The function uses the CBEM_INSERTITEM message to insert the item into the ComboBoxEx control.
 *   - The image index is used to calculate the image, selected image, and overlay image indices.
 *   - The image index and insertion index are 1-based, so they are decremented by 1 before being used in the calculations.
 *   - The ImageList should be structured with three images per item: normal, selected, and overlay.
 */
HB_FUNC( COMBOINSERTSTRINGEX )
{
#ifndef UNICODE
   LPTSTR         lpText = ( LPTSTR ) hb_parc( 2 );
#else
   LPWSTR         lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   int            nImage = hb_parni( 3 );
   COMBOBOXEXITEM cbei;

   cbei.mask = CBEIF_TEXT | CBEIF_INDENT | CBEIF_IMAGE | CBEIF_SELECTEDIMAGE | CBEIF_OVERLAY;
   cbei.iItem = hb_parni( 4 ) - 1;
   cbei.pszText = lpText;
   cbei.cchTextMax = ( int ) hb_parclen( 2 );
   cbei.iImage = ( nImage - 1 ) * 3;
   cbei.iSelectedImage = ( nImage - 1 ) * 3 + 1;
   cbei.iOverlay = ( nImage - 1 ) * 3 + 2;
   cbei.iIndent = 0;

   SendMessage( hmg_par_raw_HWND( 1 ), CBEM_INSERTITEM, 0, ( LPARAM ) & cbei );
#ifdef UNICODE
   hb_xfree( lpText );
#endif
}

/*
 * FUNCTION COMBOADDDATASTRINGEX()
 *
 * Adds a string to the end of the list in a ComboBoxEx control, using default image indices (0, 1, and 2).
 *
 * Parameters:
 *   hWnd  : The handle (HWND) of the ComboBoxEx control.
 *   cText : The string (character array) to add to the ComboBoxEx.
 *
 * Returns:
 *   None. This function does not return a value. It directly modifies the ComboBoxEx control.
 *
 * Purpose:
 *   This function provides a simplified way to add items to a ComboBoxEx control when default images are sufficient.
 *   It's useful for quickly populating the ComboBoxEx with data without needing to specify image indices explicitly.
 *   The default image indices (0, 1, and 2) are typically used to represent the normal, selected, and overlay images, respectively.
 *
 * Notes:
 *   - The function always adds the string to the end of the list (i.e., it appends the string).
 *   - The function handles Unicode/ANSI conversion based on the compilation settings.
 */
HB_FUNC( COMBOADDDATASTRINGEX )
{
#ifndef UNICODE
   LPTSTR         lpText = ( LPTSTR ) hb_parc( 2 );
#else
   LPWSTR         lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   COMBOBOXEXITEM cbei;

   cbei.mask = CBEIF_TEXT | CBEIF_INDENT | CBEIF_IMAGE | CBEIF_SELECTEDIMAGE | CBEIF_OVERLAY;
   cbei.iItem = -1;  // -1 indicates that the item should be added to the end of the list.
   cbei.pszText = lpText;
   cbei.cchTextMax = ( int ) hb_parclen( 2 );
   cbei.iImage = 0;
   cbei.iSelectedImage = 1;
   cbei.iOverlay = 2;
   cbei.iIndent = 0;

   SendMessage( hmg_par_raw_HWND( 1 ), CBEM_INSERTITEM, 0, ( LPARAM ) & cbei );
#ifdef UNICODE
   hb_xfree( lpText );
#endif
}
