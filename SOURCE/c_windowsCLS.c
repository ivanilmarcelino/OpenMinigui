/*
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This    program  is  free  software;  you can redistribute it and/or modify
   it under  the  terms  of the GNU General Public License as published by the
   Free  Software   Foundation;  either  version 2 of the License, or (at your
   option) any later version.

   This   program   is   distributed  in  the hope that it will be useful, but
   WITHOUT    ANY    WARRANTY;    without   even   the   implied  warranty  of
   MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You   should  have  received  a copy of the GNU General Public License along
   with   this   software;   see  the  file COPYING. If not, write to the Free
   Software   Foundation,   Inc.,   59  Temple  Place,  Suite  330, Boston, MA
   02111-1307 USA (or visit the web site http://www.gnu.org/).

   As   a   special  exception, you have permission for additional uses of the
   text  contained  in  this  release  of  Harbour Minigui.

   The   exception   is that,   if   you  link  the  Harbour  Minigui  library
   with  other    files   to  produce   an   executable,   this  does  not  by
   itself   cause  the   resulting   executable    to   be  covered by the GNU
   General  Public  License.  Your    use  of that   executable   is   in   no
   way  restricted on account of linking the Harbour-Minigui library code into
   it.

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

   Parts of this code are contributed and used here under permission of the
   author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */
#include <mgdefs.h>        // MiniGUI definitions and macros.
#include "hbapierr.h"      // Harbour error-handling functions.
#include "hbapistr.h"      // Harbour string handling functions.

// Compatibility definition for xHarbour version.
#ifdef __XHARBOUR__
#define hb_storclen_buffer hb_storclenAdopt
#endif
#ifdef UNICODE
// Function declaration for checking if a window has a specific class name in Unicode.
BOOL  _isValidCtrlClassW( HWND hwndTip, LPWSTR ClassName );

/*
 * FUNCTION _isValidCtrlClassW( hwndTip, ClassName )
 *
 * Checks if a window has a specific class name (Unicode version).
 *
 * Parameters:
 *   hwndTip   : The handle of the window to check (HWND).
 *   ClassName : A pointer to a wide character string containing the class name to compare against (LPWSTR).
 *
 * Returns:
 *   A logical value indicating whether the window's class name matches the specified class name.
 *   Returns TRUE if the class names match, FALSE otherwise.
 *
 * Purpose:
 *   This function is used to verify the type of a window based on its class name.
 *   It's crucial for ensuring that a window is of the expected type before performing
 *   operations specific to that type.  For example, it can be used to confirm that a
 *   window is a button before attempting to retrieve its button-specific properties.
 *
 * Notes:
 *   This function is compiled only when the UNICODE preprocessor directive is defined.
 *   It uses wide character strings (LPWSTR) for handling Unicode class names.
 */
BOOL _isValidCtrlClassW( HWND hwndTip, LPWSTR ClassName )
{
   TCHAR lpClassName[256]; // Buffer to store the retrieved class name.
   int   iLen = 0;

   // Check if the window is valid and retrieve the class name.
   if( IsWindow( hwndTip ) )
   {
      iLen = GetClassNameW( hwndTip, lpClassName, 256 ); // Get the class name in Unicode.
   }

   // Compare the retrieved class name with the given class name and return TRUE if they match.
   if( ( iLen > 0 ) && ( hb_wstrncmp( ( TCHAR * ) lpClassName, ClassName, iLen ) == 0 ) )
   {
      return TRUE;
   }
   else
   {
      return FALSE;
   }
}

#else
// Function declaration for checking if a window has a specific class name in ASCII.
BOOL  _isValidCtrlClassA( HWND hwndTip, const char *ClassName );

/*
 * FUNCTION _isValidCtrlClassA( hwndTip, ClassName )
 *
 * Checks if a window has a specific class name (ASCII version).
 *
 * Parameters:
 *   hwndTip   : The handle of the window to check (HWND).
 *   ClassName : A pointer to a null-terminated string containing the class name to compare against (const char *).
 *
 * Returns:
 *   A logical value indicating whether the window's class name matches the specified class name.
 *   Returns TRUE if the class names match, FALSE otherwise.
 *
 * Purpose:
 *   This function is used to verify the type of a window based on its class name.
 *   It's crucial for ensuring that a window is of the expected type before performing
 *   operations specific to that type.  For example, it can be used to confirm that a
 *   window is a button before attempting to retrieve its button-specific properties.
 *
 * Notes:
 *   This function is compiled only when the UNICODE preprocessor directive is *not* defined.
 *   It uses standard ASCII strings (const char *) for handling class names.
 */
BOOL _isValidCtrlClassA( HWND hwndTip, const char *ClassName )
{
   char  lpClassName[256]; // Buffer for storing class name in ASCII.
   int   iLen = 0;

   // Check if the window handle is valid and retrieve the class name.
   if( IsWindow( hwndTip ) )
   {
      iLen = GetClassNameA( hwndTip, lpClassName, 256 ); // Get the class name in ASCII.
   }

   // Compare the retrieved class name with the specified class name and return TRUE if they match.
   if( ( iLen > 0 ) && ( strncmp( ( const char * ) lpClassName, ClassName, iLen ) == 0 ) )
   {
      return TRUE;
   }
   else
   {
      return FALSE;
   }
}
#endif /* UNICODE */

/*
 * FUNCTION GETCLASSNAME( hwnd )
 *
 * Retrieves the class name of a window and returns it as a string.
 *
 * Parameters:
 *   hwnd : The handle of the window whose class name is to be retrieved (HWND).
 *
 * Returns:
 *   A character string containing the class name of the window. Returns a null string if the window handle is invalid or if the class name cannot be retrieved.
 *
 * Purpose:
 *   This function allows retrieving the class name of a window, which can be used to identify the type of control or window.
 *   This is useful for dynamic UI manipulation, debugging, and implementing generic functions that operate on different types of windows.
 *   For example, you might use this function to determine if a window is a button, a textbox, or a listbox before performing specific actions.
 *
 * Notes:
 *   If the window handle is invalid, the function triggers a runtime error.
 */
HB_FUNC( GETCLASSNAME )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );    // Retrieve the window handle from the function's first parameter.
   if( IsWindow( hwnd ) )                 // Check if the handle corresponds to a valid window.
   {
      char  ClassName[256];               // Buffer to hold the window class name.
      int   iLen;

      iLen = GetClassNameA( hwnd, ClassName, sizeof( ClassName ) / sizeof( char ) );

      if( iLen > 0 )                      // If the class name is retrieved successfully, return it as a string.
      {
         hb_retclen( ( const char * ) ClassName, iLen );
      }
      else
      {
         hb_retc_null();                  // If retrieval fails, return a null string.
      }
   }
   else
   {
      // Return an error if the window handle is invalid.
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/*
 * FUNCTION GETCLASSNAMEBYREF( hwnd, @cBuffer )
 *
 * Retrieves the class name of a window and stores it in a provided buffer.
 *
 * Parameters:
 *   hwnd    : The handle of the window whose class name is to be retrieved (HWND).
 *   @cBuffer : A character string variable passed by reference. The function will store the window's class name in this variable.
 *              The size of the buffer must be large enough to hold the class name.
 *
 * Returns:
 *   A numeric value representing the length of the class name stored in the buffer. Returns 0 if the window handle is invalid,
 *   the buffer size is insufficient, or if the class name cannot be retrieved.
 *
 * Purpose:
 *   This function allows retrieving the class name of a window and storing it in a pre-allocated buffer.
 *   This is useful when you need to avoid dynamic memory allocation or when you have a fixed-size buffer available.
 *   The function returns the length of the class name, allowing you to work with the string without relying on null termination.
 *
 * Notes:
 *   The function modifies the content of the variable passed as the second parameter (@cBuffer).
 *   It's the caller's responsibility to ensure that the buffer is large enough to hold the class name.
 */
HB_FUNC( GETCLASSNAMEBYREF )
{
   HWND     hwnd = hmg_par_raw_HWND( 1 ); // Get the window handle from the function's first parameter.
   HB_SIZE  nLen = hb_parcsiz( 2 );       // Get the size of the buffer for storing the class name.
   hb_retni( 0 );                         // Default return value (0) if retrieval fails.
   if( IsWindow( hwnd ) && nLen > 1 )                    // Proceed if the window is valid and buffer size is adequate.
   {
      char  *pBuffer = ( char * ) hb_xgrab( nLen + 1 );  // Allocate memory for the buffer.
      if( pBuffer )
      {
         int   nResult = GetClassNameA( hwnd, pBuffer, ( int ) nLen );

         if( nResult > 0 )             // Store and return the class name if retrieval succeeds.
         {
            hb_retni( hb_storclen_buffer( pBuffer, ( HB_SIZE ) nResult, 2 ) );
         }
         else
         {
            hb_xfree( pBuffer );       // Free memory if retrieval fails.
         }
      }
   }
}

/*
 * FUNCTION GETWINDOWLONG( hwnd, nIndex )
 *
 * Retrieves information about the specified window.
 *
 * Parameters:
 *   hwnd   : The handle of the window from which to retrieve information (HWND).
 *   nIndex : The zero-based offset to the value to be retrieved.  This parameter specifies the relative offset into the
 *            WNDCLASSEX structure. Valid values include GWL_EXSTYLE, GWL_STYLE, GWL_ID, GWL_USERDATA, and DWLP_MSGRESULT.
 *
 * Returns:
 *   A numeric value representing the requested window information. The meaning of the return value depends on the value of nIndex.
 *   Returns 0 if the function fails or if the window handle is invalid.
 *
 * Purpose:
 *   This function provides access to various properties of a window, such as its style, extended style, ID, and user data.
 *   It's a fundamental function for customizing and controlling the behavior of windows.
 *   For example, you can use this function to retrieve the current style of a window and then modify it to change its appearance or behavior.
 *
 * Notes:
 *   If the window handle is invalid, the function triggers a runtime error.
 */
HB_FUNC( GETWINDOWLONG )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 ); // Get the window handle.
   if( IsWindow( hwnd ) )              // Verify that the window is valid.
   {
      HB_RETNL( GetWindowLongPtr( hwnd, hb_parni( 2 ) ) );  // Retrieve and return the window property.
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS ); // Error handling.
   }
}

/*
 * FUNCTION SETWINDOWLONG( hwnd, nIndex, nNewValue )
 *
 * Changes an attribute of the specified window.
 *
 * Parameters:
 *   hwnd      : The handle of the window whose attribute is to be changed (HWND).
 *   nIndex    : The zero-based offset to the value to be set.  This parameter specifies the relative offset into the
 *               WNDCLASSEX structure. Valid values include GWL_EXSTYLE, GWL_STYLE, GWL_ID, GWL_USERDATA, and DWLP_MSGRESULT.
 *   nNewValue : The new value to be set for the specified attribute (LONG_PTR).
 *
 * Returns:
 *   A numeric value representing the previous value of the attribute. Returns 0 if the function fails or if the window handle is invalid.
 *
 * Purpose:
 *   This function allows modifying various properties of a window, such as its style, extended style, ID, and user data.
 *   It's a fundamental function for customizing and controlling the behavior of windows.
 *   For example, you can use this function to change the style of a window to add or remove borders, change its appearance, or enable/disable certain features.
 *
 * Notes:
 *   If the window handle is invalid, the function triggers a runtime error.
 */
HB_FUNC( SETWINDOWLONG )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hwnd ) )
   {
      HB_RETNL( SetWindowLongPtr( hwnd, hb_parni( 2 ), hb_parnl( 3 ) ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/*
 * FUNCTION GETWINDOWSTYLE( hwnd )
 *
 * Retrieves the style of the specified window.
 *
 * Parameters:
 *   hwnd : The handle of the window whose style is to be retrieved (HWND).
 *
 * Returns:
 *   A numeric value representing the window's style (LONG_PTR). Returns 0 if the function fails or if the window handle is invalid.
 *
 * Purpose:
 *   This function allows retrieving the current style of a window, which defines its appearance and behavior.
 *   The style is a combination of flags that specify various attributes, such as the presence of borders, title bar, scroll bars, and other features.
 *   This function is often used in conjunction with SETWINDOWSTYLE to modify the window's style dynamically.
 *
 * Notes:
 *   If the window handle is invalid, the function triggers a runtime error.
 */
HB_FUNC( GETWINDOWSTYLE )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hwnd ) )
   {
      HB_RETNL( GetWindowLongPtr( hwnd, GWL_STYLE ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/*
 * FUNCTION SETWINDOWSTYLE( hwnd, nNewStyle, lAddOrRemove )
 *
 * Sets or modifies the style of the specified window.
 *
 * Parameters:
 *   hwnd         : The handle of the window whose style is to be changed (HWND).
 *   nNewStyle    : The style flags to be added or removed (LONG_PTR).
 *   lAddOrRemove : A logical value indicating whether to add or remove the specified style flags.
 *                  .T. (TRUE) to add the style flags.
 *                  .F. (FALSE) to remove the style flags.
 *
 * Returns:
 *   A numeric value representing the previous style of the window (LONG_PTR). Returns 0 if the function fails or if the window handle is invalid.
 *
 * Purpose:
 *   This function allows dynamically modifying the style of a window by adding or removing specific style flags.
 *   This is useful for changing the appearance or behavior of a window at runtime, such as adding or removing borders, enabling or disabling scroll bars, or changing the window's size and position.
 *   For example, you can use this function to add the WS_MAXIMIZEBOX style to a window to enable the maximize button, or remove the WS_BORDER style to remove the window's border.
 *
 * Notes:
 *   If the window handle is invalid, the function triggers a runtime error.
 */
HB_FUNC( SETWINDOWSTYLE )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hwnd ) )
   {
      LONG_PTR OldStyle = GetWindowLongPtr( hwnd, GWL_STYLE ); // Retrieve current window style.
      LONG_PTR NewStyle = hmg_par_raw_LONG_PTR( 2 );           // New style to be set.
      HB_RETNL
      (
         SetWindowLongPtr
            (
               hwnd,
               GWL_STYLE,  // Update the window style based on the 3rd parameter.
               ( ( BOOL ) hb_parl( 3 ) ) ? OldStyle | NewStyle : OldStyle & ( ~NewStyle )
            )
      );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/*
 * FUNCTION ISWINDOWHASSTYLE( hwnd, nStyle )
 *
 * Checks if a window has a specific style applied.
 *
 * Parameters:
 *   hwnd   : The handle of the window to check (HWND).
 *   nStyle : The style flag to check for (LONG_PTR).
 *
 * Returns:
 *   A logical value indicating whether the window has the specified style applied.
 *   Returns .T. (TRUE) if the window has the style, .F. (FALSE) otherwise.
 *
 * Purpose:
 *   This function allows determining if a window has a particular style flag set.
 *   This is useful for conditional logic based on the window's style, such as enabling or disabling certain features based on whether a specific style is present.
 *   For example, you can use this function to check if a window has the WS_BORDER style before attempting to remove it.
 *
 * Notes:
 *   If the window handle is invalid, the function triggers a runtime error.
 */
HB_FUNC( ISWINDOWHASSTYLE )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hwnd ) )
   {
      LONG_PTR Style = GetWindowLongPtr( hwnd, GWL_STYLE );       // Retrieve the current style.
      hmg_ret_L( ( Style & hmg_par_raw_LONG_PTR( 2 ) ) );         // Check if the specified style is applied.
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/*
 * FUNCTION ISWINDOWHASEXSTYLE( hwnd, nExStyle )
 *
 * Checks if a window has a specific extended style applied.
 *
 * Parameters:
 *   hwnd     : The handle of the window to check (HWND).
 *   nExStyle : The extended style flag to check for (LONG_PTR).
 *
 * Returns:
 *   A logical value indicating whether the window has the specified extended style applied.
 *   Returns .T. (TRUE) if the window has the extended style, .F. (FALSE) otherwise.
 *
 * Purpose:
 *   This function allows determining if a window has a particular extended style flag set.
 *   Extended styles control aspects of a window's appearance and behavior that are not covered by the standard styles.
 *   This is useful for conditional logic based on the window's extended style, such as enabling or disabling certain features based on whether a specific extended style is present.
 *   For example, you can use this function to check if a window has the WS_EX_CLIENTEDGE extended style before attempting to modify its border.
 *
 * Notes:
 *   If the window handle is invalid, the function triggers a runtime error.
 */
HB_FUNC( ISWINDOWHASEXSTYLE )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hwnd ) )
   {
      LONG_PTR ExStyle = GetWindowLongPtr( hwnd, GWL_EXSTYLE );   // Get the current extended style.
      hmg_ret_L( ( ExStyle & hmg_par_raw_LONG_PTR( 2 ) ) );       // Check if the specified extended style is applied.
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}
