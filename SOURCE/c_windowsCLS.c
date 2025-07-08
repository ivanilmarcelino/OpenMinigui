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

   You   should  have  received a copy of the GNU General Public License along
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

   Parts  of  this  code  is contributed and used here under permission of his
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

// Function to validate if a given window handle (HWND) has a specified control class name in Unicode.
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

/* Check if the window class name matches the given class name. */
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
#endif

/* 
   Function to retrieve the class name of a given window handle and return it as a string.
 */
HB_FUNC( GETCLASSNAME )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );                   // Retrieve the window handle from the function's first parameter.
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
   Function to retrieve the class name of a window by reference and return its length.
 */
HB_FUNC( GETCLASSNAMEBYREF )
{
   HWND     hwnd = hmg_par_raw_HWND( 1 ); // Get the window handle from the function's first parameter.
   HB_SIZE  nLen = hb_parcsiz( 2 );       // Get the size of the buffer for storing the class name.

   hb_retni( 0 ); // Default return value (0) if retrieval fails.

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
   Function to retrieve a specific property of a window (like style or other window parameters).
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
   Function to set a specific property (e.g., style) of a window and return the old property value.
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
   Function to get the style of a window and return it for style-related operations.
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
   Function to set or modify a window's style by adding or removing a specified style.
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
   Function to check if a window has a specified style applied and return TRUE or FALSE.
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
   Function to check if a window has a specific extended style applied.
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
