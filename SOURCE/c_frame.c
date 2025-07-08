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
 */

#include <mgdefs.h>           // Include Minigui framework definitions
#include <commctrl.h>         // Include common controls library for Windows

// Define button control class for older versions of Borland C++ compilers
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_BUTTON "Button"    // Button class name definition for backward compatibility
#endif
#include "hbapierr.h"         // Include Harbour error handling API

// Function prototypes
#ifdef UNICODE
// Prototype for a function to convert ANSI strings to wide strings (Unicode)
LPWSTR      AnsiToWide( LPCSTR );
#endif

// Prototype for a function that retrieves the application instance
HINSTANCE   GetInstance( void );

/* Function: INITFRAME
   Purpose: Initializes and creates a group box (frame) within a specified window.
   
   Parameters:
      1. HWND hwnd         - Handle of the parent window.
      2. HMENU hmenu       - Handle or identifier for the menu, optional for a group box.
      3-6. int x, y, width, height - Coordinates and dimensions for positioning the frame.
      7. LPCSTR windowTitle - Title or label text of the group box.
      10. bool transparent - TRUE if the background should be opaque, FALSE for transparent.

   Returns:
      Handle to the created group box, or NULL if an error occurs.
*/
HB_FUNC( INITFRAME )
{
   // Retrieve the parent window handle from the function parameters
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   // Check if the provided window handle is valid
   if( IsWindow( hwnd ) )
   {
      // Retrieve the menu handle or ID from parameters (optional for group boxes)
      HMENU hmenu = hmg_par_raw_HMENU( 2 );

      // Set extended window style based on transparency parameter
      DWORD dwExStyle = hb_parl( 10 ) ? 0 : WS_EX_TRANSPARENT;

#ifndef UNICODE
      // Use ANSI string for the group box title if not compiling with Unicode
      LPCSTR   lpWindowName = hb_parc( 7 );
#else
      // Convert the ANSI title to Unicode if compiling with Unicode
      LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 7 ) );
#endif

      // Create the group box using the CreateWindowEx function
      hmg_ret_raw_HWND
      (
         CreateWindowEx
            (
               dwExStyle,     // Extended window style (transparency option)
               WC_BUTTON,     // Class name for button control (used for group boxes)
               lpWindowName,  // Title or label text for the group box
               WS_CHILD | WS_VISIBLE | // Basic child and visible styles for the control
               BS_GROUPBOX | BS_NOTIFY, // Group box style with notification enabled
               hb_parni( 3 ), // X-coordinate of the frame
               hb_parni( 4 ), // Y-coordinate of the frame
               hb_parni( 5 ), // Width of the frame
               hb_parni( 6 ), // Height of the frame
               hwnd,          // Parent window handle
               ( IsMenu( hmenu ) ? hmenu : NULL ), // Menu handle, or NULL if invalid
               GetInstance(), // Handle to the application instance
               NULL           // Additional data (not used)
            )
      );

      // Free allocated memory for the title if UNICODE is used
#ifdef UNICODE
      hb_xfree( ( TCHAR * ) lpWindowName );
#endif
   }
   else
   {
      // Raise an error if the window handle is invalid, specifying an argument error
      hb_errRT_BASE_SubstR( EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}
