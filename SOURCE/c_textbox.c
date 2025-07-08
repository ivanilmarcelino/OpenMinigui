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
#include <mgdefs.h>     // MiniGUI definitions
#include <commctrl.h>   // Common controls library for Windows

// For older versions of Borland C++ (prior to version 5.8), define WC_EDIT as "Edit" to ensure compatibility
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
   #define WC_EDIT   "Edit"
#endif
#include "hbvm.h"       // Harbour Virtual Machine header for interfacing with Harbour

// Function prototype for a custom window procedure for Edit controls
LRESULT CALLBACK  OwnEditProc( HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam );

// Retrieve the application instance handle
HINSTANCE         GetInstance( void );

/**
 * INITMASKEDTEXTBOX - Initializes a masked textbox control with specified properties.
 * Configures the style based on input parameters and applies custom settings.
 */
HB_FUNC( INITMASKEDTEXTBOX )
{
   HWND  hedit;         // Handle for the edit control
   DWORD Style = WS_CHILD | ES_AUTOHSCROLL;  // Base style for the edit control

   // Adjust the style based on input parameters
   if( hb_parl( 9 ) )         // If uppercase is enabled
   {
      Style |= ES_UPPERCASE;
   }
   else if( hb_parl( 10 ) )   // If lowercase is enabled
   {
      Style |= ES_LOWERCASE;
   }

   if( hb_parl( 12 ) )        // Right-align text if specified
   {
      Style |= ES_RIGHT;
   }

   if( hb_parl( 13 ) )        // Set to read-only mode if specified
   {
      Style |= ES_READONLY;
   }

   if( !hb_parl( 14 ) )       // Show the control if visibility is enabled
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 15 ) )       // Enable tab stop if required
   {
      Style |= WS_TABSTOP;
   }

   // Create the Edit control window with extended and standard styles
   hedit = CreateWindowEx
      (
         hb_parl( 16 ) ? 0 : WS_EX_CLIENTEDGE,  // Optional client-edge style
         WC_EDIT,                // Class name for Edit control
         TEXT( "" ),             // No initial text
         Style,                  // Combined styles
         hb_parni( 3 ),          // X position
         hb_parni( 4 ),          // Y position
         hb_parni( 5 ),          // Width
         hb_parni( 11 ),         // Height
         hmg_par_raw_HWND( 1 ),  // Parent window handle
         hmg_par_raw_HMENU( 2 ), // Menu or control ID
         GetInstance(),          // Application instance handle
         NULL                    // Additional parameters
      );

   // Store the original window procedure and apply a custom subclass procedure
   SetProp( ( HWND ) hedit, TEXT( "oldeditproc" ), ( HWND ) GetWindowLongPtr( ( HWND ) hedit, GWLP_WNDPROC ) );
   SubclassWindow2( hedit, OwnEditProc );

   hmg_ret_raw_HWND( hedit );    // Return the handle of the created control
}

/**
 * INITTEXTBOX - Initializes a standard textbox control.
 * Configures the style based on input parameters, including numeric and password fields.
 */
HB_FUNC( INITTEXTBOX )
{
   HWND  hedit;                  // Handle for the edit control
   DWORD iStyle = WS_CHILD | ES_AUTOHSCROLL | BS_FLAT;   // Base style for textbox control

   // Apply specific styles based on input parameters
   if( hb_parl( 12 ) )           // Enable numeric input only
   {
      iStyle |= ES_NUMBER;
   }
   else
   {
      if( hb_parl( 10 ) )        // Enable uppercase
      {
         iStyle |= ES_UPPERCASE;
      }
      else if( hb_parl( 11 ) )   // Enable lowercase
      {
         iStyle |= ES_LOWERCASE;
      }
   }

   if( hb_parl( 13 ) )           // Enable password masking
   {
      iStyle |= ES_PASSWORD;
   }

   if( hb_parl( 14 ) )           // Right-align text if specified
   {
      iStyle |= ES_RIGHT;
   }

   if( hb_parl( 15 ) )           // Set control as read-only
   {
      iStyle |= ES_READONLY;
   }

   if( !hb_parl( 16 ) )          // Show the control if visibility is enabled
   {
      iStyle |= WS_VISIBLE;
   }

   if( !hb_parl( 17 ) )          // Enable tab stop if required
   {
      iStyle |= WS_TABSTOP;
   }

   // Create the Edit control window
   hedit = CreateWindowEx
      (
         hb_parl( 18 ) ? 0 : WS_EX_CLIENTEDGE,  // Optional client-edge style
         WC_EDIT,                            // Class name for Edit control
         TEXT( "" ),                         // No initial text
         iStyle,                             // Combined styles
         hb_parni( 3 ),                      // X position
         hb_parni( 4 ),                      // Y position
         hb_parni( 5 ),                      // Width
         hb_parni( 6 ),                      // Height
         hmg_par_raw_HWND( 1 ),              // Parent window handle
         hmg_par_raw_HMENU( 2 ),             // Menu or control ID
         GetInstance(),                      // Application instance handle
         NULL                                // Additional parameters
      );

   // Limit the maximum text length based on parameter
   SendMessage( hedit, EM_LIMITTEXT, hmg_par_WPARAM( 9 ), ( LPARAM ) 0 );

   // Store original window procedure and apply custom subclass procedure
   SetProp( ( HWND ) hedit, TEXT( "oldeditproc" ), ( HWND ) GetWindowLongPtr( ( HWND ) hedit, GWLP_WNDPROC ) );
   SubclassWindow2( hedit, OwnEditProc );

   hmg_ret_raw_HWND( hedit );                // Return the handle of the created control
}

/**
 * INITCHARMASKTEXTBOX - Initializes a character-masked textbox control.
 * Configures style settings for text alignment, read-only, and visibility options.
 */
HB_FUNC( INITCHARMASKTEXTBOX )
{
   HWND  hedit;                              // Handle for the edit control
   DWORD Style = WS_CHILD | ES_AUTOHSCROLL;  // Base style for the character-masked textbox

   // Apply specific styles based on input parameters
   if( hb_parl( 9 ) )         // Enable uppercase
   {
      Style |= ES_UPPERCASE;
   }
   else if( hb_parl( 10 ) )   // Enable lowercase
   {
      Style |= ES_LOWERCASE;
   }

   if( hb_parl( 12 ) )        // Right-align text if specified
   {
      Style |= ES_RIGHT;
   }

   if( hb_parl( 13 ) )        // Set control as read-only
   {
      Style |= ES_READONLY;
   }

   if( !hb_parl( 14 ) )       // Show the control if visibility is enabled
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 15 ) )       // Enable tab stop if required
   {
      Style |= WS_TABSTOP;
   }

   // Create the Edit control window
   hedit = CreateWindowEx
      (
         hb_parl( 16 ) ? 0 : WS_EX_CLIENTEDGE,  // Optional client-edge style
         WC_EDIT,                      // Class name for Edit control
         TEXT( "" ),                   // No initial text
         Style,                        // Combined styles
         hb_parni( 3 ),                // X position
         hb_parni( 4 ),                // Y position
         hb_parni( 5 ),                // Width
         hb_parni( 11 ),               // Height
         hmg_par_raw_HWND( 1 ),        // Parent window handle
         hmg_par_raw_HMENU( 2 ),       // Menu or control ID
         GetInstance(),                // Application instance handle
         NULL                          // Additional parameters
      );

   // Store the original window procedure and apply custom subclass procedure
   SetProp( ( HWND ) hedit, TEXT( "oldeditproc" ), ( HWND ) GetWindowLongPtr( ( HWND ) hedit, GWLP_WNDPROC ) );
   SubclassWindow2( hedit, OwnEditProc );

   hmg_ret_raw_HWND( hedit );          // Return the handle of the created control
}

/**
 * OwnEditProc - Custom window procedure for Edit controls to handle specific messages.
 * Processes events such as context menu and character input for custom behaviors.
 */
LRESULT CALLBACK OwnEditProc( HWND hButton, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB   pSymbol = NULL;   // Symbol for dynamically loaded Harbour procedure
   LRESULT           r;                // Result to be returned to the caller
   WNDPROC           OldWndProc;       // Original window procedure

   // Retrieve the stored original window procedure
   OldWndProc = ( WNDPROC ) ( HB_PTRUINT ) GetProp( hButton, TEXT( "oldeditproc" ) );

   switch( Msg )
   {
      case WM_DESTROY:                 // Clean up properties on destroy
         SubclassWindow2( hButton, OldWndProc );    // Restore original window procedure
         RemoveProp( hButton, TEXT( "oldeditproc" ) ); // Remove stored property
         break;

      case WM_CONTEXTMENU:             // Handle right-click context menu
      case WM_CHAR:                    // Handle character input
         if( !pSymbol )                // Load symbol dynamically if not loaded
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OEDITEVENTS" ) );
         }

         if( pSymbol )                 // Invoke Harbour callback if symbol exists
         {
            hb_vmPushSymbol( pSymbol );               // Push symbol for the Harbour function
            hb_vmPushNil();                           // Push Nil for the object instance
            hb_vmPushNumInt( ( HB_PTRUINT ) hButton ); // Push button handle
            hb_vmPushLong( Msg );                     // Push message
            hb_vmPushNumInt( wParam );                // Push WPARAM
            hb_vmPushNumInt( lParam );                // Push LPARAM
            hb_vmDo( 4 );                             // Call the Harbour function
         }

         // Retrieve the result and decide whether to return it or call original procedure
         r = hmg_par_LRESULT( -1 );
         return( r != 0 ) ? r : CallWindowProc( OldWndProc, hButton, Msg, wParam, lParam );
   }

   return CallWindowProc( OldWndProc, hButton, Msg, wParam, lParam ); // Default processing
}
