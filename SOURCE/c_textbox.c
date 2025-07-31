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

/*
 * FUNCTION INITMASKEDTEXTBOX
 *
 * Initializes a masked textbox control with specified properties.
 *
 * Parameters:
 *   1: HWND (Parent Window Handle) - The handle of the parent window to which the textbox will be attached.
 *   2: HMENU (Control ID) - The ID of the textbox control.  This is used to identify the control within the parent window.
 *   3: INT (X Position) - The X coordinate of the top-left corner of the textbox, relative to the parent window.
 *   4: INT (Y Position) - The Y coordinate of the top-left corner of the textbox, relative to the parent window.
 *   5: INT (Width) - The width of the textbox control.
 *   9: LOGICAL (Uppercase) - A logical value indicating whether the input should be automatically converted to uppercase.  .T. for uppercase, .F. otherwise.
 *  10: LOGICAL (Lowercase) - A logical value indicating whether the input should be automatically converted to lowercase.  .T. for lowercase, .F. otherwise.
 *  12: LOGICAL (Right Align) - A logical value indicating whether the text should be right-aligned within the textbox.  .T. for right-aligned, .F. otherwise.
 *  13: LOGICAL (Read Only) - A logical value indicating whether the textbox should be read-only.  .T. for read-only, .F. otherwise.
 *  14: LOGICAL (Visible) - A logical value indicating whether the textbox should be initially visible.  .T. for visible, .F. for hidden.
 *  15: LOGICAL (Tab Stop) - A logical value indicating whether the textbox should be included in the tab order.  .T. for tab stop, .F. otherwise.
 *  11: INT (Height) - The height of the textbox control.
 *  16: LOGICAL (Client Edge) - A logical value indicating whether the textbox should have a client edge (sunken border). .T. for no client edge, .F. for client edge.
 *
 * Return Value:
 *   HWND - The handle of the newly created masked textbox control.
 *
 * Purpose:
 *   This function creates a masked textbox control, allowing for customized input formatting and validation.
 *   It's used to create specialized text input fields with features like automatic case conversion, alignment, and read-only status.
 *   Example Usage: Creating a masked textbox for entering a phone number or a social security number.
 *
 * Notes:
 *   The "masked" aspect of this textbox is not directly implemented in this function.  It likely relies on the custom window procedure (OwnEditProc)
 *   and/or additional Harbour code to handle the masking logic.  The parameters here primarily control the basic style and appearance of the textbox.
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

/*
 * FUNCTION INITTEXTBOX
 *
 * Initializes a standard textbox control with specified properties.
 *
 * Parameters:
 *   1: HWND (Parent Window Handle) - The handle of the parent window to which the textbox will be attached.
 *   2: HMENU (Control ID) - The ID of the textbox control.  This is used to identify the control within the parent window.
 *   3: INT (X Position) - The X coordinate of the top-left corner of the textbox, relative to the parent window.
 *   4: INT (Y Position) - The Y coordinate of the top-left corner of the textbox, relative to the parent window.
 *   5: INT (Width) - The width of the textbox control.
 *   6: INT (Height) - The height of the textbox control.
 *   9: INT (Max Length) - The maximum number of characters that can be entered into the textbox.
 *  10: LOGICAL (Uppercase) - A logical value indicating whether the input should be automatically converted to uppercase.  .T. for uppercase, .F. otherwise.
 *  11: LOGICAL (Lowercase) - A logical value indicating whether the input should be automatically converted to lowercase.  .T. for lowercase, .F. otherwise.
 *  12: LOGICAL (Numeric) - A logical value indicating whether the textbox should only accept numeric input.  .T. for numeric only, .F. otherwise.
 *  13: LOGICAL (Password) - A logical value indicating whether the textbox should mask the input as a password field.  .T. for password masking, .F. otherwise.
 *  14: LOGICAL (Right Align) - A logical value indicating whether the text should be right-aligned within the textbox.  .T. for right-aligned, .F. otherwise.
 *  15: LOGICAL (Read Only) - A logical value indicating whether the textbox should be read-only.  .T. for read-only, .F. otherwise.
 *  16: LOGICAL (Visible) - A logical value indicating whether the textbox should be initially visible.  .T. for visible, .F. for hidden.
 *  17: LOGICAL (Tab Stop) - A logical value indicating whether the textbox should be included in the tab order.  .T. for tab stop, .F. otherwise.
 *  18: LOGICAL (Client Edge) - A logical value indicating whether the textbox should have a client edge (sunken border). .T. for no client edge, .F. for client edge.
 *
 * Return Value:
 *   HWND - The handle of the newly created textbox control.
 *
 * Purpose:
 *   This function creates a standard textbox control with various style options, including numeric input, case conversion, password masking, and alignment.
 *   It's used to create general-purpose text input fields with customizable behavior.
 *   Example Usage: Creating a textbox for entering a username, password, or a numeric value.
 *
 * Notes:
 *   The maximum text length is controlled by the EM_LIMITTEXT message.
 *   The custom window procedure (OwnEditProc) is applied to handle specific events and behaviors.
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

/*
 * FUNCTION INITCHARMASKTEXTBOX
 *
 * Initializes a character-masked textbox control.
 *
 * Parameters:
 *   1: HWND (Parent Window Handle) - The handle of the parent window to which the textbox will be attached.
 *   2: HMENU (Control ID) - The ID of the textbox control.  This is used to identify the control within the parent window.
 *   3: INT (X Position) - The X coordinate of the top-left corner of the textbox, relative to the parent window.
 *   4: INT (Y Position) - The Y coordinate of the top-left corner of the textbox, relative to the parent window.
 *   5: INT (Width) - The width of the textbox control.
 *   9: LOGICAL (Uppercase) - A logical value indicating whether the input should be automatically converted to uppercase.  .T. for uppercase, .F. otherwise.
 *  10: LOGICAL (Lowercase) - A logical value indicating whether the input should be automatically converted to lowercase.  .T. for lowercase, .F. otherwise.
 *  12: LOGICAL (Right Align) - A logical value indicating whether the text should be right-aligned within the textbox.  .T. for right-aligned, .F. otherwise.
 *  13: LOGICAL (Read Only) - A logical value indicating whether the textbox should be read-only.  .T. for read-only, .F. otherwise.
 *  14: LOGICAL (Visible) - A logical value indicating whether the textbox should be initially visible.  .T. for visible, .F. for hidden.
 *  15: LOGICAL (Tab Stop) - A logical value indicating whether the textbox should be included in the tab order.  .T. for tab stop, .F. otherwise.
 *  11: INT (Height) - The height of the textbox control.
 *  16: LOGICAL (Client Edge) - A logical value indicating whether the textbox should have a client edge (sunken border). .T. for no client edge, .F. for client edge.
 *
 * Return Value:
 *   HWND - The handle of the newly created character-masked textbox control.
 *
 * Purpose:
 *   This function creates a character-masked textbox control, similar to INITMASKEDTEXTBOX, but potentially with a different masking implementation.
 *   It's used to create specialized text input fields with features like automatic case conversion, alignment, and read-only status.
 *   The "character-masked" aspect likely refers to a specific type of masking that restricts input to certain characters or patterns.
 *   Example Usage: Creating a textbox for entering a date in a specific format (e.g., MM/DD/YYYY).
 *
 * Notes:
 *   The "character-masked" aspect of this textbox is not directly implemented in this function.  It likely relies on the custom window procedure (OwnEditProc)
 *   and/or additional Harbour code to handle the masking logic.  The parameters here primarily control the basic style and appearance of the textbox.
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
         WC_EDIT,                                        // Class name for Edit control
         TEXT( "" ),                                     // No initial text
         Style,                                          // Combined styles
         hb_parni( 3 ),                                  // X position
         hb_parni( 4 ),                                  // Y position
         hb_parni( 5 ),                                  // Width
         hb_parni( 11 ),                                 // Height
         hmg_par_raw_HWND( 1 ),                          // Parent window handle
         hmg_par_raw_HMENU( 2 ),                         // Menu or control ID
         GetInstance(),                                  // Application instance handle
         NULL                                            // Additional parameters
      );

   // Store the original window procedure and apply custom subclass procedure
   SetProp( ( HWND ) hedit, TEXT( "oldeditproc" ), ( HWND ) GetWindowLongPtr( ( HWND ) hedit, GWLP_WNDPROC ) );
   SubclassWindow2( hedit, OwnEditProc );

   hmg_ret_raw_HWND( hedit );                            // Return the handle of the created control
}

/*
 * FUNCTION OwnEditProc( HWND hButton, UINT Msg, WPARAM wParam, LPARAM lParam )
 *
 * Custom window procedure for Edit controls to handle specific messages.
 *
 * Parameters:
 *   hButton: HWND - The handle of the Edit control.
 *   Msg: UINT - The Windows message being processed.
 *   wParam: WPARAM - Message-specific information.
 *   lParam: LPARAM - Message-specific information.
 *
 * Return Value:
 *   LRESULT - The result of the message processing.  This value is typically returned to Windows.
 *
 * Purpose:
 *   This function intercepts and processes specific Windows messages for Edit controls, allowing for custom behavior.
 *   It's used to extend the functionality of standard Edit controls by handling events like right-clicks (context menu) and character input.
 *   The primary goal is to provide a mechanism for Harbour code to respond to these events.
 *   Example Usage: Implementing custom validation logic when a user types into a textbox, or displaying a custom context menu when the user right-clicks.
 *
 * Notes:
 *   This function uses window subclassing to intercept messages.  The original window procedure is stored and called for default processing.
 *   The "OEDITEVENTS" symbol is dynamically loaded, suggesting that the actual event handling logic is implemented in Harbour code.
 *   The function pushes parameters onto the Harbour VM stack and calls a Harbour function to handle the event.
 */
LRESULT CALLBACK OwnEditProc( HWND hButton, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB   pSymbol = NULL;                     // Symbol for dynamically loaded Harbour procedure
   LRESULT           r;                                  // Result to be returned to the caller
   WNDPROC           OldWndProc;                         // Original window procedure

   // Retrieve the stored original window procedure
   OldWndProc = ( WNDPROC ) ( HB_PTRUINT ) GetProp( hButton, TEXT( "oldeditproc" ) );

   switch( Msg )
   {
      case WM_DESTROY:                                   // Clean up properties on destroy
         SubclassWindow2( hButton, OldWndProc );         // Restore original window procedure
         RemoveProp( hButton, TEXT( "oldeditproc" ) );   // Remove stored property
         break;

      case WM_CONTEXTMENU:                // Handle right-click context menu
      case WM_CHAR:                       // Handle character input
         if( !pSymbol )                   // Load symbol dynamically if not loaded
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OEDITEVENTS" ) );
         }

         if( pSymbol )                    // Invoke Harbour callback if symbol exists
         {
            hb_vmPushSymbol( pSymbol );   // Push symbol for the Harbour function
            hb_vmPushNil();               // Push Nil for the object instance
            hb_vmPushNumInt( ( HB_PTRUINT ) hButton );   // Push button handle
            hb_vmPushLong( Msg );         // Push message
            hb_vmPushNumInt( wParam );    // Push WPARAM
            hb_vmPushNumInt( lParam );    // Push LPARAM
            hb_vmDo( 4 );                 // Call the Harbour function
         }

         // Retrieve the result and decide whether to return it or call original procedure
         r = hmg_par_LRESULT( -1 );
         return( r != 0 ) ? r : CallWindowProc( OldWndProc, hButton, Msg, wParam, lParam );
   }

   return CallWindowProc( OldWndProc, hButton, Msg, wParam, lParam );   // Default processing
}
