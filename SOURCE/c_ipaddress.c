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
#define _WIN32_IE 0x0501                  // Define the minimum required Internet Explorer version to 5.01 for common controls.
#include <mgdefs.h>                       // Include MiniGUI definitions for Harbour.
#include <commctrl.h>                     // Include Windows Common Controls library for handling IP address controls.

// Prototype for the function that retrieves the application instance handle.
HINSTANCE   GetInstance( void );

/**
 * Harbour function to initialize an IP address control.
 * This function creates a Windows IP address control and configures its properties.
 */
HB_FUNC( INITIPADDRESS )
{
   DWORD                Style = WS_CHILD; // Basic window style for a child window (not visible by default).
   INITCOMMONCONTROLSEX icex;             // Structure to specify the initialization of common controls.
   icex.dwSize = sizeof( INITCOMMONCONTROLSEX );   // Set the structure size for INITCOMMONCONTROLSEX.
   icex.dwICC = ICC_INTERNET_CLASSES;              // Set the class of controls to initialize (internet-related classes).
   InitCommonControlsEx( &icex );                  // Initialize the IP address control.

   // If the 7th parameter is false, add WS_VISIBLE to the style to make the control visible.
   if( !hb_parl( 7 ) )
   {
      Style |= WS_VISIBLE;
   }

   // If the 8th parameter is false, add WS_TABSTOP to the style to make it a tab-stop control.
   if( !hb_parl( 8 ) )
   {
      Style |= WS_TABSTOP;
   }

   // Create the IP address control window with the specified extended style, control style, position, size, and parent.
   hmg_ret_raw_HWND
   (
      CreateWindowEx
         (
            WS_EX_CLIENTEDGE,       // Extended window style: creates a sunken edge around the IP control.
            WC_IPADDRESS,           // Class name for IP address controls.
            TEXT( "" ),             // Window text (empty for IP address controls).
            Style,                  // Combined style settings for the IP control.
            hb_parni( 3 ),          // X position of the control.
            hb_parni( 4 ),          // Y position of the control.
            hb_parni( 5 ),          // Width of the control.
            hb_parni( 6 ),          // Height of the control.
            hmg_par_raw_HWND( 1 ),  // Parent window handle from the first parameter.
            hmg_par_raw_HMENU( 2 ), // Control identifier (menu handle) from the second parameter.
            GetInstance(),          // Application instance handle.
            NULL                    // No additional creation data.
         )
   );
}

/**
 * Harbour function to set an IP address in the IP address control.
 * Parameters:
 * - Parameter 1: Handle to the IP address control.
 * - Parameters 2-5: IP address components (bytes).
 */
HB_FUNC( SETIPADDRESS )
{
   // Send an IPM_SETADDRESS message to set the IP address in the control.
   SendMessage( hmg_par_raw_HWND( 1 ), IPM_SETADDRESS, 0, MAKEIPADDRESS( hmg_par_BYTE( 2 ), hmg_par_BYTE( 3 ), hmg_par_BYTE( 4 ), hmg_par_BYTE( 5 ) ) );
}

/**
 * Harbour function to retrieve the IP address from the IP address control.
 * The function returns the IP address as a 4-element array.
 * Parameter: Handle to the IP address control.
 */
HB_FUNC( GETIPADDRESS )
{
   DWORD pdwAddr;                   // Variable to store the IP address.

   // Send an IPM_GETADDRESS message to retrieve the IP address from the control.
   SendMessage( hmg_par_raw_HWND( 1 ), IPM_GETADDRESS, 0, ( LPARAM ) ( LPDWORD ) & pdwAddr );

   hb_reta( 4 );                    // Return an array of 4 integers to hold each byte of the IP address.
   HB_STORNI( ( INT ) FIRST_IPADDRESS( pdwAddr ), -1, 1 );  // Store the first byte of the IP address.
   HB_STORNI( ( INT ) SECOND_IPADDRESS( pdwAddr ), -1, 2 ); // Store the second byte of the IP address.
   HB_STORNI( ( INT ) THIRD_IPADDRESS( pdwAddr ), -1, 3 );  // Store the third byte of the IP address.
   HB_STORNI( ( INT ) FOURTH_IPADDRESS( pdwAddr ), -1, 4 ); // Store the fourth byte of the IP address.
}
