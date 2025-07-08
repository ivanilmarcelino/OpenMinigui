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
   File:           c_cursor.c
   Contributors:   Jacek Kubica <kubica@wssk.wroc.pl>
                   Grigory Filatov <gfilatov@gmail.com>
   Description:    Handles mouse cursor shapes for MiniGUI.
   Status:         Public Domain
 */

#include <mgdefs.h>                 // Include required definitions and macros for the program

#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );   // Function declaration for converting ANSI to wide (Unicode) strings
#endif
HINSTANCE   GetInstance( void );    // Function declaration to get the instance handle of the application
HINSTANCE   GetResources( void );   // Function declaration to get the resources handle of the application

// Helper function to get the cursor name as LPCSTR or LPCWSTR, depending on Unicode setting
#ifdef UNICODE
static LPCWSTR GetCursorNameWide( int paramIndex )
{
   // Convert ANSI string to wide if parameter is a string; otherwise, use as resource ID
   if ( hb_parinfo( paramIndex ) & HB_IT_STRING )
   {
      return AnsiToWide( ( char * ) hb_parc( paramIndex ) );
   }
   else
      return MAKEINTRESOURCE( hb_parni( paramIndex ) );
}

#else
static LPCSTR GetCursorNameAnsi( int paramIndex )
{
   // Returns cursor name as ANSI string or as resource ID if the parameter is an integer
   return( hb_parinfo( paramIndex ) & HB_IT_STRING ) ? hb_parc( paramIndex ) : MAKEINTRESOURCE( hb_parni( paramIndex ) );
}
#endif

// Function to load a cursor by name or resource identifier
HB_FUNC( LOADCURSOR )
{
   // Use NULL if no instance is specified; otherwise, get the instance handle
   HINSTANCE   hInstance = HB_ISNIL( 1 ) ? NULL : hmg_par_raw_HINSTANCE( 1 );

#ifndef UNICODE
   LPCSTR      lpCursorName = GetCursorNameAnsi( 2 );             // Get ANSI cursor name
   hmg_ret_raw_HANDLE( LoadCursor( hInstance, lpCursorName ) );   // Load and return cursor handle
#else
   LPCWSTR  lpCursorName = GetCursorNameWide( 2 );                // Get Unicode cursor name
   hmg_ret_raw_HANDLE( LoadCursor( hInstance, lpCursorName ) );
   if ( HB_ISCHAR( 2 ) ) // Free memory only if it was allocated dynamically
   {
      hb_xfree( ( void * ) lpCursorName );
   }
#endif
}

// Function to load a cursor from a file path
HB_FUNC( LOADCURSORFROMFILE )
{
#ifdef UNICODE
   LPCWSTR  lpFileName = AnsiToWide( ( char * ) hb_parc( 1 ) );   // Convert ANSI path to wide string
   hmg_ret_raw_HANDLE( LoadCursorFromFile( lpFileName ) );        // Load cursor from file and return handle
   hb_xfree( ( TCHAR * ) lpFileName ); // Free the wide string after use
#else
   hmg_ret_raw_HANDLE( LoadCursorFromFile( ( LPCSTR ) hb_parc( 1 ) ) ); // Load cursor from ANSI path
#endif
}

// Function to set a specified cursor as the active cursor
HB_FUNC( SETRESCURSOR )
{
   hmg_ret_raw_HANDLE( SetCursor( hmg_par_raw_HCURSOR( 1 ) ) );         // Set and return the specified cursor handle
}

// Load a cursor from file and set it as the active cursor
HB_FUNC( FILECURSOR )
{
#ifdef UNICODE
   LPCWSTR  lpFileName = AnsiToWide( ( char * ) hb_parc( 1 ) );         // Convert file path to Unicode if needed
   hmg_ret_raw_HANDLE( SetCursor( LoadCursorFromFile( lpFileName ) ) ); // Load, set, and return cursor
   hb_xfree( ( TCHAR * ) lpFileName ); // Free the wide string
#else
   hmg_ret_raw_HANDLE( SetCursor( LoadCursorFromFile( ( LPCSTR ) hb_parc( 1 ) ) ) );  // Load and set cursor from file
#endif
}

// Function to set a specific window’s cursor
HB_FUNC( SETWINDOWCURSOR )
{
   HCURSOR  ch;

#ifdef UNICODE
   LPCWSTR  lpCursorName = GetCursorNameWide( 2 );                // Get Unicode cursor name
#else
   LPCSTR   lpCursorName = GetCursorNameAnsi( 2 );                // Get ANSI cursor name
#endif

   // Load cursor either from resources or file
   ch = LoadCursor( HB_ISCHAR( 2 ) ? GetResources() : NULL, lpCursorName );

   // If cursor not found in resources, try loading from file
   if( ch == NULL && HB_ISCHAR( 2 ) )
   {
      ch = LoadCursorFromFile( lpCursorName );
   }

   // Set window class cursor if cursor is loaded successfully
   if( ch != NULL )
   {
      SetClassLongPtr( hmg_par_raw_HWND( 1 ), GCLP_HCURSOR, ( LONG_PTR ) ch );
   }

#ifdef UNICODE
   if ( HB_ISCHAR( 2 ) ) // Free memory only if it was allocated dynamically
      hb_xfree( ( void * ) lpCursorName );
#endif
}

// Function to load the hand cursor, falling back to arrow if not available
static HCURSOR LoadHandCursor( void )
{
#if ( WINVER >= 0x0500 )
   return LoadCursor( NULL, IDC_HAND );                           // Load system hand cursor if available
#else
   return LoadCursor( GetInstance(), TEXT( "MINIGUI_FINGER" ) );  // Custom cursor as fallback
#endif
}

// Function to set the hand cursor as active and return previous cursor
HB_FUNC( CURSORHAND )
{
   HCURSOR  hCursor = LoadHandCursor();         // Load the hand cursor
   if( !hCursor )
   {
      hCursor = LoadCursor( NULL, IDC_ARROW );  // Fallback to arrow cursor if hand cursor unavailable
   }

   hmg_ret_raw_HANDLE( SetCursor( hCursor ) );  // Set and return the active cursor handle
}

// Function to set a specific window’s cursor to the hand cursor
HB_FUNC( SETHANDCURSOR )
{
   HCURSOR  hCursor = LoadHandCursor();         // Load the hand cursor
   if( hCursor )
   {
      HWND  hWnd = hmg_par_raw_HWND( 1 );       // Get the handle of the specified window

      // Set window class cursor and return success status
      hmg_ret_L( SetClassLongPtr( hWnd, GCLP_HCURSOR, ( LONG_PTR ) hCursor ) != 0 );
   }
   else
   {
      hb_retl( HB_FALSE ); // Return false if cursor was not set
   }
}
