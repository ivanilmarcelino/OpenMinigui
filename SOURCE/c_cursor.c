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

#include <mgdefs.h>

#ifdef UNICODE

/**
 * Function: AnsiToWide
 * --------------------
 * Converts a null-terminated ANSI string (LPCSTR) to a wide (Unicode) string (LPWSTR).
 *
 * Parameters:
 *   str  - ANSI string to convert.
 *
 * Returns:
 *   Newly allocated wide-character string. Caller is responsible for freeing it.
 *
 * Notes:
 *   Used in Unicode builds to bridge between ANSI strings from Harbour and WinAPI.
 */
LPWSTR      AnsiToWide( LPCSTR );
#endif

/**
 * Function: GetInstance
 * ---------------------
 * Retrieves the current application instance handle.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   The HINSTANCE of the running application.
 */
HINSTANCE   GetInstance( void );

/**
 * Function: GetResources
 * ----------------------
 * Retrieves the current application resource handle (useful for custom cursors/icons).
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   The HINSTANCE containing resources.
 */
HINSTANCE   GetResources( void );

/*-------------------------------------------------------------------------*/
/* Helper Functions for Cursor Resource Naming and Retrieval              */
/*-------------------------------------------------------------------------*/

#ifdef UNICODE

/**
 * Function: GetCursorNameWide
 * ---------------------------
 * Determines the correct wide-character cursor name or resource ID based on input type.
 *
 * Parameters:
 *   paramIndex - Index of parameter passed from Harbour (expected as string or integer).
 *
 * Returns:
 *   If parameter is string: pointer to wide string (allocated via AnsiToWide).
 *   If parameter is integer: resource identifier cast as wide string.
 *
 * Notes:
 *   Memory allocation must be freed by caller if a string was passed.
 */
static LPCWSTR GetCursorNameWide( int paramIndex )
{
   if( hb_parinfo( paramIndex ) & HB_IT_STRING )
   {
      return AnsiToWide( hb_parc( paramIndex ) );
   }
   else
   {
      return MAKEINTRESOURCE( hb_parni( paramIndex ) );
   }
}

#else

/**
 * Function: GetCursorNameAnsi
 * ---------------------------
 * Determines the correct ANSI cursor name or resource ID based on input type.
 *
 * Parameters:
 *   paramIndex - Index of parameter passed from Harbour (string or integer).
 *
 * Returns:
 *   ANSI string or resource ID cast as ANSI pointer.
 */
static LPCSTR GetCursorNameAnsi( int paramIndex )
{
   return( hb_parinfo( paramIndex ) & HB_IT_STRING ) ? hb_parc( paramIndex ) : MAKEINTRESOURCE( hb_parni( paramIndex ) );
}
#endif

/*-------------------------------------------------------------------------*/
/* Main Cursor Functions                                                   */
/*-------------------------------------------------------------------------*/

/**
 * Function: LOADCURSOR
 * --------------------
 * Loads a cursor from resources or name and returns a handle to Harbour caller.
 *
 * Parameters:
 *   1: HINSTANCE (optional) - Instance handle for resource lookup or NULL for default.
 *   2: STRING or INTEGER  - Cursor name or resource ID.
 *
 * Returns:
 *   Handle to the loaded cursor (HCURSOR) or NULL if loading fails.
 *
 * Notes:
 *   Handles memory freeing in Unicode builds when string names are used.
 */
HB_FUNC( LOADCURSOR )
{
   HINSTANCE   hInstance = HB_ISNIL( 1 ) ? NULL : hmg_par_raw_HINSTANCE( 1 );

#ifndef UNICODE
   LPCSTR      lpCursorName = GetCursorNameAnsi( 2 );
   hmg_ret_raw_HANDLE( LoadCursor( hInstance, lpCursorName ) );
#else
   LPCWSTR  lpCursorName = GetCursorNameWide( 2 );
   hmg_ret_raw_HANDLE( LoadCursor( hInstance, lpCursorName ) );
   if( HB_ISCHAR( 2 ) && lpCursorName )
   {
      hb_xfree( ( void * ) lpCursorName );   /* Free allocated wide string */
   }
#endif
}

/* Function: LOADCURSORFROMFILE
 * ----------------------------
 * Loads a cursor from a file path specified by the user and returns its handle.
 *
 * Parameters:
 *   1: STRING - File path to cursor file.
 *
 * Returns:
 *   Handle to the loaded cursor (HCURSOR) or NULL on failure.
 *
 * Notes:
 *   Converts ANSI to Unicode string in Unicode builds and frees memory afterward.
 */
HB_FUNC( LOADCURSORFROMFILE )
{
#ifdef UNICODE
   LPCWSTR  lpFileName = AnsiToWide( hb_parc( 1 ) );
   hmg_ret_raw_HANDLE( LoadCursorFromFile( lpFileName ) );
   if( lpFileName )
   {
      hb_xfree( ( TCHAR * ) lpFileName );
   }
#else
   hmg_ret_raw_HANDLE( LoadCursorFromFile( ( LPCSTR ) hb_parc( 1 ) ) );
#endif
}

/**
 * Function: SETRESCURSOR
 * ----------------------
 * Sets the active cursor using a provided cursor handle.
 *
 * Parameters:
 *   1: HCURSOR - Handle of the cursor to activate.
 *
 * Returns:
 *   The handle to the previously active cursor.
 */
HB_FUNC( SETRESCURSOR )
{
   hmg_ret_raw_HANDLE( SetCursor( hmg_par_raw_HCURSOR( 1 ) ) );
}

/**
 * Function: FILECURSOR
 * --------------------
 * Loads a cursor from file and sets it as the active cursor.
 *
 * Parameters:
 *   1: STRING - Path to the cursor file.
 *
 * Returns:
 *   Handle to the cursor that was set.
 */
HB_FUNC( FILECURSOR )
{
#ifdef UNICODE
   LPCWSTR  lpFileName = AnsiToWide( hb_parc( 1 ) );
   hmg_ret_raw_HANDLE( SetCursor( LoadCursorFromFile( lpFileName ) ) );
   if( lpFileName )
   {
      hb_xfree( ( TCHAR * ) lpFileName );
   }
#else
   hmg_ret_raw_HANDLE( SetCursor( LoadCursorFromFile( ( LPCSTR ) hb_parc( 1 ) ) ) );
#endif
}

/**
 * Function: SETWINDOWCURSOR
 * -------------------------
 * Sets a specified window's cursor based on name or resource ID.
 *
 * Parameters:
 *   1: HWND   - Window handle whose class cursor should be changed.
 *   2: STRING or INTEGER - Cursor name or resource ID.
 *
 * Returns:
 *   (void) — updates window class cursor if successful.
 *
 * Notes:
 *   Attempts resource-based loading first, then file-based fallback.
 *   Frees Unicode string memory when necessary.
 */
HB_FUNC( SETWINDOWCURSOR )
{
   HCURSOR  ch;
#ifdef UNICODE
   LPCWSTR  lpCursorName = GetCursorNameWide( 2 );
#else
   LPCSTR   lpCursorName = GetCursorNameAnsi( 2 );
#endif

   /* Try loading from resources */
   ch = LoadCursor( HB_ISCHAR( 2 ) ? GetResources() : NULL, lpCursorName );

   /* Fallback: load from file if not found and parameter is string */
   if( ch == NULL && HB_ISCHAR( 2 ) )
   {
      ch = LoadCursorFromFile( lpCursorName );
   }

   /* If a cursor was successfully loaded, assign it to the window class */
   if( ch != NULL )
   {
      SetClassLongPtr( hmg_par_raw_HWND( 1 ), GCLP_HCURSOR, ( LONG_PTR ) ch );
   }

#ifdef UNICODE
   if( HB_ISCHAR( 2 ) )
   {
      hb_xfree( ( void * ) lpCursorName );
   }
#endif
}

/**
 * Function: LoadHandCursor
 * ------------------------
 * Internal helper: obtains the "hand"-style cursor or a fallback if unavailable.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   HCURSOR handle for the hand or fallback cursor.
 *
 * Notes:
 *   Uses IDC_HAND for Windows equal or more than 0x0500, otherwise uses a custom Minigui resource.
 */
static HCURSOR LoadHandCursor( void )
{
#if ( WINVER >= 0x0500 )
   return LoadCursor( NULL, IDC_HAND );
#else
   return LoadCursor( GetInstance(), TEXT( "MINIGUI_FINGER" ) );
#endif
}

/**
 * Function: CURSORHAND
 * --------------------
 * Sets the cursor to the hand cursor, returning the previous cursor handle.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   Handle to the previously active cursor.
 */
HB_FUNC( CURSORHAND )
{
   HCURSOR  hCursor = LoadHandCursor();
   if( !hCursor )
   {
      hCursor = LoadCursor( NULL, IDC_ARROW );
   }

   hmg_ret_raw_HANDLE( SetCursor( hCursor ) );
}

/**
 * Function: SETHANDCURSOR
 * -----------------------
 * Sets a specific window's cursor to the hand style.
 *
 * Parameters:
 *   1: HWND - Window handle to update.
 *
 * Returns:
 *   Logical flag (true if successful, false otherwise).
 */
HB_FUNC( SETHANDCURSOR )
{
   HCURSOR  hCursor = LoadHandCursor();
   if( hCursor )
   {
      HWND  hWnd = hmg_par_raw_HWND( 1 );
      hmg_ret_L( SetClassLongPtr( hWnd, GCLP_HCURSOR, ( LONG_PTR ) hCursor ) != 0 );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}
