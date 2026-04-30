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
    Copyright 1999-2026, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>
 ---------------------------------------------------------------------------*/

/*
   File:           c_cursor.c
   Description:    Low-level C routines for mouse cursor management in HMG Extended.
   Functionality:  Provides wrappers for Windows API cursor functions, handling 
                   resource loading, file-based cursors, and Unicode compatibility.
 */

#include <mgdefs.h>

#ifdef UNICODE
/* 
   Function: AnsiToWide
   Purpose: Converts a standard ANSI string to a Wide (Unicode) string.
   Parameters: LPCSTR - The source ANSI string.
   Returns: LPWSTR - The allocated Unicode string.
   Note: Essential for HMG Unicode builds to interface with Windows 'W' APIs.
*/
LPWSTR      AnsiToWide( LPCSTR );
#endif

/* 
   Functions: GetInstance / GetResources
   Purpose: Retrieves the application or resource instance handles.
   Returns: HINSTANCE - Handle to the module containing the executable or resources.
*/
HINSTANCE   GetInstance( void );
HINSTANCE   GetResources( void );

/*-------------------------------------------------------------------------*/
/* Internal Helper Functions                                               */
/*-------------------------------------------------------------------------*/

#ifdef UNICODE
/*
   Function: GetCursorNameWide
   Purpose: Resolves a Harbour parameter into a Unicode cursor identifier.
   Logic: If the parameter is a string, it converts it to WideChar. 
          If it's a number, it treats it as a Resource ID (MAKEINTRESOURCE).
   Returns: LPCWSTR - Pointer to the resource name or ID.
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
/*
   Function: GetCursorNameAnsi
   Purpose: Resolves a Harbour parameter into an ANSI cursor identifier.
   Returns: LPCSTR - Pointer to the resource name or ID.
*/
static LPCSTR GetCursorNameAnsi( int paramIndex )
{
   return( hb_parinfo( paramIndex ) & HB_IT_STRING ) ? hb_parc( paramIndex ) : MAKEINTRESOURCE( hb_parni( paramIndex ) );
}
#endif

/*-------------------------------------------------------------------------*/
/* Harbour API Wrappers                                                    */
/*-------------------------------------------------------------------------*/

/*
   Function: LOADCURSOR( [hInst], cnCursorName )
   Purpose: Loads a cursor resource from an executable or DLL.
   Parameters:
      1: hInst (Optional) - Handle to the module. If NIL, system cursors are used.
      2: cnCursorName - String (name) or Integer (ID) of the cursor.
   Returns: HCURSOR handle.
   Side Effects: Allocates memory for string conversion in Unicode mode.
*/
HB_FUNC( LOADCURSOR )
{
   // Determine if we are loading a system cursor (NULL) or a specific instance resource
   HINSTANCE   hInstance = HB_ISNIL( 1 ) ? NULL : hmg_par_raw_HINSTANCE( 1 );

#ifndef UNICODE
   LPCSTR      lpCursorName = GetCursorNameAnsi( 2 );
   hmg_ret_raw_HANDLE( LoadCursor( hInstance, lpCursorName ) );
#else
   LPCWSTR  lpCursorName = GetCursorNameWide( 2 );
   hmg_ret_raw_HANDLE( LoadCursor( hInstance, lpCursorName ) );
   
   // Clean up temporary wide string if one was allocated by AnsiToWide
   if( HB_ISCHAR( 2 ) && lpCursorName )
   {
      hb_xfree( ( void * ) lpCursorName );
   }
#endif
}

/*
   Function: LOADCURSORFROMFILE( cFileName )
   Purpose: Creates a cursor based on data contained in a file (.cur or .ani).
   Parameters: cFileName - Path to the cursor file.
   Returns: HCURSOR handle.
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

/*
   Function: SETRESCURSOR( hCursor )
   Purpose: Sets the cursor shape for the current thread using a handle.
   Parameters: hCursor - Handle to the cursor.
   Returns: Handle to the previous cursor.
*/
HB_FUNC( SETRESCURSOR )
{
   hmg_ret_raw_HANDLE( SetCursor( hmg_par_raw_HCURSOR( 1 ) ) );
}

/*
   Function: FILECURSOR( cFileName )
   Purpose: Loads a cursor from a file and immediately applies it.
   Parameters: cFileName - Path to the cursor file.
   Returns: Handle to the newly set cursor.
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

/*
   Function: SETWINDOWCURSOR( hWnd, cnCursor )
   Purpose: Changes the default cursor for a specific window class.
   Parameters:
      1: hWnd - Handle to the window.
      2: cnCursor - Resource name (String), Resource ID (Integer), or File Path (String).
   Logic: 
      1. Attempts to load from application resources.
      2. If loading fails and input is a string, attempts to load as a file.
      3. Updates the window class using SetClassLongPtr.
*/
HB_FUNC( SETWINDOWCURSOR )
{
   HCURSOR  ch;
   BOOL     bIsString = HB_ISCHAR( 2 );

#ifdef UNICODE
   LPCWSTR  lpCursorName = GetCursorNameWide( 2 );
#else
   LPCSTR   lpCursorName = GetCursorNameAnsi( 2 );
#endif

   // Attempt to load from resources first (internal or system)
   ch = LoadCursor( bIsString ? GetResources() : NULL, lpCursorName );

   // If resource load fails and it's a string, try loading as an external file
   if( ch == NULL && bIsString )
   {
#ifdef UNICODE
      LPCWSTR  lpFile = AnsiToWide( hb_parc( 2 ) );
      ch = LoadCursorFromFile( lpFile );
      if( lpFile )
      {
         hb_xfree( ( void * ) lpFile );
      }
#else
      ch = LoadCursorFromFile( hb_parc( 2 ) );
#endif
   }

   // If a valid cursor was obtained, update the window class attribute
   if( ch != NULL )
   {
      SetClassLongPtr( hmg_par_raw_HWND( 1 ), GCLP_HCURSOR, ( LONG_PTR ) ch );
   }

#ifdef UNICODE
   if( bIsString && lpCursorName )
   {
      hb_xfree( ( void * ) lpCursorName );
   }
#endif
}

/*
   Function: LoadHandCursor
   Purpose: Internal helper to provide the 'Hand' cursor across different Windows versions.
   Logic: Windows 2000 (0x0500) and later have IDC_HAND built-in. 
          For older systems, HMG uses a custom resource "MINIGUI_FINGER".
   Returns: HCURSOR handle.
*/
static HCURSOR LoadHandCursor( void )
{
#if ( WINVER >= 0x0500 )
   return LoadCursor( NULL, IDC_HAND );
#else
   return LoadCursor( GetInstance(), TEXT( "MINIGUI_FINGER" ) );
#endif
}

/*
   Function: CURSORHAND()
   Purpose: Sets the current mouse pointer to the Hand shape.
   Returns: Handle to the previous cursor.
*/
HB_FUNC( CURSORHAND )
{
   HCURSOR  hCursor = LoadHandCursor();
   
   // Fallback to standard arrow if hand cursor cannot be loaded
   if( !hCursor )
   {
      hCursor = LoadCursor( NULL, IDC_ARROW );
   }

   hmg_ret_raw_HANDLE( SetCursor( hCursor ) );
}

/*
   Function: SETHANDCURSOR( hWnd )
   Purpose: Assigns the Hand cursor to a specific window's class.
   Parameters: hWnd - Handle to the target window.
   Returns: Logical - True if successful.
*/
HB_FUNC( SETHANDCURSOR )
{
   HCURSOR  hCursor = LoadHandCursor();
   if( hCursor )
   {
      HWND  hWnd = hmg_par_raw_HWND( 1 );
      // Update the class cursor so the hand appears whenever the mouse enters this window
      hmg_ret_L( SetClassLongPtr( hWnd, GCLP_HCURSOR, ( LONG_PTR ) hCursor ) != 0 );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}