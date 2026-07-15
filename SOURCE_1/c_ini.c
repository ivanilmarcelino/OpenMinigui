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

#include <mgdefs.h>

/* 
 * String Conversion Utilities
 * These prototypes handle the transformation between ANSI (8-bit) and 
 * Wide (16-bit Unicode) characters, essential for HMG Extended's 
 * compatibility with different Windows character sets.
 */
#ifdef UNICODE
LPWSTR   AnsiToWide( LPCSTR );
LPSTR    WideToAnsi( LPWSTR );
#endif

/* 
 * Macro Definitions for Harbour/C String Interoperability
 * HB_PARSTR: Safely retrieves a string parameter from Harbour, converting to Wide if necessary.
 * HB_RETSTR: Returns a string to Harbour, handling Unicode-to-ANSI conversion and memory cleanup.
 */
#ifdef UNICODE
#define HB_PARSTR( n )  AnsiToWide( hb_parc( n ) )
#define HB_RETSTR( s ) \
   do \
   { \
      LPSTR p = WideToAnsi( s ); \
      hb_retc( p ); \
      hb_xfree( p ); \
   } \
   while( 0 )
#else
#define HB_PARSTR( n )  ( TCHAR * ) hb_parc( n )
#define HB_RETSTR( s )  hb_retc( s )
#endif

/*
 * PROCEDURE hb_freeW( p )
 * Purpose: Internal helper to release memory allocated for Wide strings.
 * Logic: Only performs an action in UNICODE mode to prevent memory leaks 
 *        from temporary buffers created during parameter parsing.
 */
   static void hb_freeW( void *p )
{
#ifdef UNICODE
   if( p )
   {
      hb_xfree( p );
   }
#else
   HB_SYMBOL_UNUSED( p );
#endif
}

/*
 * HB_FUNC( GETPRIVATEPROFILESTRING )
 * Purpose: Retrieves a string from a specified section in an INI file.
 * Parameters:
 *    1. cSection  (String): The section containing the key.
 *    2. cEntry    (String): The key name whose value is to be retrieved.
 *    3. cDefault  (String): The default value if the key is not found.
 *    4. cFileName (String): The path to the INI file.
 * Returns: The string value associated with the key, or the default value.
 * Side Effects: Allocates temporary memory for buffer management.
 */
HB_FUNC( GETPRIVATEPROFILESTRING )
{
   DWORD nSize = 256, dwLen;
   TCHAR *buffer = NULL;
   BOOL  ok = FALSE;

   // Extract parameters from Harbour stack
   TCHAR *section = HB_ISCHAR( 1 ) ? HB_PARSTR( 1 ) : NULL;
   TCHAR *entry = HB_ISCHAR( 2 ) ? HB_PARSTR( 2 ) : NULL;
   TCHAR *def = HB_PARSTR( 3 );
   TCHAR *filename = HB_PARSTR( 4 );

   /* 
    * Dynamic Buffer Allocation Logic:
    * We don't know the length of the INI value beforehand. We start with 256 bytes
    * and double the size if GetPrivateProfileString indicates the buffer was too small
    * (return value equals nSize - 1).
    */
   do
   {
      if( buffer )
      {
         hb_xfree( buffer );
      }

      nSize *= 2;
      buffer = ( TCHAR * ) hb_xgrab( nSize * sizeof( TCHAR ) );

      dwLen = GetPrivateProfileString( section, entry, def, buffer, nSize, filename );

      // Check for API failure
      if( dwLen == 0 && GetLastError() != ERROR_SUCCESS )
      {
         break;
      }

      ok = TRUE;
   }
   while( dwLen >= nSize - 1 );

   // Return the result to Harbour
   if( ok )
   {
      if( dwLen > 0 )
      {
         HB_RETSTR( buffer );
      }
      else
      {
         HB_RETSTR( def );
      }
   }
   else
   {
      hb_retc( "" );
   }

   // Cleanup allocated resources
   if( buffer )
   {
      hb_xfree( buffer );
   }

   hb_freeW( section );
   hb_freeW( entry );
   hb_freeW( def );
   hb_freeW( filename );
}

/*
 * HB_FUNC( WRITEPRIVATEPROFILESTRING )
 * Purpose: Copies a string into the specified section of an INI file.
 * Parameters:
 *    1. cSection  (String): The section name.
 *    2. cEntry    (String): The key name.
 *    3. cData     (String): The string to be written.
 *    4. cFileName (String): The path to the INI file.
 * Returns: Logical (.T. if successful, .F. otherwise).
 */
HB_FUNC( WRITEPRIVATEPROFILESTRING )
{
   TCHAR *section = HB_PARSTR( 1 );
   TCHAR *entry = HB_ISCHAR( 2 ) ? HB_PARSTR( 2 ) : NULL;
   TCHAR *data = HB_ISCHAR( 3 ) ? HB_PARSTR( 3 ) : NULL;
   TCHAR *filename = HB_PARSTR( 4 );

   // Call Win32 API and return success status
   hb_retl( WritePrivateProfileString( section, entry, data, filename ) );

   hb_freeW( section );
   hb_freeW( entry );
   hb_freeW( data );
   hb_freeW( filename );
}

/*
 * HB_FUNC( DELINIENTRY )
 * Purpose: Deletes a specific key (entry) from an INI file.
 * Parameters:
 *    1. cSection  (String): The section containing the key.
 *    2. cEntry    (String): The key to delete.
 *    3. cFileName (String): The path to the INI file.
 * Returns: Logical (.T. if successful).
 * Logic: Per Win32 API documentation, passing NULL as the data parameter 
 *        to WritePrivateProfileString deletes the key.
 */
HB_FUNC( DELINIENTRY )
{
   TCHAR *section = HB_PARSTR( 1 );
   TCHAR *entry = HB_PARSTR( 2 );
   TCHAR *filename = HB_PARSTR( 3 );

   hb_retl( WritePrivateProfileString( section, entry, NULL, filename ) );

   hb_freeW( section );
   hb_freeW( entry );
   hb_freeW( filename );
}

/*
 * HB_FUNC( DELINISECTION )
 * Purpose: Deletes an entire section from an INI file.
 * Parameters:
 *    1. cSection  (String): The section to delete.
 *    2. cFileName (String): The path to the INI file.
 * Returns: Logical (.T. if successful).
 * Logic: Passing NULL as the entry parameter to WritePrivateProfileString 
 *        deletes the entire section.
 */
HB_FUNC( DELINISECTION )
{
   TCHAR *section = HB_PARSTR( 1 );
   TCHAR *filename = HB_PARSTR( 2 );

   hb_retl( WritePrivateProfileString( section, NULL, TEXT( "" ), filename ) );

   hb_freeW( section );
   hb_freeW( filename );
}

/* 
 * Internal Helper: FindFirstSubString
 * Purpose: Returns the start of the first string in a null-separated buffer.
 */
static TCHAR *FindFirstSubString( TCHAR *s )
{
   return( *s == 0 ) ? NULL : s;
}

/* 
 * Internal Helper: FindNextSubString
 * Purpose: Navigates to the next string in a null-separated buffer.
 * Logic: Moves the pointer past the current null terminator. If the next 
 *        character is also null, we have reached the end of the list.
 */
static TCHAR *FindNextSubString( TCHAR *s )
{
   TCHAR *p = s + lstrlen( s ) + 1;
   return( *p == 0 ) ? NULL : p;
}

/* 
 * Internal Helper: CountSubStrings
 * Purpose: Counts how many null-terminated strings exist in a double-null terminated block.
 */
static INT CountSubStrings( TCHAR *s )
{
   INT   count = 0;
   TCHAR *p = FindFirstSubString( s );

   while( p )
   {
      count++;
      p = FindNextSubString( p );
   }

   return count;
}

/*
 * HB_FUNC( _GETPRIVATEPROFILESECTIONNAMES )
 * Purpose: Retrieves all section names in an INI file.
 * Parameters:
 *    1. cFileName (String): The path to the INI file.
 * Returns: An Array of strings containing all section names.
 * Logic: Uses a large buffer to capture the null-separated list of names 
 *        returned by the Windows API, then parses them into a Harbour array.
 */
HB_FUNC( _GETPRIVATEPROFILESECTIONNAMES )
{
   DWORD nSize = 32767; // Initial large buffer size for section names
   TCHAR *buffer = NULL;
   TCHAR *p;
   INT   i, count;

   TCHAR *filename = HB_PARSTR( 1 );

   do
   {
      if( buffer )
      {
         hb_xfree( buffer );
      }

      buffer = ( TCHAR * ) hb_xgrab( nSize * sizeof( TCHAR ) );
      ZeroMemory( buffer, nSize * sizeof( TCHAR ) );

      // If the buffer is too small, the API returns nSize - 2.
      if( GetPrivateProfileSectionNames( buffer, nSize, filename ) != nSize - 2 )
      {
         break;
      }

      nSize *= 2;
   }
   while( TRUE );

   // Determine array size and initialize Harbour array
   count = CountSubStrings( buffer );
   hb_reta( count );

   p = FindFirstSubString( buffer );

   // Iterate through the buffer and store each section name in the array
   for( i = 1; p; ++i, p = FindNextSubString( p ) )
   {
#ifdef UNICODE
      LPSTR tmp = WideToAnsi( p );
      HB_STORC( tmp, -1, i );
      hb_xfree( tmp );
#else
      HB_STORC( p, -1, i );
#endif
   }

   if( buffer )
   {
      hb_xfree( buffer );
   }

   hb_freeW( filename );
}

/*
 * HB_FUNC( _GETPRIVATEPROFILESECTION )
 * Purpose: Retrieves all keys and values for a specific section.
 * Parameters:
 *    1. cSection  (String): The section name.
 *    2. cFileName (String): The path to the INI file.
 * Returns: An Array of strings, where each element is "Key=Value".
 */
HB_FUNC( _GETPRIVATEPROFILESECTION )
{
   TCHAR buffer[32767]; // Fixed buffer size for section content
   TCHAR *p;
   INT   i, count;

   TCHAR *section = HB_PARSTR( 1 );
   TCHAR *filename = HB_PARSTR( 2 );

   ZeroMemory( buffer, sizeof( buffer ) );

   GetPrivateProfileSection( section, buffer, sizeof( buffer ) / sizeof( TCHAR ), filename );

   count = CountSubStrings( buffer );
   hb_reta( count );

   p = FindFirstSubString( buffer );

   // Populate the Harbour array with "Key=Value" strings
   for( i = 1; p; ++i, p = FindNextSubString( p ) )
   {
#ifdef UNICODE
      LPSTR tmp = WideToAnsi( p );
      HB_STORC( tmp, -1, i );
      hb_xfree( tmp );
#else
      HB_STORC( p, -1, i );
#endif
   }

   hb_freeW( section );
   hb_freeW( filename );
}

/*
 * HB_FUNC( ISINIKEYEXISTS )
 * Purpose: Checks if a specific key exists within a section.
 * Parameters:
 *    1. cSection  (String): The section name.
 *    2. cKey      (String): The key name.
 *    3. cFileName (String): The path to the INI file.
 * Returns: Logical (.T. if the key exists).
 * Logic: Attempts to read the key. If the returned length is greater than 0, 
 *        the key is considered present.
 */
HB_FUNC( ISINIKEYEXISTS )
{
   TCHAR *section = HB_PARSTR( 1 );
   TCHAR *key = HB_PARSTR( 2 );
   TCHAR *filename = HB_PARSTR( 3 );

   TCHAR buffer[2] = { 0 };

   if( section && key && filename )
   {
      hb_retl( GetPrivateProfileString( section, key, TEXT( "" ), buffer, 2, filename ) > 0 );
   }
   else
   {
      hb_retl( HB_FALSE );
   }

   hb_freeW( section );
   hb_freeW( key );
   hb_freeW( filename );
}

/*
 * HB_FUNC( ISINISECTIONEXISTS )
 * Purpose: Checks if a specific section exists in the INI file.
 * Parameters:
 *    1. cSection  (String): The section name.
 *    2. cFileName (String): The path to the INI file.
 * Returns: Logical (.T. if the section exists).
 * Logic: Attempts to retrieve the section content. If the API returns 
 *        data, the section exists.
 */
HB_FUNC( ISINISECTIONEXISTS )
{
   TCHAR *section = HB_PARSTR( 1 );
   TCHAR *filename = HB_PARSTR( 2 );

   TCHAR buffer[256] = { 0 };

   if( section && filename )
   {
      hb_retl( GetPrivateProfileSection( section, buffer, 256, filename ) > 0 );
   }
   else
   {
      hb_retl( HB_FALSE );
   }

   hb_freeW( section );
   hb_freeW( filename );
}
