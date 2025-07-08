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
#include <mgdefs.h>

#ifdef UNICODE
// Functions to convert ANSI strings to wide character strings and vice versa
LPWSTR   AnsiToWide( LPCSTR );
LPSTR    WideToAnsi( LPWSTR );
#endif

// Function: GETPRIVATEPROFILESTRING
// Retrieves a string value from an INI file for a given section and key.
// Parameters:
//   1. Section name (string) - The section in the INI file.
//   2. Key name (string) - The specific key within the section.
//   3. Default value (string) - Value to return if key is not found.
//   4. File path (string) - Path to the INI file.
// Returns:
//   The value associated with the key as a string, or the default value if not found.
HB_FUNC( GETPRIVATEPROFILESTRING )
{
   DWORD nSize = 256;   // Initial buffer size
   TCHAR *bBuffer;      // Buffer to hold the retrieved string
   DWORD dwLen;         // Length of the string read from the file
#ifndef UNICODE
   // If not Unicode, retrieve parameters as ANSI strings
   LPCSTR   lpSection = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : NULL;
   LPCSTR   lpEntry = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : NULL;
   LPCSTR   lpDefault = hb_parc( 3 );
   LPCSTR   lpFileName = hb_parc( 4 );
#else
   // If Unicode, convert input parameters from ANSI to wide characters
   LPCWSTR  lpSection = HB_ISCHAR( 1 ) ? AnsiToWide( ( char * ) hb_parc( 1 ) ) : NULL;
   LPCWSTR  lpEntry = HB_ISCHAR( 2 ) ? AnsiToWide( ( char * ) hb_parc( 2 ) ) : NULL;
   LPCWSTR  lpDefault = AnsiToWide( ( char * ) hb_parc( 3 ) );
   LPCWSTR  lpFileName = AnsiToWide( ( char * ) hb_parc( 4 ) );
   LPSTR    pStr;       // Temporary variable for ANSI conversion
#endif

   // Loop until the buffer is large enough to hold the result
   do
   {
      nSize *= 2;       // Double the buffer size if needed
      bBuffer = ( TCHAR * ) hb_xgrab( sizeof( TCHAR ) * nSize );
      dwLen = GetPrivateProfileString( lpSection, lpEntry, lpDefault, bBuffer, nSize, lpFileName );
   }
   while( dwLen >= nSize - 1 );

   // If retrieval was successful, return the retrieved value
   if( dwLen )
   {
#ifndef UNICODE
      hb_retclen( ( TCHAR * ) bBuffer, dwLen ); // Return the string directly for ANSI
#else
      pStr = WideToAnsi( bBuffer );       // Convert from wide to ANSI
      hb_retc( pStr );                    // Return the ANSI string
      hb_xfree( pStr );                   // Free memory
      hb_xfree( ( TCHAR * ) lpFileName ); // Free memory for converted strings
      hb_xfree( ( TCHAR * ) lpDefault );
#endif
   }
   else                 // If retrieval failed, return the default value
   {
#ifndef UNICODE
      hb_retc( lpDefault );
#else
      pStr = WideToAnsi( ( LPWSTR ) lpDefault );
      hb_retc( pStr );
      hb_xfree( pStr );
      hb_xfree( ( TCHAR * ) lpFileName );
      hb_xfree( ( TCHAR * ) lpDefault );
#endif
   }

   hb_xfree( bBuffer ); // Free the buffer memory
}

// Function: WRITEPRIVATEPROFILESTRING
// Writes a string value to an INI file for a given section and key.
// Parameters:
//   1. Section name (string) - The section to modify or create.
//   2. Key name (string) - The key to modify or create.
//   3. Value (string) - The value to set for the specified key.
//   4. File path (string) - Path to the INI file.
// Returns:
//   A logical value indicating whether the write operation was successful.
HB_FUNC( WRITEPRIVATEPROFILESTRING )
{
#ifndef UNICODE
   LPCSTR   lpSection = hb_parc( 1 );
   LPCSTR   lpEntry = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : NULL;
   LPCSTR   lpData = HB_ISCHAR( 3 ) ? hb_parc( 3 ) : NULL;
   LPCSTR   lpFileName = hb_parc( 4 );
#else
   // Convert input parameters to wide characters
   LPCWSTR  lpSection = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPCWSTR  lpEntry = HB_ISCHAR( 2 ) ? AnsiToWide( ( char * ) hb_parc( 2 ) ) : NULL;
   LPCWSTR  lpData = HB_ISCHAR( 3 ) ? AnsiToWide( ( char * ) hb_parc( 3 ) ) : NULL;
   LPCWSTR  lpFileName = AnsiToWide( ( char * ) hb_parc( 4 ) );
#endif
   hb_retl( WritePrivateProfileString( lpSection, lpEntry, lpData, lpFileName ) );  // Write to INI
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpSection );
   hb_xfree( ( TCHAR * ) lpFileName );
#endif
}

// Function: DELINIENTRY
// Deletes a specific key from a section in the INI file.
// Parameters:
//   1. Section name (string) - The section containing the key.
//   2. Key name (string) - The key to delete.
//   3. File path (string) - Path to the INI file.
// Returns:
//   A logical value indicating whether the deletion was successful.
HB_FUNC( DELINIENTRY )
{
#ifndef UNICODE
   LPCSTR   lpSection = hb_parc( 1 );
   LPCSTR   lpEntry = hb_parc( 2 );
   LPCSTR   lpFileName = hb_parc( 3 );
#else
   LPCWSTR  lpSection = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPCWSTR  lpEntry = AnsiToWide( ( char * ) hb_parc( 2 ) );
   LPCWSTR  lpFileName = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
   hb_retl( WritePrivateProfileString( lpSection, lpEntry, NULL, lpFileName ) );    // NULL deletes the entry
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpSection );
   hb_xfree( ( TCHAR * ) lpEntry );
   hb_xfree( ( TCHAR * ) lpFileName );
#endif
}

// Function: DELINISECTION
// Deletes an entire section from an INI file.
// Parameters:
//   1. Section name (string) - The section to delete.
//   2. File path (string) - Path to the INI file.
// Returns:
//   A logical value indicating whether the section was successfully deleted.
HB_FUNC( DELINISECTION )
{
#ifndef UNICODE
   LPCSTR   lpSection = hb_parc( 1 );
   LPCSTR   lpFileName = hb_parc( 2 );
#else
   LPCWSTR  lpSection = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPCWSTR  lpFileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   hb_retl( WritePrivateProfileString( lpSection, NULL, TEXT( "" ), lpFileName ) ); // NULL deletes the section
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpSection );
   hb_xfree( ( TCHAR * ) lpFileName );
#endif
}

// Helper function: FindFirstSubString
// Finds the start of the first substring in a list of null-terminated strings.
static TCHAR *FindFirstSubString( TCHAR *Strings )
{
   TCHAR *p = Strings;
   if( *p == 0 )
   {
      p = NULL;                     // If empty, return NULL
   }

   return p;
}

// Helper function: FindNextSubString
// Finds the start of the next substring after a given string.
static TCHAR *FindNextSubString( TCHAR *Strings )
{
   TCHAR *p = Strings;
   p = p + lstrlen( Strings ) + 1;  // Move to next substring
   if( *p == 0 )
   {
      p = NULL;                     // Return NULL if end of list
   }

   return p;
}

// Helper function: FindLenSubString
// Finds the number of substrings in a list of null-terminated strings.
static INT FindLenSubString( TCHAR *Strings )
{
   INT   i = 0;
   TCHAR *p = Strings;

   if( ( p = FindFirstSubString( p ) ) != NULL )
   {
      for( i = 1; ( p = FindNextSubString( p ) ) != NULL; i++ );
   }

   return i;                  // Return count of substrings
}

// Function: _GETPRIVATEPROFILESECTIONNAMES
// Retrieves all section names from an INI file.
// Parameters:
//   1. File path (string) - Path to the INI file.
// Returns:
//   An array of section names.
HB_FUNC( _GETPRIVATEPROFILESECTIONNAMES )
{
   TCHAR    bBuffer[32767];   // Buffer for section names
   TCHAR    *p;
   INT      i, nLen;

#ifndef UNICODE
   LPCSTR   lpFileName = hb_parc( 1 );
#else
   LPCWSTR  lpFileName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPSTR    pStr;
#endif
   ZeroMemory( bBuffer, sizeof( bBuffer ) ); // Clear buffer
   GetPrivateProfileSectionNames( bBuffer, sizeof( bBuffer ) / sizeof( TCHAR ), lpFileName );

   p = ( TCHAR * ) bBuffer;
   nLen = FindLenSubString( p );             // Count sections
   hb_reta( nLen );           // Return array of sections
   if( nLen > 0 )
   {
#ifndef UNICODE
      HB_STORC( ( p = FindFirstSubString( p ) ), -1, 1 );
      for( i = 2; ( p = FindNextSubString( p ) ) != NULL; i++ )
      {
         HB_STORC( p, -1, i );
      }

#else
      p = FindFirstSubString( p );
      pStr = WideToAnsi( p );
      HB_STORC( pStr, -1, 1 );
      for( i = 2; ( p = FindNextSubString( p ) ) != NULL; i++ )
      {
         pStr = WideToAnsi( p );
         HB_STORC( pStr, -1, i );
      }

      hb_xfree( pStr );
      hb_xfree( ( TCHAR * ) lpFileName );
#endif
   }
}

// Function: _GETPRIVATEPROFILESECTION
// Retrieves all key-value pairs for a given section in an INI file.
// Parameters:
//   1. Section name (string) - The section to retrieve.
//   2. File path (string) - Path to the INI file.
// Returns:
//   An array of key-value pairs as strings.
HB_FUNC( _GETPRIVATEPROFILESECTION )
{
   TCHAR    bBuffer[32767];   // Buffer for section data
   TCHAR    *p;
   INT      i, nLen;

#ifndef UNICODE
   LPCSTR   lpSectionName = hb_parc( 1 );
   LPCSTR   lpFileName = hb_parc( 2 );
#else
   LPCWSTR  lpSectionName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPCWSTR  lpFileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
   LPSTR    pStr;
#endif
   ZeroMemory( bBuffer, sizeof( bBuffer ) ); // Clear buffer
   GetPrivateProfileSection( lpSectionName, bBuffer, sizeof( bBuffer ) / sizeof( TCHAR ), lpFileName );

   p = ( TCHAR * ) bBuffer;
   nLen = FindLenSubString( p );             // Count entries
   hb_reta( nLen );  // Return array of entries
   if( nLen > 0 )
   {
#ifndef UNICODE
      HB_STORC( ( p = FindFirstSubString( p ) ), -1, 1 );
      for( i = 2; ( p = FindNextSubString( p ) ) != NULL; i++ )
      {
         HB_STORC( p, -1, i );
      }

#else
      p = FindFirstSubString( p );
      pStr = WideToAnsi( p );
      HB_STORC( pStr, -1, 1 );
      for( i = 2; ( p = FindNextSubString( p ) ) != NULL; i++ )
      {
         pStr = WideToAnsi( p );
         HB_STORC( pStr, -1, i );
      }

      hb_xfree( pStr );
      hb_xfree( ( TCHAR * ) lpSectionName );
      hb_xfree( ( TCHAR * ) lpFileName );
#endif
   }
}
