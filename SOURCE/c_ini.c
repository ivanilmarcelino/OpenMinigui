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

/*
 * FUNCTION GETPRIVATEPROFILESTRING( cSection, cKey, cDefault, cFile )
 *
 * Retrieves a string value from a specified key within a given section of an INI file.
 *
 * Parameters:
 *   cSection   : [Character] The name of the section in the INI file.
 *   cKey       : [Character] The name of the key whose value is to be retrieved.
 *   cDefault   : [Character] The default string to return if the specified key is not found.
 *   cFile      : [Character] The full path to the INI file.
 *
 * Returns:
 *   [Character] The string value associated with the key, or the default value if the key is not found.
 *
 * Purpose:
 *   This function provides a way to read configuration settings from an INI file.  It's used to
 *   retrieve application-specific settings such as user preferences, database connection strings,
 *   or other configurable parameters. The function handles the case where the requested key
 *   doesn't exist by returning a default value, ensuring that the application can continue to
 *   function even if the INI file is incomplete or missing.
 *
 * Notes:
 *   - The function dynamically allocates memory to store the retrieved string, ensuring that it can
 *     handle values of varying lengths.
 *   - It handles both ANSI and Unicode INI files, depending on the compilation settings.
 *   - If an error occurs during the retrieval process (e.g., the file is not found), the function
 *     returns an empty string.
 */
HB_FUNC( GETPRIVATEPROFILESTRING )
{
   DWORD    nSize = 256;
   TCHAR    *bBuffer = NULL;
   DWORD    dwLen;
   DWORD    lastError;
   BOOL     success;

#ifndef UNICODE
   LPCSTR   lpSection = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : NULL;
   LPCSTR   lpEntry = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : NULL;
   LPCSTR   lpDefault = hb_parc( 3 );
   LPCSTR   lpFileName = hb_parc( 4 );
#else
   LPWSTR   lpSection = HB_ISCHAR( 1 ) ? AnsiToWide( ( char * ) hb_parc( 1 ) ) : NULL;
   LPWSTR   lpEntry = HB_ISCHAR( 2 ) ? AnsiToWide( ( char * ) hb_parc( 2 ) ) : NULL;
   LPWSTR   lpDefault = AnsiToWide( ( char * ) hb_parc( 3 ) );
   LPWSTR   lpFileName = AnsiToWide( ( char * ) hb_parc( 4 ) );
   LPSTR    pStr;
#endif
   do
   {
      if( bBuffer != NULL )
      {
         hb_xfree( bBuffer );
      }

      nSize *= 2;
      bBuffer = ( TCHAR * ) hb_xgrab( sizeof( TCHAR ) * nSize );
      dwLen = GetPrivateProfileString( lpSection, lpEntry, lpDefault, bBuffer, nSize, lpFileName );
      lastError = GetLastError();   // Capture the error code
      if( dwLen == 0 && lastError != ERROR_SUCCESS )
      {
         // Handle the error (e.g., log it, return an error code)
         hb_retc( "" );             // Return an empty string on error
         success = FALSE;
         break;                     // Exit the loop
      }
      else
      {
         success = TRUE;
      }
   }
   while( dwLen >= nSize - 1 );

   if( success )
   {
      if( dwLen )
      {
#ifndef UNICODE
         hb_retclen( ( TCHAR * ) bBuffer, dwLen );
#else
         pStr = WideToAnsi( bBuffer );
         hb_retc( pStr );
         hb_xfree( pStr );
#endif
      }
      else
      {
#ifndef UNICODE
         hb_retc( lpDefault );
#else
         pStr = WideToAnsi( ( LPWSTR ) lpDefault );
         hb_retc( pStr );
         hb_xfree( pStr );
#endif
      }
   }

   if( bBuffer != NULL )
   {
      hb_xfree( bBuffer );
   }

#ifdef UNICODE
   hb_xfree( lpFileName );
   hb_xfree( lpDefault );
   if( lpSection )
   {
      hb_xfree( lpSection );
   }

   if( lpEntry )
   {
      hb_xfree( lpEntry );
   }
#endif
}

/*
 * FUNCTION WRITEPRIVATEPROFILESTRING( cSection, cKey, cValue, cFile )
 *
 * Writes a string value to a specified key within a given section of an INI file.
 *
 * Parameters:
 *   cSection : [Character] The name of the section in the INI file. If the section does not exist, it will be created.
 *   cKey     : [Character] The name of the key to write to. If NULL and cValue is NULL, the section is deleted.
 *   cValue   : [Character] The string value to write to the key. If NULL, the key is deleted.
 *   cFile    : [Character] The full path to the INI file.
 *
 * Returns:
 *   [Logical] .T. if the write operation was successful, .F. otherwise.
 *
 * Purpose:
 *   This function allows the application to persistently store configuration settings in an INI file.
 *   It's used to save user preferences, application state, or other configurable parameters that
 *   need to be preserved between application sessions. The function handles the creation of new
 *   sections and keys, as well as the deletion of existing ones.
 *
 * Notes:
 *   - The function handles both ANSI and Unicode INI files, depending on the compilation settings.
 *   - If an error occurs during the write operation, the function returns .F.
 */
HB_FUNC( WRITEPRIVATEPROFILESTRING )
{
#ifndef UNICODE
   LPCSTR   lpSection = hb_parc( 1 );
   LPCSTR   lpEntry = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : NULL;
   LPCSTR   lpData = HB_ISCHAR( 3 ) ? hb_parc( 3 ) : NULL;
   LPCSTR   lpFileName = hb_parc( 4 );
#else
   LPWSTR   lpSection = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPWSTR   lpEntry = HB_ISCHAR( 2 ) ? AnsiToWide( ( char * ) hb_parc( 2 ) ) : NULL;
   LPWSTR   lpData = HB_ISCHAR( 3 ) ? AnsiToWide( ( char * ) hb_parc( 3 ) ) : NULL;
   LPWSTR   lpFileName = AnsiToWide( ( char * ) hb_parc( 4 ) );
#endif
   BOOL     result = WritePrivateProfileString( lpSection, lpEntry, lpData, lpFileName );

   if( !result )
   {
      // DWORD lastError = GetLastError();
      // Log the error or return a specific error code to Harbour
      // Example: hb_errrt_raise(HB_ERR_GENERAL, 1234, "WritePrivateProfileString failed", lastError);
      hb_retl( FALSE );
   }
   else
   {
      hb_retl( TRUE );
   }

#ifdef UNICODE
   hb_xfree( lpSection );
   hb_xfree( lpFileName );
   if( lpEntry )
   {
      hb_xfree( lpEntry );
   }

   if( lpData )
   {
      hb_xfree( lpData );
   }
#endif
}

/*
 * FUNCTION DELINIENTRY( cSection, cKey, cFile )
 *
 * Deletes a specified key from a given section in an INI file.
 *
 * Parameters:
 *   cSection : [Character] The name of the section in the INI file.
 *   cKey     : [Character] The name of the key to delete.
 *   cFile    : [Character] The full path to the INI file.
 *
 * Returns:
 *   [Logical] .T. if the deletion was successful, .F. otherwise.
 *
 * Purpose:
 *   This function allows the application to remove specific configuration settings from an INI file.
 *   It's used to delete obsolete or no-longer-needed settings.
 *
 * Notes:
 *   - The function handles both ANSI and Unicode INI files, depending on the compilation settings.
 *   - The function internally calls WritePrivateProfileString with a NULL value to delete the key.
 */
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
   hb_retl( WritePrivateProfileString( lpSection, lpEntry, NULL, lpFileName ) );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpSection );
   hb_xfree( ( TCHAR * ) lpEntry );
   hb_xfree( ( TCHAR * ) lpFileName );
#endif
}

/*
 * FUNCTION DELINISECTION( cSection, cFile )
 *
 * Deletes a specified section from an INI file.
 *
 * Parameters:
 *   cSection : [Character] The name of the section to delete.
 *   cFile    : [Character] The full path to the INI file.
 *
 * Returns:
 *   [Logical] .T. if the deletion was successful, .F. otherwise.
 *
 * Purpose:
 *   This function allows the application to remove an entire section and all its associated keys
 *   from an INI file. It's used to remove obsolete or no-longer-needed sections.
 *
 * Notes:
 *   - The function handles both ANSI and Unicode INI files, depending on the compilation settings.
 *   - The function internally calls WritePrivateProfileString with a NULL key and an empty string value
 *     to delete the section.
 */
HB_FUNC( DELINISECTION )
{
#ifndef UNICODE
   LPCSTR   lpSection = hb_parc( 1 );
   LPCSTR   lpFileName = hb_parc( 2 );
#else
   LPCWSTR  lpSection = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPCWSTR  lpFileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   hb_retl( WritePrivateProfileString( lpSection, NULL, TEXT( "" ), lpFileName ) );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpSection );
   hb_xfree( ( TCHAR * ) lpFileName );
#endif
}

/*
 * Helper FUNCTION FindFirstSubString( pStrings )
 *
 * Finds the first substring from a double-null-terminated string block.
 *
 * Parameters:
 *   pStrings : [TCHAR*] Pointer to a block of null-separated strings.
 *
 * Returns:
 *   [TCHAR*] Pointer to the first substring, or NULL if empty.
 *
 * Purpose:
 *   Used to traverse Windows-style INI string lists.
 */
static TCHAR *FindFirstSubString( TCHAR *Strings )
{
   return( *Strings == 0 ) ? NULL : Strings;
}

/*
 * Helper FUNCTION FindNextSubString( pStrings )
 *
 * Finds the next string in a list of null-terminated strings.
 *
 * Parameters:
 *   pStrings : [TCHAR*] Current string pointer.
 *
 * Returns:
 *   [TCHAR*] Pointer to the next string, or NULL if end of list.
 */
static TCHAR *FindNextSubString( TCHAR *Strings )
{
   TCHAR *p = Strings + lstrlen( Strings ) + 1;
   return( *p == 0 ) ? NULL : p;
}

/*
 * Helper FUNCTION FindLenSubString( pStrings )
 *
 * Counts the number of substrings in a null-separated string block.
 *
 * Parameters:
 *   pStrings : [TCHAR*] Pointer to the string block.
 *
 * Returns:
 *   [INT] Number of substrings.
 */
static INT FindLenSubString( TCHAR *Strings )
{
   INT   i = 0;
   TCHAR *p = FindFirstSubString( Strings );
   while( p && ( p = FindNextSubString( p ) ) != NULL )
   {
      ++i;
   }

   return i + ( FindFirstSubString( Strings ) ? 1 : 0 ); // Adjust count to include the first string
}

/*
 * FUNCTION _GETPRIVATEPROFILESECTIONNAMES( cFile )
 *
 * Retrieves an array containing the names of all sections in an INI file.
 *
 * Parameters:
 *   cFile : [Character] The full path to the INI file.
 *
 * Returns:
 *   [Array of Character] An array where each element is the name of a section in the INI file.
 *                        Returns an empty array if no sections are found.
 *
 * Purpose:
 *   This function allows the application to enumerate all the sections in an INI file.  It's useful
 *   for tasks such as displaying a list of available configuration options to the user or validating
 *   the structure of the INI file.
 *
 * Notes:
 *   - The function dynamically allocates memory to store the section names, ensuring that it can
 *     handle INI files with a large number of sections.
 *   - It handles both ANSI and Unicode INI files, depending on the compilation settings.
 */
HB_FUNC( _GETPRIVATEPROFILESECTIONNAMES )
{
   DWORD    nSize = 32767; // Start with a reasonable size
   TCHAR    *bBuffer = NULL;
   TCHAR    *p;
   INT      i, nLen;
   BOOL     success;

#ifndef UNICODE
   LPCSTR   lpFileName = hb_parc( 1 );
#else
   LPWSTR   lpFileName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPSTR    pStr;
#endif
   DWORD    result;

   do
   {
      if( bBuffer != NULL )
      {
         hb_xfree( bBuffer );
      }

      bBuffer = ( TCHAR * ) hb_xgrab( nSize * sizeof( TCHAR ) );
      ZeroMemory( bBuffer, nSize * sizeof( TCHAR ) );

      result = GetPrivateProfileSectionNames( bBuffer, nSize, lpFileName );

      if( result == nSize - 2 )
      {                    // Buffer too small
         nSize *= 2;       // Double the buffer size
      }
      else if( result == 0 )
      {
         success = TRUE;
         break;
      }
      else
      {
         success = TRUE;
         break;
      }
   }
   while( TRUE );

   if( success )
   {
      p = bBuffer;
      nLen = FindLenSubString( p );
      hb_reta( nLen );

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
         for( i = 1; p != NULL; i++, p = FindNextSubString( p ) )
         {
            pStr = WideToAnsi( p );
            HB_STORC( pStr, -1, i );
            hb_xfree( pStr );
         }

         hb_xfree( lpFileName );
#endif
      }
   }
   else
   {
      hb_reta( 0 );        // Return an empty array on error
   }

   if( bBuffer != NULL )
   {
      hb_xfree( bBuffer );
   }
}

/*
 * FUNCTION _GETPRIVATEPROFILESECTION( cSection, cFile )
 *
 * Retrieves an array containing all key-value pairs from a specified section in an INI file.
 *
 * Parameters:
 *   cSection : [Character] The name of the section to retrieve.
 *   cFile    : [Character] The full path to the INI file.
 *
 * Returns:
 *   [Array of Character] An array where each element is a string in the format "key=value".
 *                        Returns an empty array if the section is not found or is empty.
 *
 * Purpose:
 *   This function allows the application to retrieve all the key-value pairs within a specific
 *   section of an INI file. It's useful for tasks such as loading all the settings for a particular
 *   module or component of the application.
 *
 * Notes:
 *   - The function handles both ANSI and Unicode INI files, depending on the compilation settings.
 *   - The function uses a fixed-size buffer to store the key-value pairs. If the section contains
 *     more data than the buffer can hold, the function may not retrieve all the data.
 */
HB_FUNC( _GETPRIVATEPROFILESECTION )
{
   TCHAR    bBuffer[32767];
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
   ZeroMemory( bBuffer, sizeof( bBuffer ) );
   GetPrivateProfileSection( lpSectionName, bBuffer, sizeof( bBuffer ) / sizeof( TCHAR ), lpFileName );

   p = bBuffer;
   nLen = FindLenSubString( p );
   hb_reta( nLen );

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
      for( i = 1; p != NULL; i++, p = FindNextSubString( p ) )
      {
         pStr = WideToAnsi( p );
         HB_STORC( pStr, -1, i );
         hb_xfree( pStr );
      }

      hb_xfree( ( TCHAR * ) lpSectionName );
      hb_xfree( ( TCHAR * ) lpFileName );
#endif
   }
}

/*
 * FUNCTION ISINIKEYEXISTS( cSection, cKey, cFile )
 *
 * Checks if a specified key exists within a given section of an INI file.
 *
 * Parameters:
 *   cSection : [Character] The name of the section to check.
 *   cKey     : [Character] The name of the key to check for.
 *   cFile    : [Character] The full path to the INI file.
 *
 * Returns:
 *   [Logical] .T. if the key exists in the section, .F. otherwise.
 *
 * Purpose:
 *   This function allows the application to determine whether a specific configuration setting
 *   is present in an INI file before attempting to read its value. This can be useful for
 *   handling optional settings or for validating the structure of the INI file.
 *
 * Notes:
 *   - The function handles both ANSI and Unicode INI files, depending on the compilation settings.
 *   - The function checks for the existence of the key by attempting to read its value into a small buffer.
 *     If the key exists, the read operation will succeed, and the function will return .T.
 */
HB_FUNC( ISINIKEYEXISTS )
{
#ifndef UNICODE
   LPCSTR   lpSection = hb_parc( 1 );
   LPCSTR   lpKey = hb_parc( 2 );
   LPCSTR   lpFileName = hb_parc( 3 );
   char     buffer[2] = { 0 };

   if( lpSection && lpKey && lpFileName )
   {
      DWORD len = GetPrivateProfileStringA( lpSection, lpKey, "", buffer, sizeof( buffer ), lpFileName );
      hb_retl( len > 0 );
   }
   else
   {
      hb_retl( HB_FALSE );
   }

#else
   LPWSTR   lpSection = AnsiToWide( hb_parc( 1 ) );
   LPWSTR   lpKey = AnsiToWide( hb_parc( 2 ) );
   LPWSTR   lpFileName = AnsiToWide( hb_parc( 3 ) );
   WCHAR    buffer[2] = { 0 };

   if( lpSection && lpKey && lpFileName )
   {
      DWORD len = GetPrivateProfileStringW( lpSection, lpKey, L"", buffer, sizeof( buffer ) / sizeof( WCHAR ), lpFileName );
      hb_retl( len > 0 );
   }
   else
   {
      hb_retl( HB_FALSE );
   }

   hb_xfree( lpSection );
   hb_xfree( lpKey );
   hb_xfree( lpFileName );
#endif
}

/*
 * FUNCTION ISINISECTIONEXISTS( cSection, cFile )
 *
 * Checks if a specified section exists in an INI file.
 *
 * Parameters:
 *   cSection : [Character] The name of the section to check for.
 *   cFile    : [Character] The full path to the INI file.
 *
 * Returns:
 *   [Logical] .T. if the section exists, .F. otherwise.
 *
 * Purpose:
 *   This function allows the application to determine whether a specific section is present in an
 *   INI file before attempting to read its contents. This can be useful for handling optional
 *   sections or for validating the structure of the INI file.
 *
 * Notes:
 *   - The function handles both ANSI and Unicode INI files, depending on the compilation settings.
 *   - The function checks for the existence of the section by attempting to read its contents into a buffer.
 *     If the section exists, the read operation will succeed, and the function will return .T.
 */
HB_FUNC( ISINISECTIONEXISTS )
{
#ifndef UNICODE
   LPCSTR   lpSection = hb_parc( 1 );
   LPCSTR   lpFileName = hb_parc( 2 );
   char     buffer[128] = { 0 };

   if( lpSection && lpFileName )
   {
      DWORD len = GetPrivateProfileSectionA( lpSection, buffer, sizeof( buffer ), lpFileName );
      hb_retl( len > 0 );
   }
   else
   {
      hb_retl( HB_FALSE );
   }

#else
   LPWSTR   lpSection = AnsiToWide( hb_parc( 1 ) );
   LPWSTR   lpFileName = AnsiToWide( hb_parc( 2 ) );
   WCHAR    buffer[256] = { 0 };

   if( lpSection && lpFileName )
   {
      DWORD len = GetPrivateProfileSectionW( lpSection, buffer, sizeof( buffer ) / sizeof( WCHAR ), lpFileName );
      hb_retl( len > 0 );
   }
   else
   {
      hb_retl( HB_FALSE );
   }

   hb_xfree( lpSection );
   hb_xfree( lpFileName );
#endif
}
