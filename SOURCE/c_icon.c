/* MINIGUI - Harbour Win32 GUI library source code

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

   Parts  of  this  code  is contributed and used here under permission of his
   author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */
#include <mgdefs.h>
#include <shellapi.h>

// Function prototype for converting ANSI strings to UNICODE (wide strings)
#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );
#endif

// Function prototypes for getting instances and resource handles for MiniGUI
HINSTANCE   GetInstance( void );
HINSTANCE   GetResources( void );

// Resource management functions for MiniGUI
// Registers a resource to manage and keep track of loaded resources
void        RegisterResource( HANDLE hResource, LPCSTR szType );

// Deletes or releases a registered resource
void pascal DelResource( HANDLE hResource );

// Function to duplicate an icon handle
HB_FUNC( COPYICON )
{
   HICON hIcon;

   // Duplicate the icon provided as input
   hIcon = CopyIcon( hmg_par_raw_HICON( 1 ) );

   // Register the duplicated icon as a resource in MiniGUI
   RegisterResource( hIcon, "ICON" );

   // Return the duplicated icon handle to Harbor
   hmg_ret_raw_HANDLE( hIcon );
}

// Function to destroy an icon handle and release its resources
HB_FUNC( DESTROYICON )
{
   HICON hIcon = hmg_par_raw_HICON( 1 );

   // Unregister the icon from MiniGUI resource management
   DelResource( hIcon );

   // Destroy the icon and return the result (TRUE if successful)
   hb_retl( DestroyIcon( hIcon ) );
}

// Function to create a duplicate of an icon from an HINSTANCE
HB_FUNC( DUPLICATEICON )
{
   HICON hIcon;

   // Duplicate the icon (source instance is NULL)
   hIcon = DuplicateIcon( ( HINSTANCE ) NULL, hmg_par_raw_HICON( 1 ) );

   // Register the duplicated icon as a resource
   RegisterResource( hIcon, "ICON" );

   // Return the duplicated icon handle
   hmg_ret_raw_HANDLE( hIcon );
}

// Function to load an icon from a resource or file
HB_FUNC( LOADICON )
{
   // Retrieve the instance to load the icon from; if none provided, set to NULL
   HINSTANCE   hinstance = ( HB_ISNIL( 1 ) ? NULL : hmg_par_raw_HINSTANCE( 1 ) );
   HICON       hIcon;

#ifndef UNICODE
   // Load the icon by name or resource ID if not UNICODE
   hIcon = LoadIcon( hinstance, HB_ISCHAR( 2 ) ? hb_parc( 2 ) : MAKEINTRESOURCE( hb_parni( 2 ) ) );
#else
   // Convert ANSI string to wide string for UNICODE systems
   LPWSTR   pW = AnsiToWide( ( char * ) hb_parc( 2 ) );
   hIcon = LoadIcon( hinstance, HB_ISCHAR( 2 ) ? pW : ( LPCWSTR ) MAKEINTRESOURCE( hb_parni( 2 ) ) );
#endif

   // Register the loaded icon as a resource
   RegisterResource( hIcon, "ICON" );

   // Return the icon handle
   hmg_ret_raw_HANDLE( hIcon );

#ifdef UNICODE
   hb_xfree( pW );   // Free memory allocated for wide string conversion
#endif
}

// Function to extract an icon from an executable file at a specific index
HB_FUNC( EXTRACTICON )
{
#ifndef UNICODE
   // Get file name as ANSI string if not UNICODE
   char     *lpFileName = ( char * ) hb_parc( 1 );
#else
   // Convert to wide string for UNICODE systems
   LPWSTR   lpFileName = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   int      nIconIndex = hmg_par_INT( 2 );         // Icon index in the file
   if( nIconIndex == -1 )
   {
      // Extract icon handle if the index is -1
      hmg_ret_raw_HANDLE( ExtractIcon( GetInstance(), lpFileName, nIconIndex ) );
   }
   else
   {
      HICON hIcon;

      // Extract and register the icon at the specified index
      hIcon = ExtractIcon( GetInstance(), lpFileName, nIconIndex );
      RegisterResource( hIcon, "ICON" );
      hmg_ret_raw_HANDLE( hIcon );
   }

#ifdef UNICODE
   hb_xfree( lpFileName );                         // Free allocated memory for wide string
#endif
}

// Function to extract multiple icons with custom sizes from a file
HB_FUNC( EXTRACTICONEX )
{
#ifndef UNICODE
   char     *lpFileName = ( char * ) hb_parc( 1 ); // File name for non-UNICODE
#else
   LPWSTR   lpFileName = AnsiToWide( ( char * ) hb_parc( 1 ) );   // Wide string for UNICODE
#endif
   int      nIconIndex = hb_parni( 2 );   // Icon index in file
   if( nIconIndex == -1 )
   {
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
      // Extract icons using ExtractIconEx for Borland 5.5 compiler
      hmg_ret_UINT( ExtractIconEx( lpFileName, nIconIndex, NULL, NULL, 0 ) );
#else
      // Use PrivateExtractIcons for other compilers
      hmg_ret_UINT( PrivateExtractIcons( lpFileName, nIconIndex, 0, 0, NULL, NULL, 0, 0 ) );
#endif
   }
   else
   {
      HICON hIcon;
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
      UINT  nIconId = 0;

      // Extract single icon for Borland 5.5 compiler
      UINT  nIconCount = ExtractIconEx( lpFileName, nIconIndex, &hIcon, NULL, 1 );
#else
      UINT  nIconId;
      int   cx = hb_parnidef( 3, GetSystemMetrics( SM_CXICON ) ); // Icon width
      int   cy = hb_parnidef( 4, GetSystemMetrics( SM_CYICON ) ); // Icon height

      // Extract icon using PrivateExtractIcons
      UINT  nIconCount = PrivateExtractIcons( lpFileName, nIconIndex, cx, cy, &hIcon, &nIconId, 1, 0 );
#endif
      if( nIconCount > 0 )
      {
         hb_reta( 2 );

         // Register the icon at the specified index
         RegisterResource( hIcon, "ICON" );

         // Store icon and ID in return array
         hmg_storvnl_HANDLE( hIcon, -1, 1 );
         HB_STORNI( nIconId, -1, 2 );
      }
   }

#ifdef UNICODE
   hb_xfree( lpFileName );                   // Free wide string memory
#endif
}

// Function to check if a handle is an icon
HB_FUNC( ISHICON )
{
   ICONINFO iconinfo;
   BOOL     bTrue;

   // Get icon information; if successful, confirm if it's an icon
   bTrue = GetIconInfo( hmg_par_raw_HICON( 1 ), &iconinfo );

   if( bTrue )
   {
      bTrue = iconinfo.fIcon;

      // Delete associated bitmaps to release memory
      if( iconinfo.hbmMask )
      {
         DeleteObject( iconinfo.hbmMask );
      }

      if( iconinfo.hbmColor )
      {
         DeleteObject( iconinfo.hbmColor );
      }
   }

   // Return TRUE if the handle is an icon
   hb_retl( bTrue );
}

// Function to load an icon by name from resources or file
HB_FUNC( LOADICONBYNAME )
{
   HICON hIcon = NULL;

   if( hb_parclen( 1 ) > 0 )
   {
#ifndef UNICODE
      const char  *pszResOrFile = hb_parc( 1 );
#else
      LPCWSTR     pszResOrFile = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
      int         cxDesired = hb_parni( 2 ); // Desired icon width
      int         cyDesired = hb_parni( 3 ); // Desired icon height
      HINSTANCE   hInstance = HB_ISNIL( 4 ) ? GetResources() : hmg_par_raw_HINSTANCE( 4 );

      // Load icon from resources or file with specified size
      hIcon = ( HICON ) LoadImage( hInstance, pszResOrFile, IMAGE_ICON, cxDesired, cyDesired, LR_DEFAULTCOLOR );

      if( hIcon == NULL )
      {
         // Fallback to loading from file if resource loading fails
         hIcon = ( HICON ) LoadImage( 0, pszResOrFile, IMAGE_ICON, cxDesired, cyDesired, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
      }

      if( hIcon != NULL )
      {
         // Register loaded icon as a resource
         RegisterResource( hIcon, "ICON" );
      }

#ifdef UNICODE
      hb_xfree( ( TCHAR * ) pszResOrFile );  // Free wide string memory
#endif
   }

   hmg_ret_raw_HANDLE( hIcon );              // Return icon handle
}

// Function to draw an icon on a window with a background brush to reduce flickering
HB_FUNC( DRAWICONEX )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hwnd ) )
   {
      HICON    hIcon = hmg_par_raw_HICON( 4 );
      HDC      hdc = GetDC( hwnd );          // Get device context for drawing
      HBRUSH   hbrFlickerFreeDraw = CreateSolidBrush( hmg_par_COLORREF( 7 ) );   // Create background brush

      // Draw the icon at specified coordinates with flicker-free background
      hb_retl( DrawIconEx( hdc, hb_parni( 2 ), hb_parni( 3 ), hIcon, hb_parni( 5 ), hb_parni( 6 ), 0, hbrFlickerFreeDraw, DI_NORMAL ) );

      // Release resources
      DeleteObject( hbrFlickerFreeDraw );

      if( hb_parldef( 8, HB_TRUE ) )
      {
         DelResource( hIcon );   // Unregister the icon
         DestroyIcon( hIcon );   // Destroy the icon handle
      }

      ReleaseDC( hwnd, hdc );    // Release device context
   }
}
