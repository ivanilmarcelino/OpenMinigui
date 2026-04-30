/*
   MINIGUI - Harbour Win32 GUI library source code

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
    Copyright 1999-2026, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

   Parts of this code are contributed and used here under permission of the
   author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */

#include <mgdefs.h>
#include <shellapi.h>

#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );
#endif
HINSTANCE   GetInstance( void );
HINSTANCE   GetResources( void );

/* 
 * Internal HMG Resource Management Prototypes 
 * These functions track GDI objects to prevent memory leaks during application execution.
 */
void        RegisterResource( HANDLE hResource, LPCSTR szType );
void pascal DelResource( HANDLE hResource );

/*
 * FUNCTION COPYICON
 * ------------------
 * Purpose: Creates a duplicate of a specified icon.
 * 
 * Parameters:
 *    1: hIcon (HANDLE) - The handle of the icon to be copied.
 * 
 * Returns:
 *    HANDLE - The handle to the new icon duplicate.
 * 
 * Implementation Detail:
 *    Uses the Windows CopyIcon API. The new handle is registered with HMG's 
 *    resource manager to ensure it is tracked for eventual cleanup.
 */
HB_FUNC( COPYICON )
{
   // Retrieve the icon handle from the first Harbour parameter
   HICON hIcon = CopyIcon( hmg_par_raw_HICON( 1 ) );
   
   // Register the new handle in the HMG internal resource table
   RegisterResource( hIcon, "ICON" );
   
   // Return the raw handle back to the Harbour virtual machine
   hmg_ret_raw_HANDLE( hIcon );
}

/*
 * FUNCTION DESTROYICON
 * ---------------------
 * Purpose: Destroys an icon and releases any memory the icon occupied.
 * 
 * Parameters:
 *    1: hIcon (HANDLE) - The handle of the icon to destroy.
 * 
 * Returns:
 *    LOGICAL - .T. if successful, .F. otherwise.
 * 
 * Side Effects:
 *    Removes the icon from the HMG resource tracking system.
 */
HB_FUNC( DESTROYICON )
{
   HICON hIcon = hmg_par_raw_HICON( 1 );
   
   // Unregister from HMG tracking before physical destruction
   DelResource( hIcon );
   
   // Call Windows API to free GDI resources
   hb_retl( DestroyIcon( hIcon ) );
}

/*
 * FUNCTION DUPLICATEICON
 * -----------------------
 * Purpose: Creates a duplicate of an icon using the DuplicateIcon API.
 * 
 * Parameters:
 *    1: hIcon (HANDLE) - The handle of the icon to duplicate.
 * 
 * Returns:
 *    HANDLE - The handle to the duplicated icon.
 * 
 * Implementation Detail:
 *    Unlike CopyIcon, DuplicateIcon is often used when the icon belongs to 
 *    another module or process.
 */
HB_FUNC( DUPLICATEICON )
{
   // NULL instance indicates the icon is not associated with a specific module
   HICON hIcon = DuplicateIcon( NULL, hmg_par_raw_HICON( 1 ) );
   
   RegisterResource( hIcon, "ICON" );
   hmg_ret_raw_HANDLE( hIcon );
}

/*
 * FUNCTION LOADICON
 * ------------------
 * Purpose: Loads an icon resource from an executable or DLL.
 * 
 * Parameters:
 *    1: hInstance (HANDLE/NIL) - Module handle. If NIL, loads a system icon.
 *    2: cIconName (STRING/NUMERIC) - Resource name or integer ID.
 * 
 * Returns:
 *    HANDLE - The loaded icon handle.
 */
HB_FUNC( LOADICON )
{
   // Determine if we are using a specific instance or system resources
   HINSTANCE   hInst = HB_ISNIL( 1 ) ? NULL : hmg_par_raw_HINSTANCE( 1 );
   HICON       hIcon;

#ifndef UNICODE
   // Standard ANSI loading logic
   hIcon = LoadIcon( hInst, HB_ISCHAR( 2 ) ? hb_parc( 2 ) : MAKEINTRESOURCE( hb_parni( 2 ) ) );
#else
   // Convert Harbour string to Wide String for Unicode compatibility
   LPWSTR   pW = AnsiToWide( hb_parc( 2 ) );
   hIcon = LoadIcon( hInst, HB_ISCHAR( 2 ) ? pW : ( LPCWSTR ) MAKEINTRESOURCE( hb_parni( 2 ) ) );
   hb_xfree( pW );
#endif

   RegisterResource( hIcon, "ICON" );
   hmg_ret_raw_HANDLE( hIcon );
}

/*
 * FUNCTION EXTRACTICON
 * ---------------------
 * Purpose: Extracts an icon from a file (EXE, DLL, or ICO).
 * 
 * Parameters:
 *    1: cFileName (STRING) - Path to the file.
 *    2: nIconIndex (NUMERIC) - Zero-based index of the icon to extract.
 * 
 * Returns:
 *    HANDLE - The extracted icon handle.
 * 
 * Implementation Detail:
 *    If nIconIndex is -1, the function returns the count of icons in the file.
 */
HB_FUNC( EXTRACTICON )
{
#ifndef UNICODE
   const char  *lpFileName = hb_parc( 1 );
#else
   LPWSTR      lpFileName = AnsiToWide( hb_parc( 1 ) );
#endif
   int         nIconIndex = hmg_par_INT( 2 );

   if( nIconIndex == -1 )
   {
      // Return the count of icons available in the file
#if defined( __BORLANDC__ )
      hb_retni( ( int ) ExtractIcon( GetInstance(), lpFileName, nIconIndex ) );
#else
      hmg_ret_raw_HANDLE( ExtractIcon( GetInstance(), lpFileName, nIconIndex ) );
#endif
   }
   else
   {
      // Extract the specific icon and register it for HMG management
      HICON hIcon = ExtractIcon( GetInstance(), lpFileName, nIconIndex );
      RegisterResource( hIcon, "ICON" );
      hmg_ret_raw_HANDLE( hIcon );
   }

#ifdef UNICODE
   hb_xfree( lpFileName );
#endif
}

/*
 * FUNCTION EXTRACTICONEX
 * -----------------------
 * Purpose: Advanced icon extraction allowing size specification.
 * 
 * Parameters:
 *    1: cFileName (STRING) - Path to the file.
 *    2: nIconIndex (NUMERIC) - Index or -1 for count.
 *    3: nWidth (NUMERIC, Optional) - Desired width.
 *    4: nHeight (NUMERIC, Optional) - Desired height.
 * 
 * Returns:
 *    If index is -1: NUMERIC (Count).
 *    Otherwise: ARRAY { hIcon, nIconId }.
 */
HB_FUNC( EXTRACTICONEX )
{
#ifndef UNICODE
   const char  *lpFileName = hb_parc( 1 );
#else
   LPWSTR      lpFileName = AnsiToWide( hb_parc( 1 ) );
#endif
   int         nIconIndex = hb_parni( 2 );

   if( nIconIndex == -1 )
   {
      // Query total icon count using the most appropriate API for the compiler
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
      hmg_ret_UINT( ExtractIconEx( lpFileName, nIconIndex, NULL, NULL, 0 ) );
#else
      hmg_ret_UINT( PrivateExtractIcons( lpFileName, nIconIndex, 0, 0, NULL, NULL, 0, 0 ) );
#endif
   }
   else
   {
      HICON hIcon;
      UINT  nIconId = 0;
      UINT  nIconCount;

#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
      // Legacy Borland compilers use the standard ExtractIconEx
      nIconCount = ExtractIconEx( lpFileName, nIconIndex, &hIcon, NULL, 1 );
#else
      // Modern compilers use PrivateExtractIcons for better control over dimensions
      int   cx = hb_parnidef( 3, GetSystemMetrics( SM_CXICON ) );
      int   cy = hb_parnidef( 4, GetSystemMetrics( SM_CYICON ) );
      nIconCount = PrivateExtractIcons( lpFileName, nIconIndex, cx, cy, &hIcon, &nIconId, 1, 0 );
#endif
      if( nIconCount > 0 )
      {
         RegisterResource( hIcon, "ICON" );
         hb_reta( 2 ); // Return array with handle and ID
         hmg_storvnl_HANDLE( hIcon, -1, 1 );
         HB_STORNI( nIconId, -1, 2 );
      }
   }

#ifdef UNICODE
   hb_xfree( lpFileName );
#endif
}

/*
 * FUNCTION ISHICON
 * -----------------
 * Purpose: Validates if a handle is a valid Icon handle.
 * 
 * Parameters:
 *    1: hIcon (HANDLE) - The handle to validate.
 * 
 * Returns:
 *    LOGICAL - .T. if valid, .F. otherwise.
 * 
 * Implementation Detail:
 *    Uses GetIconInfo to probe the handle. Crucially, it deletes the 
 *    bitmaps created by GetIconInfo to prevent a GDI leak.
 */
HB_FUNC( ISHICON )
{
   ICONINFO ii;
   BOOL     bIsIcon = GetIconInfo( hmg_par_raw_HICON( 1 ), &ii );

   if( bIsIcon )
   {
      bIsIcon = ii.fIcon;
      
      // GetIconInfo creates new bitmap handles that MUST be deleted by the caller
      if( ii.hbmMask )
      {
         DeleteObject( ii.hbmMask );
      }

      if( ii.hbmColor )
      {
         DeleteObject( ii.hbmColor );
      }
   }

   hb_retl( bIsIcon );
}

/*
 * FUNCTION LOADICONBYNAME
 * ------------------------
 * Purpose: Loads an icon by name from resources or an external file.
 * 
 * Parameters:
 *    1: cIconName (STRING) - Resource name or file path.
 *    2: nWidth (NUMERIC) - Desired width.
 *    3: nHeight (NUMERIC) - Desired height.
 *    4: hInstance (HANDLE, Optional) - Instance handle.
 * 
 * Returns:
 *    HANDLE - The loaded icon handle.
 */
HB_FUNC( LOADICONBYNAME )
{
   HICON hIcon = NULL;

   if( hb_parclen( 1 ) > 0 )
   {
#ifndef UNICODE
      const char  *pszName = hb_parc( 1 );
#else
      LPWSTR      pszName = AnsiToWide( hb_parc( 1 ) );
#endif
      int         cx = hb_parni( 2 );
      int         cy = hb_parni( 3 );
      HINSTANCE   hInst = HB_ISNIL( 4 ) ? GetResources() : hmg_par_raw_HINSTANCE( 4 );

      // Attempt 1: Load from internal resources
      hIcon = ( HICON ) LoadImage( hInst, pszName, IMAGE_ICON, cx, cy, LR_DEFAULTCOLOR );
      
      // Attempt 2: If not in resources, try loading from an external file
      if( !hIcon )
      {
         hIcon = ( HICON ) LoadImage( NULL, pszName, IMAGE_ICON, cx, cy, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
      }

      if( hIcon )
      {
         RegisterResource( hIcon, "ICON" );
      }

#ifdef UNICODE
      hb_xfree( pszName );
#endif
   }

   hmg_ret_raw_HANDLE( hIcon );
}

/*
 * FUNCTION DRAWICONEX
 * --------------------
 * Purpose: Draws an icon on a window with flicker-reduction logic.
 * 
 * Parameters:
 *    1: hWnd (HANDLE) - Target window handle.
 *    2: nX, 3: nY (NUMERIC) - Coordinates.
 *    4: hIcon (HANDLE) - Icon to draw.
 *    5: nWidth, 6: nHeight (NUMERIC) - Dimensions.
 *    7: crBackColor (COLORREF) - Background color for the brush.
 *    8: lDestroy (LOGICAL) - If .T., the icon is destroyed after drawing.
 * 
 * Returns:
 *    LOGICAL - Success status.
 * 
 * Implementation Detail:
 *    Uses a solid brush to fill the background during the draw operation, 
 *    which significantly reduces flickering in event-driven UI updates.
 */
HB_FUNC( DRAWICONEX )
{
   HDC      hDC;
   HBRUSH   hBrush;
   BOOL     bResult;
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   HICON    hIcon = hmg_par_raw_HICON( 4 );
   
   // Validation to prevent crashes on invalid handles
   if( !IsWindow( hWnd ) || hIcon == NULL )
   {
      hb_retl( HB_FALSE );
      return;
   }

   hDC = GetDC( hWnd );
   
   // Create a brush with the specified background color for flicker-free drawing
   hBrush = CreateSolidBrush( hmg_par_COLORREF( 7 ) );
   
   // Perform the actual drawing using the extended Windows API
   bResult = DrawIconEx( hDC, hb_parni( 2 ), hb_parni( 3 ), hIcon, hb_parni( 5 ), hb_parni( 6 ), 0, hBrush, DI_NORMAL );

   // Cleanup GDI brush immediately
   DeleteObject( hBrush );

   // Optional auto-destruction of the icon handle to simplify Harbour-side logic
   if( hb_parldef( 8, HB_TRUE ) )
   {
      DelResource( hIcon );
      DestroyIcon( hIcon );
   }

   ReleaseDC( hWnd, hDC );

   hb_retl( bResult );
}