/*----------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   Copyright 2005 Andy Wos <andywos@unwired.com.au>

   Added to MiniGUI project (1.0 Experimental Build 9a)
   by Jacek Kubica <kubica@wssk.wroc.pl>

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

#include <mgdefs.h>     // Include application-specific definitions
#include <hbapiitm.h>   // Include Harbour API item definitions

// Function to get the address of a procedure from a module
extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );

// Utility functions to convert arrays to various structures
BOOL              Array2Rect( PHB_ITEM aRect, RECT *rc );
BOOL              Array2Point( PHB_ITEM aPoint, POINT *pt );
BOOL              Array2ColorRef( PHB_ITEM aCRef, COLORREF *cr );

#if ( defined( _MSC_VER ) && !defined( __POCC__ ) ) || defined( __MINGW32__ ) || ( defined( __BORLANDC__ ) && __BORLANDC__ >= 1410 )
#include "uxtheme.h"
#endif
#ifndef _UXTHEME_H_
#define _UXTHEME_H_

#include <commctrl.h>   // Include common control definitions and structures for UI elements

// Define necessary macros and structures if not already defined
#if defined( __XCC__ ) || ( defined( __POCC__ ) && __POCC__ >= 900 )
#ifndef _NO_W32_PSEUDO_MODIFIERS
#define IN
#define OUT
#ifndef OPTIONAL
#define OPTIONAL
#endif
#endif
#endif

// Define handle to theme data
typedef HANDLE HTHEME;

// Function prototypes for theme-related functions
HRESULT WINAPI CloseThemeData( HTHEME hTheme );
HRESULT WINAPI DrawThemeBackground( HTHEME, HDC, int, int, const RECT *, const RECT * );

// Define constants for DrawThemeBackgroundEx function
#define DTBG_CLIPRECT         0x00000001
#define DTBG_DRAWSOLID        0x00000002
#define DTBG_OMITBORDER       0x00000004
#define DTBG_OMITCONTENT      0x00000008
#define DTBG_COMPUTINGREGION  0x00000010
#define DTBG_MIRRORDC         0x00000020

// Define structure for DrawThemeBackgroundEx function
typedef struct _DTBGOPTS
{
   DWORD dwSize;
   DWORD dwFlags;
   RECT  rcClip;
} DTBGOPTS, *PDTBGOPTS;

// Function prototype for DrawThemeBackgroundEx
HRESULT WINAPI DrawThemeBackgroundEx( HTHEME, HDC, int, int, const RECT *, const DTBGOPTS * );

// Function prototype for DrawThemeEdge
HRESULT WINAPI DrawThemeEdge( HTHEME, HDC, int, int, const RECT *, UINT, UINT, RECT * );

// Function prototype for DrawThemeIcon
HRESULT WINAPI DrawThemeIcon( HTHEME, HDC, int, int, const RECT *, HIMAGELIST, int );

// Function prototype for DrawThemeParentBackground
HRESULT WINAPI DrawThemeParentBackground( HWND, HDC, RECT * );

// Define constant for DrawThemeText function
#define DTT_GRAYED   0x1

// Function prototype for DrawThemeText
HRESULT WINAPI DrawThemeText( HTHEME, HDC, int, int, LPCWSTR, int, DWORD, DWORD, const RECT * );

// Define constants for EnableThemeDialogTexture function
#define ETDT_DISABLE       0x00000001
#define ETDT_ENABLE        0x00000002
#define ETDT_USETABTEXTURE 0x00000004
#define ETDT_ENABLETAB     ( ETDT_ENABLE | ETDT_USETABTEXTURE )

// Function prototype for EnableThemeDialogTexture
HRESULT WINAPI EnableThemeDialogTexture( HWND, DWORD );

// Function prototype for EnableTheming
HRESULT WINAPI EnableTheming( BOOL );

// Function prototype for GetCurrentThemeName
HRESULT WINAPI GetCurrentThemeName( LPWSTR, int, LPWSTR, int, LPWSTR, int );

// Define constants for GetThemeAppProperties function
#define STAP_ALLOW_NONCLIENT  ( 1 << 0 )
#define STAP_ALLOW_CONTROLS   ( 1 << 1 )
#define STAP_ALLOW_WEBCONTENT ( 1 << 2 )

// Function prototype for GetThemeAppProperties
DWORD WINAPI   GetThemeAppProperties( void );

// Function prototype for GetThemeBackgroundContentRect
HRESULT WINAPI GetThemeBackgroundContentRect( HTHEME, HDC, int, int, const RECT *, RECT * );

// Function prototype for GetThemeBackgroundExtent
HRESULT WINAPI GetThemeBackgroundExtent( HTHEME, HDC, int, int, const RECT *, RECT * );

// Function prototype for GetThemeBackgroundRegion
HRESULT WINAPI GetThemeBackgroundRegion( HTHEME, HDC, int, int, const RECT *, HRGN * );

// Function prototype for GetThemeBool
HRESULT WINAPI GetThemeBool( HTHEME, int, int, int, BOOL * );

// Function prototype for GetThemeColor
HRESULT WINAPI GetThemeColor( HTHEME, int, int, int, COLORREF * );

// Function prototype for GetThemeDocumentationProperty
HRESULT WINAPI GetThemeDocumentationProperty( LPCWSTR, LPCWSTR, LPWSTR, int );

// Function prototype for GetThemeEnumValue
HRESULT WINAPI GetThemeEnumValue( HTHEME, int, int, int, int * );

// Function prototype for GetThemeFilename
HRESULT WINAPI GetThemeFilename( HTHEME, int, int, int, LPWSTR, int );

// Function prototype for GetThemeFont
HRESULT WINAPI GetThemeFont( HTHEME, HDC, int, int, int, LOGFONTW * );

// Function prototype for GetThemeInt
HRESULT WINAPI GetThemeInt( HTHEME, int, int, int, int * );

// Define maximum number of integers in a list
#define MAX_INTLIST_COUNT  10

// Define structure for integer list
typedef struct _INTLIST
{
   int   iValueCount;
   int   iValues[MAX_INTLIST_COUNT];
} INTLIST, *PINTLIST;

// Function prototype for GetThemeIntList
HRESULT WINAPI GetThemeIntList( HTHEME, int, int, int, INTLIST * );

// Define structure for margins
typedef struct _MARGINS
{
   int   cxLeftWidth;
   int   cxRightWidth;
   int   cyTopHeight;
   int   cyBottomHeight;
} MARGINS, *PMARGINS;

// Function prototype for GetThemeMargins
HRESULT WINAPI GetThemeMargins( HTHEME, HDC, int, int, int, RECT *, MARGINS * );

// Function prototype for GetThemeMetric
HRESULT WINAPI GetThemeMetric( HTHEME, HDC, int, int, int, int * );

// Define enum for theme size
#if !( defined( __POCC__ ) || defined( __WATCOMC__ ) )
typedef enum
{
   TS_MIN,
   TS_TRUE,
   TS_DRAW
} THEMESIZE;
#else
enum THEMESIZE
{
   TS_MIN,
   TS_TRUE,
   TS_DRAW,
};
#endif

// Function prototype for GetThemePartSize
HRESULT WINAPI GetThemePartSize( HTHEME, HDC, int, int, RECT *, enum THEMESIZE, SIZE * );

// Function prototype for GetThemePosition
HRESULT WINAPI GetThemePosition( HTHEME, int, int, int, POINT * );

// Define enum for property origin
typedef enum
{
   PO_STATE,
   PO_PART,
   PO_CLASS,
   PO_GLOBAL,
   PO_NOTFOUND
} PROPERTYORIGIN;

// Function prototype for GetThemePropertyOrigin
HRESULT WINAPI GetThemePropertyOrigin( HTHEME, int, int, int, PROPERTYORIGIN * );

// Function prototype for GetThemeRect
HRESULT WINAPI GetThemeRect( HTHEME, int, int, int, RECT * );

// Function prototype for GetThemeString
HRESULT WINAPI GetThemeString( HTHEME, int, int, int, LPWSTR, int );

// Function prototype for GetThemeSysBool
BOOL WINAPI GetThemeSysBool( HTHEME, int );

// Function prototype for GetThemeSysColor
COLORREF WINAPI GetThemeSysColor( HTHEME, int );

// Function prototype for GetThemeSysColorBrush
HBRUSH WINAPI GetThemeSysColorBrush( HTHEME, int );

// Function prototype for GetThemeSysFont
HRESULT WINAPI GetThemeSysFont( HTHEME, int, LOGFONTW * );

// Function prototype for GetThemeSysInt
HRESULT WINAPI GetThemeSysInt( HTHEME, int, int * );

// Function prototype for GetThemeSysSize
int WINAPI GetThemeSysSize( HTHEME, int );

// Function prototype for GetThemeSysString
HRESULT WINAPI GetThemeSysString( HTHEME, int, LPWSTR, int );

// Function prototype for GetThemeTextExtent
HRESULT WINAPI GetThemeTextExtent( HTHEME, HDC, int, int, LPCWSTR, int, DWORD, const RECT *, RECT * );

// Function prototype for GetThemeTextMetrics
HRESULT WINAPI GetThemeTextMetrics( HTHEME, HDC, int, int, TEXTMETRICW * );

// Function prototype for GetWindowTheme
HTHEME WINAPI GetWindowTheme( HWND );

// Define constants for HitTestThemeBackground function
#define HTTB_BACKGROUNDSEG          0x0000
#define HTTB_FIXEDBORDER            0x0002
#define HTTB_CAPTION                0x0004
#define HTTB_RESIZINGBORDER_LEFT    0x0010
#define HTTB_RESIZINGBORDER_TOP     0x0020
#define HTTB_RESIZINGBORDER_RIGHT   0x0040
#define HTTB_RESIZINGBORDER_BOTTOM  0x0080
#define HTTB_RESIZINGBORDER         ( HTTB_RESIZINGBORDER_LEFT | HTTB_RESIZINGBORDER_TOP | HTTB_RESIZINGBORDER_RIGHT | HTTB_RESIZINGBORDER_BOTTOM )
#define HTTB_SIZINGTEMPLATE         0x0100
#define HTTB_SYSTEMSIZINGMARGINS    0x0200

// Function prototype for HitTestThemeBackground
HRESULT WINAPI HitTestThemeBackground( HTHEME, HDC, int, int, DWORD, const RECT *, HRGN, POINT, WORD * );

// Function prototype for IsAppThemed
BOOL WINAPI    IsAppThemed( void );

// Function prototype for IsThemeActive
BOOL WINAPI    IsThemeActive( void );

// Function prototype for IsThemeBackgroundPartiallyTransparent
BOOL WINAPI    IsThemeBackgroundPartiallyTransparent( HTHEME, int, int );

// Function prototype for IsThemeDialogTextureEnabled
BOOL WINAPI    IsThemeDialogTextureEnabled( void );

// Function prototype for IsThemePartDefined
BOOL WINAPI    IsThemePartDefined( HTHEME, int, int );

// Function prototype for OpenThemeData
HTHEME WINAPI  OpenThemeData( HWND, LPCWSTR );

// Function prototype for SetThemeAppProperties
void WINAPI    SetThemeAppProperties( DWORD );

// Function prototype for SetWindowTheme
HRESULT WINAPI SetWindowTheme( HWND, LPCWSTR, LPCWSTR );
#endif /* _UXTHEME_H_ */

// Define function pointer types for various theme-related functions
typedef HTHEME ( WINAPI *fnOpenThemeData ) ( HWND hwnd, LPCWSTR pszClassList );
typedef HRESULT ( WINAPI *fnCloseThemeData ) ( HTHEME hTheme );
typedef HRESULT ( WINAPI *fnDrawThemeBackground ) ( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const RECT * pRect, const RECT * pClipRect );
typedef HRESULT ( WINAPI *fnGetThemeBackgroundContentRect )
   (
      HTHEME hTheme,
      HDC hdc,
      int iPartId,
      int iStateId,
      const RECT * pBoundingRect,
      RECT * pContentRect
   );
typedef HRESULT ( WINAPI *fnDrawThemeText )
   (
      HTHEME hTheme,
      HDC hdc,
      int iPartId,
      int iStateId,
      LPCWSTR pszText,
      int iCharCount,
      DWORD dwTextFlags,
      DWORD dwTextFlags2,
      const RECT * pRect
   );
typedef HRESULT ( WINAPI *fnHitTestThemeBackground )
   (
      HTHEME hTheme,
      OPTIONAL HDC hdc,
      int iPartId,
      int iStateId,
      DWORD dwOptions,
      const RECT * pRect,
      OPTIONAL HRGN hrgn,
      POINT ptTest,
      OUT WORD * pwHitTestCode
   );
typedef BOOL ( WINAPI *fnIsAppThemed ) ( void );
typedef COLORREF ( WINAPI *fnGetThemeSysColor ) ( HTHEME hTheme, int iColorId );
typedef HRESULT ( WINAPI *fnGetThemeSysFont ) ( HTHEME hTheme, int iFontId, OUT LOGFONT * plf );
typedef HRESULT ( WINAPI *fnDrawThemeIcon ) ( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const RECT * pRect, HIMAGELIST himl, int iImageIndex );
typedef HRESULT ( WINAPI *fnGetThemeTextExtent )
   (
      HTHEME hTheme,
      HDC hdc,
      int iPartId,
      int iStateId,
      LPCWSTR pszText,
      int iCharCount,
      DWORD dwTextFlags,
      const RECT * pRect,
      OUT RECT * pExtent
   );
typedef HRESULT ( WINAPI *fnDrawThemeParentBackground ) ( HWND hwnd, HDC hdc, OPTIONAL RECT * prc );
typedef HRESULT ( WINAPI *fnDrawThemeEdge )
   (
      HTHEME hTheme,
      HDC hdc,
      int iPartId,
      int iStateId,
      const RECT * pDestRect,
      UINT uEdge,
      UINT uFlags,
      OPTIONAL OUT RECT * pContentRect
   );
typedef HRESULT ( WINAPI *fnGetThemeRect ) ( HTHEME hTheme, int iPartId, int iStateId, int iPropId, RECT * pPoint );
typedef HRESULT ( WINAPI *fnGetThemePartSize ) ( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, RECT * prc, enum THEMESIZE eSize, SIZE * psz );
typedef void ( WINAPI *fnSetThemeAppProperties ) ( DWORD dwFlags );
typedef DWORD ( WINAPI *fnGetThemeAppProperties ) ( void );
typedef HTHEME ( WINAPI *fnGetWindowTheme ) ( HWND hWnd );
typedef BOOL ( WINAPI *fnIsThemeActive ) ( void );
typedef HRESULT ( WINAPI *fnSetWindowTheme ) ( HWND hwnd, LPCWSTR pszSubAppName, LPCWSTR pszSubIdList );
typedef HRESULT ( WINAPI *fnEnableThemeDialogTexture ) ( HWND hwnd, DWORD dwFlags );
typedef HRESULT ( WINAPI *fnGetThemeColor ) ( HTHEME hTheme, int iPartId, int iStateId, int iPropId, COLORREF * pColor );

// Global variable to hold the instance handle of the "uxtheme.dll" library
static HINSTANCE  hUxTheme;

/*
 * FUNCTION: InitUxTheme
 *
 * This function loads the "uxtheme.dll" library if it is not already loaded.
 *
 * Returns:
 *   The handle to the loaded library.
 */
HINSTANCE InitUxTheme( void )
{
   // Load the "uxtheme.dll" library if not already loaded
   if( hUxTheme == NULL )
   {
      hUxTheme = LoadLibraryEx( TEXT( "uxtheme.dll" ), NULL, 0 );
   }

   // Return the library handle
   return hUxTheme;
}

/*
 * FUNCTION: EndUxTheme
 *
 * This function frees the library if it has been loaded and resets the handle.
 */
void EndUxTheme( void )
{
   // Free the library if it has been loaded and reset the handle
   if( hUxTheme != NULL )
   {
      FreeLibrary( hUxTheme );
      hUxTheme = NULL;
   }
}

/*
 * FUNCTION: INITUXTHEME
 *
 * This function initializes the uxtheme library and returns its handle.
 *
 * Returns:
 *   The handle to the uxtheme library.
 */
HB_FUNC( INITUXTHEME )
{
   hmg_ret_raw_HANDLE( InitUxTheme() );
}

/*
 * FUNCTION: ENDUXTHEME
 *
 * This function frees the uxtheme library.
 */
HB_FUNC( ENDUXTHEME )
{
   EndUxTheme();
}

/*
 * FUNCTION: ISTHEMEACTIVE
 *
 * This function checks if themes are currently active in the operating system.
 *
 * Returns:
 *   TRUE if themes are active, FALSE otherwise.
 */
HB_FUNC( ISTHEMEACTIVE )
{
   BOOL  bRet = HB_FALSE;

   // Load the library if not already loaded
   if( hUxTheme == NULL )
   {
      hUxTheme = LoadLibraryEx( TEXT( "uxtheme.dll" ), NULL, 0 );
   }

   if( hUxTheme )
   {
      // Get the address of "IsThemeActive" function
      fnIsThemeActive   pfn = ( fnIsThemeActive ) wapi_GetProcAddress( hUxTheme, "IsThemeActive" );
      if( pfn )
      {
         // Call the function to check if themes are active
         bRet = ( BOOL ) pfn();
      }
   }

   // Return the result as a boolean
   hb_retl( bRet );
}

/*
 * FUNCTION: ISAPPTHEMED
 *
 * This function checks if themes are enabled for the application.
 *
 * Returns:
 *   TRUE if themes are enabled for the application, FALSE otherwise.
 */
HB_FUNC( ISAPPTHEMED )
{
   BOOL  bRet = HB_FALSE;

   // Load the library if not already loaded
   if( hUxTheme == NULL )
   {
      hUxTheme = LoadLibraryEx( TEXT( "uxtheme.dll" ), NULL, 0 );
   }

   if( hUxTheme )
   {
      // Get the address of "IsAppThemed" function
      fnIsAppThemed  pfn = ( fnIsAppThemed ) wapi_GetProcAddress( hUxTheme, "IsAppThemed" );
      if( pfn )
      {
         // Call the function to check if application themes are enabled
         bRet = ( BOOL ) pfn();
      }
   }

   // Return the result as a boolean
   hb_retl( bRet );
}

/*
 * FUNCTION: OPENTHEMEDATA
 *
 * This function opens themed data for a given window and class list.
 *
 * Parameters:
 *   hWnd: Handle to the window.
 *   pszClassList: Pointer to the class list string.
 *
 * Returns:
 *   The handle to the opened theme data if successful, otherwise NIL.
 */
HB_FUNC( OPENTHEMEDATA )
{
   HTHEME   nRet = ( HTHEME ) NULL;
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   LPCWSTR  pszClassList = ( LPCWSTR ) hb_parc( 2 );

   // Load the library if not already loaded
   if( hUxTheme == NULL )
   {
      hUxTheme = LoadLibraryEx( TEXT( "uxtheme.dll" ), NULL, 0 );
   }

   if( hUxTheme )
   {
      // Get the address of "OpenThemeData" function
      fnOpenThemeData   pfn = ( fnOpenThemeData ) wapi_GetProcAddress( hUxTheme, "OpenThemeData" );
      if( pfn )
      {
         // Open themed data for the specified window and class list
         nRet = ( HTHEME ) pfn( hWnd, pszClassList );
      }
   }

   // Return the theme handle if successful, otherwise return NIL
   if( nRet != NULL )
   {
      hmg_ret_raw_HANDLE( nRet );
   }
   else
   {
      hb_ret();
   }
}

/*
 * FUNCTION: CLOSETHEMEDATA
 *
 * This function closes themed data for a given theme handle.
 *
 * Parameters:
 *   hTheme: Handle to the theme data.
 *
 * Returns:
 *   TRUE if the operation was successful, otherwise FALSE.
 */
HB_FUNC( CLOSETHEMEDATA )
{
   HRESULT  nRet = S_FALSE;
   HTHEME   hTheme = hmg_par_raw_HTHEME( 1 );

   // Load the library if not already loaded
   if( hUxTheme == NULL )
   {
      hUxTheme = LoadLibraryEx( TEXT( "uxtheme.dll" ), NULL, 0 );
   }

   if( hUxTheme )
   {
      // Get the address of "CloseThemeData" function
      fnCloseThemeData  pfn = ( fnCloseThemeData ) wapi_GetProcAddress( hUxTheme, "CloseThemeData" );
      if( pfn )
      {
         // Close themed data for the specified theme handle
         nRet = ( HRESULT ) pfn( hTheme );
      }
   }

   // Return TRUE if the operation was successful, otherwise FALSE
   hmg_ret_L( nRet == S_OK );
}

/*
 * FUNCTION: DRAWTHEMEBACKGROUND
 *
 * This function draws a themed background in a specified rectangle.
 *
 * Parameters:
 *   hTheme: Handle to the theme.
 *   hDC: Handle to the device context.
 *   iPartId: Part identifier.
 *   iStateId: State identifier.
 *   pRect: Rectangle to draw the background in.
 *   pClipRect: Clipping rectangle.
 *
 * Returns:
 *   TRUE if the operation was successful, otherwise FALSE.
 */
HB_FUNC( DRAWTHEMEBACKGROUND )
{
   HRESULT  nRet = S_FALSE;
   HTHEME   hTheme = hmg_par_raw_HTHEME( 1 );
   HDC      hDC = hmg_par_raw_HDC( 2 );
   int      iPartId = hb_parni( 3 );
   int      iStateId = hb_parni( 4 );

   RECT     pRect;
   RECT     pClipRect;

   // Convert array parameters to RECT structures
   Array2Rect( hb_param( 5, HB_IT_ARRAY ), &pRect );
   Array2Rect( hb_param( 6, HB_IT_ARRAY ), &pClipRect );

   // Load the library if not already loaded
   if( hUxTheme == NULL )
   {
      hUxTheme = LoadLibraryEx( TEXT( "uxtheme.dll" ), NULL, 0 );
   }

   if( hUxTheme )
   {
      // Get the address of "DrawThemeBackground" function
      fnDrawThemeBackground   pfn = ( fnDrawThemeBackground ) wapi_GetProcAddress( hUxTheme, "DrawThemeBackground" );
      if( pfn )
      {
         // Draw the themed background
         nRet = ( HRESULT ) pfn( hTheme, hDC, iPartId, iStateId, &pRect, &pClipRect );
      }
   }

   // Return TRUE if the operation was successful, otherwise FALSE
   hmg_ret_L( nRet == S_OK );
}

/*
 * FUNCTION: DRAWTHEMEPARENTBACKGROUND
 *
 * This function draws the themed background of a parent window.
 *
 * Parameters:
 *   hWnd: Handle to the window.
 *   hDC: Handle to the device context.
 *   pRect: Rectangle to draw the background in.
 *
 * Returns:
 *   TRUE if the operation was successful, otherwise FALSE.
 */
HB_FUNC( DRAWTHEMEPARENTBACKGROUND )
{
   HRESULT  nRet = S_FALSE;
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   HDC      hDC  = hmg_par_raw_HDC( 2 );
   RECT     pRect;

   // Convert array parameter to RECT structure if provided
   if( HB_ISARRAY( 3 ) )
   {
      Array2Rect( hb_param( 3, HB_IT_ARRAY ), &pRect );
   }

   // Load the library if not already loaded
   if( hUxTheme == NULL )
   {
      hUxTheme = LoadLibraryEx( TEXT( "uxtheme.dll" ), NULL, 0 );
   }

   if( hUxTheme )
   {
      // Get the address of "DrawThemeParentBackground" function
      fnDrawThemeParentBackground   pfn = ( fnDrawThemeParentBackground ) wapi_GetProcAddress( hUxTheme, "DrawThemeParentBackground" );
      if( pfn )
      {
         // Draw the parent window themed background
         nRet = ( HRESULT ) pfn( hWnd, hDC, &pRect );
      }
   }

   // Return TRUE if the operation was successful, otherwise FALSE
   hmg_ret_L( nRet == S_OK );
}

/*
 * FUNCTION: SETWINDOWTHEME
 *
 * This function sets the window theme for a specified window.
 *
 * Parameters:
 *   hWnd: Handle to the window.
 *   pszSubAppName: Sub application name.
 *   pszSubIdList: Sub ID list.
 *
 * Returns:
 *   TRUE if the operation was successful, otherwise FALSE.
 */
HB_FUNC( SETWINDOWTHEME )
{
   HRESULT  nRet = S_FALSE;
   HWND     hWnd = hmg_par_raw_HWND( 1 );
#ifdef UNICODE
   LPCWSTR  pszSubAppName = ( LPCWSTR ) hb_parc( 2 );
   LPCWSTR  pszSubIdList = ( LPCWSTR ) hb_parc( 3 );
#else
   // Convert the provided multibyte string to a wide string (Unicode) if it is not empty
   LPCWSTR  pszSubAppName = ( hb_parclen( 2 ) == 0 ? ( LPCWSTR ) hb_parc( 2 ) : hb_mbtowc( hb_parc( 2 ) ) );
   LPCWSTR  pszSubIdList = ( hb_parclen( 3 ) == 0 ? ( LPCWSTR ) hb_parc( 3 ) : hb_mbtowc( hb_parc( 3 ) ) );
#endif

   // Load the library if not already loaded
   if( hUxTheme == NULL )
   {
      hUxTheme = LoadLibraryEx( TEXT( "uxtheme.dll" ), NULL, 0 );
   }

   if( hUxTheme )
   {
      // Get the address of "SetWindowTheme" function
      fnSetWindowTheme  pfn = ( fnSetWindowTheme ) wapi_GetProcAddress( hUxTheme, "SetWindowTheme" );
      if( pfn )
      {
         // Set the window theme
         nRet = ( HRESULT ) pfn( hWnd, pszSubAppName, pszSubIdList );
      }
   }

   // Return TRUE if the operation was successful, otherwise FALSE
   hmg_ret_L( nRet == S_OK );
}

/*
 * FUNCTION: ENABLETHEMEDIALOGTEXTURE
 *
 * This function enables themed dialog texture for a specified window.
 *
 * Parameters:
 *   hWnd: Handle to the window.
 *   flags: Flags for enabling the texture.
 *
 * Returns:
 *   TRUE if the operation was successful, otherwise FALSE.
 */
HB_FUNC( ENABLETHEMEDIALOGTEXTURE )
{
   HRESULT  nRet = S_FALSE;
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   DWORD    flags = hmg_par_DWORD( 2 );

   // Load the library if not already loaded
   if( hUxTheme == NULL )
   {
      hUxTheme = LoadLibraryEx( TEXT( "uxtheme.dll" ), NULL, 0 );
   }

   if( hUxTheme )
   {
      // Get the address of "EnableThemeDialogTexture" function
      fnEnableThemeDialogTexture pfn = ( fnEnableThemeDialogTexture ) wapi_GetProcAddress( hUxTheme, "EnableThemeDialogTexture" );
      if( pfn )
      {
         // Enable themed dialog texture
         nRet = ( HRESULT ) pfn( hWnd, flags );
      }
   }

   // Return TRUE if the operation was successful, otherwise FALSE
   hmg_ret_L( nRet == S_OK );
}

/*
 * FUNCTION: PTINRECT
 *
 * This function checks if a point is inside a specified rectangle.
 *
 * Parameters:
 *   aPoint: Array containing the point coordinates.
 *   aRect: Array containing the rectangle coordinates.
 *
 * Returns:
 *   TRUE if the point is inside the rectangle, otherwise FALSE.
 */
HB_FUNC( PTINRECT )
{
   POINT point;
   RECT  rect;

   // Convert array parameters to POINT and RECT structures
   if( Array2Point( hb_param( 1, HB_IT_ANY ), &point ) && Array2Rect( hb_param( 2, HB_IT_ANY ), &rect ) )
   {
      // Check if the point is within the rectangle
      hmg_ret_L( PtInRect( &rect, point ) );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

/*
 * FUNCTION: Array2Rect
 *
 * This function converts an array to a RECT structure.
 *
 * Parameters:
 *   aRect: Array containing the rectangle coordinates.
 *   rc: Pointer to the RECT structure to fill.
 *
 * Returns:
 *   TRUE if the conversion was successful, otherwise FALSE.
 */
BOOL Array2Rect( PHB_ITEM aRect, RECT *rc )
{
   // Check if the input is an array with 4 elements
   if( HB_IS_ARRAY( aRect ) && hb_arrayLen( aRect ) == 4 )
   {
      // Fill the RECT structure with values from the array
      rc->left = hb_arrayGetNI( aRect, 1 );
      rc->top = hb_arrayGetNI( aRect, 2 );
      rc->right = hb_arrayGetNI( aRect, 3 );
      rc->bottom = hb_arrayGetNI( aRect, 4 );

      return TRUE;
   }

   return FALSE;
}

/*
 * FUNCTION: Array2Point
 *
 * This function converts an array to a POINT structure.
 *
 * Parameters:
 *   aPoint: Array containing the point coordinates.
 *   pt: Pointer to the POINT structure to fill.
 *
 * Returns:
 *   TRUE if the conversion was successful, otherwise FALSE.
 */
BOOL Array2Point( PHB_ITEM aPoint, POINT *pt )
{
   // Check if the input is an array with 2 elements
   if( HB_IS_ARRAY( aPoint ) && hb_arrayLen( aPoint ) == 2 )
   {
      // Fill the POINT structure with values from the array
      pt->x = hb_arrayGetNI( aPoint, 1 );
      pt->y = hb_arrayGetNI( aPoint, 2 );

      return TRUE;
   }

   return FALSE;
}

/*
 * FUNCTION: Array2ColorRef
 *
 * This function converts an array to a COLORREF.
 *
 * Parameters:
 *   aCRef: Array containing the color components.
 *   cr: Pointer to the COLORREF to fill.
 *
 * Returns:
 *   TRUE if the conversion was successful, otherwise FALSE.
 */
BOOL Array2ColorRef( PHB_ITEM aCRef, COLORREF *cr )
{
   // Check if the input is an array with 3 elements
   if( HB_IS_ARRAY( aCRef ) && hb_arrayLen( aCRef ) == 3 )
   {
      // Extract the color components from the array
      USHORT   r, g, b;

      r = ( USHORT ) HB_arrayGetNL( aCRef, 1 );
      g = ( USHORT ) HB_arrayGetNL( aCRef, 2 );
      b = ( USHORT ) HB_arrayGetNL( aCRef, 3 );

      // Create the COLORREF from the color components
      *cr = RGB( r, g, b );

      return TRUE;
   }

   return FALSE;
}

/*
 * FUNCTION: Rect2Array
 *
 * This function converts a RECT structure to an array.
 *
 * Parameters:
 *   rc: Pointer to the RECT structure to convert.
 *
 * Returns:
 *   An array containing the rectangle coordinates.
 */
HB_EXPORT PHB_ITEM Rect2Array( RECT *rc )
{
   // Create a new array with 4 elements
   PHB_ITEM aRect = hb_itemArrayNew( 4 );

   // Fill the array with values from the RECT structure
   HB_arraySetNL( aRect, 1, rc->left );
   HB_arraySetNL( aRect, 2, rc->top );
   HB_arraySetNL( aRect, 3, rc->right );
   HB_arraySetNL( aRect, 4, rc->bottom );

   return aRect;
}

/*
 * FUNCTION: Point2Array
 *
 * This function converts a POINT structure to an array.
 *
 * Parameters:
 *   pt: Pointer to the POINT structure to convert.
 *
 * Returns:
 *   An array containing the point coordinates.
 */
HB_EXPORT PHB_ITEM Point2Array( POINT *pt )
{
   // Create a new array with 2 elements
   PHB_ITEM aPoint = hb_itemArrayNew( 2 );

   // Fill the array with values from the POINT structure
   HB_arraySetNL( aPoint, 1, pt->x );
   HB_arraySetNL( aPoint, 2, pt->y );

   return aPoint;
}
