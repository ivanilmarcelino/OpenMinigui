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
*/
#include <mgdefs.h>
#include <windowsx.h>
#include "hbapiitm.h"
#include "hbapierr.h"

/* 
   Compatibility layer for Harbour/xHarbour string handling.
   Ensures that the correct string copy function is used based on the compiler version.
*/
#if !defined( __XHARBOUR__ ) && ( __HARBOUR__ - 0 > 0x030000 )
#include "hbwinuni.h"
#else
#define HB_STRNCPY   hb_strncpy
#endif

/* 
   Forward declarations for Unicode conversion and resource management.
   RegisterResource is an HMG internal function used to track GDI objects 
   to prevent memory leaks by ensuring they are eventually freed.
*/
#ifdef UNICODE
LPWSTR         AnsiToWide( LPCSTR );
LPSTR          WideToAnsi( LPWSTR );
#endif
void           RegisterResource( HANDLE hResource, LPCSTR szType );

int CALLBACK   EnumFontFamExProc( ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme, DWORD FontType, LPARAM lParam );

/*
   _IsValidWindow
   Purpose: Verifies if a given window handle (HWND) is valid within the Win32 subsystem.
   Input: hWnd - The window handle to check.
   Return: BOOL - TRUE if valid, FALSE otherwise.
   Side Effects: Triggers a Harbour Runtime Error if the window is invalid.
   Reasoning: Prevents the library from attempting GDI operations on non-existent controls, 
   which could lead to undefined behavior or silent failures.
*/
static BOOL _IsValidWindow( HWND hWnd )
{
   if( !IsWindow( hWnd ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return FALSE;
   }

   return TRUE;
}

/*
   _CreateFont
   Purpose: Low-level wrapper for the Win32 CreateFont function.
   Input: fontName (TCHAR*), size (int), weight (int), italic/underline/strikeout/angle/charset (DWORD).
   Return: HFONT - Handle to the newly created logical font.
   Reasoning: Point size in HMG is converted to logical units (pixels) using the device context 
   of the desktop to ensure consistent scaling across different DPI settings (MulDiv).
*/
static HFONT _CreateFont( TCHAR *fontName, int size, int weight, DWORD italic, DWORD underline, DWORD strikeout, DWORD angle, DWORD charset )
{
   HDC   hDC = GetDC( HWND_DESKTOP );

   // Convert point size to logical units based on vertical DPI
   size = -MulDiv( size, GetDeviceCaps( hDC, LOGPIXELSY ), 72 );
   ReleaseDC( HWND_DESKTOP, hDC );

   return CreateFont
      (
         size,
         0,
         angle,
         angle,
         weight,
         italic,
         underline,
         strikeout,
         charset,
         OUT_TT_PRECIS,
         CLIP_DEFAULT_PRECIS,
         DEFAULT_QUALITY,
         FF_DONTCARE,
         fontName
      );
}

/*
   PrepareFont
   Purpose: Public C-level interface to create a font.
   Note: This is a direct pass-through to the internal _CreateFont helper.
*/
HFONT PrepareFont( TCHAR *FontName, int FontSize, int Weight, DWORD Italic, DWORD Underline, DWORD StrikeOut, DWORD Angle, DWORD charset )
{
   return _CreateFont( FontName, FontSize, Weight, Italic, Underline, StrikeOut, Angle, charset );
}

/*
   _PrepareFontFromParams
   Purpose: Extracts font attributes from Harbour parameters and creates the font.
   Input: iName (index of name param), iSize (index of size param), and other attributes.
   Return: HFONT handle.
   Reasoning: Centralizes the logic for parameter extraction and Unicode/Ansi conversion 
   before calling the GDI creation logic.
*/
static HFONT _PrepareFontFromParams( int iName, int iSize, int weight, DWORD italic, DWORD underline, DWORD strikeout, DWORD angle, DWORD charset )
{
#ifdef UNICODE
   // Convert Harbour's ANSI string to Wide for Unicode builds
   LPWSTR   pStr = AnsiToWide( hb_parc( iName ) );
   HFONT    hFont = _CreateFont( ( TCHAR * ) pStr, hb_parni( iSize ), weight, italic, underline, strikeout, angle, charset );
   hb_xfree( pStr );
#else
   HFONT hFont = _CreateFont( ( TCHAR * ) hb_parc( iName ), hb_parni( iSize ), weight, italic, underline, strikeout, angle, charset );
#endif
   return hFont;
}

/*
   HB_FUNC( INITFONT )
   Purpose: Harbour-level function to create a font object.
   Parameters: 
      1: Font Name (String)
      2: Font Size (Numeric)
      3: Bold (Logical)
      4: Italic (Logical)
      5: Underline (Logical)
      6: Strikeout (Logical)
      7: Angle (Numeric)
      8: Charset (Numeric, Optional)
   Return: Handle to the created font.
   Side Effects: Registers the font handle in HMG's resource tracker.
*/
HB_FUNC( INITFONT )
{
   int   weight = hb_parl( 3 ) ? FW_BOLD : FW_NORMAL;
   DWORD italic = ( DWORD ) hb_parl( 4 );
   DWORD underline = ( DWORD ) hb_parl( 5 );
   DWORD strikeout = ( DWORD ) hb_parl( 6 );
   DWORD angle = hb_parnl( 7 );
   DWORD charset = hb_parnldef( 8, DEFAULT_CHARSET );

   HFONT hFont = _PrepareFontFromParams( 1, 2, weight, italic, underline, strikeout, angle, charset );

   // Registering ensures HMG cleans up this GDI object when the application closes
   RegisterResource( hFont, "FONT" );
   hmg_ret_raw_HANDLE( hFont );
}

/*
   HB_FUNC( _SETFONT )
   Purpose: Creates a font and immediately applies it to a specific GUI control.
   Parameters:
      1: Control Handle (HWND)
      2-9: Font attributes (Name, Size, Bold, etc.)
   Return: Handle to the created font.
   Side Effects: Updates the UI of the target control.
*/
HB_FUNC( _SETFONT )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( !_IsValidWindow( hwnd ) )
   {
      return;
   }
   {
      int   weight = hb_parl( 4 ) ? FW_BOLD : FW_NORMAL;
      DWORD italic = ( DWORD ) hb_parl( 5 );
      DWORD underline = ( DWORD ) hb_parl( 6 );
      DWORD strikeout = ( DWORD ) hb_parl( 7 );
      DWORD angle = hb_parnl( 8 );
      DWORD charset = hb_parnldef( 9, DEFAULT_CHARSET );

      HFONT hFont = _PrepareFontFromParams( 2, 3, weight, italic, underline, strikeout, angle, charset );

      // Apply the font to the window using the standard Win32 WM_SETFONT message
      SetWindowFont( hwnd, hFont, TRUE );
      RegisterResource( hFont, "FONT" );

      hmg_ret_raw_HANDLE( hFont );
   }
}

/*
   HB_FUNC( _SETFONTHANDLE )
   Purpose: Applies an existing font handle to a control.
   Parameters:
      1: Control Handle (HWND)
      2: Font Handle (HFONT/HGDIOBJ)
   Reasoning: Used when a font has already been created (e.g., via INITFONT) 
   and needs to be assigned to a control without creating a duplicate GDI object.
*/
HB_FUNC( _SETFONTHANDLE )
{
   HWND     hwnd = hmg_par_raw_HWND( 1 );
   HGDIOBJ  hGdi = hmg_par_raw_HGDIOBJ( 2 );

   if( !_IsValidWindow( hwnd ) )
   {
      return;
   }

   // Safety check: Ensure the provided GDI handle is actually a font
   if( GetObjectType( hGdi ) == OBJ_FONT )
   {
      SetWindowFont( hwnd, ( HFONT ) hGdi, TRUE );
   }
   else
   {
      // Raise error if the handle type is incorrect
      hb_errRT_BASE_SubstR( EG_ARG, 5050 + OBJ_FONT, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/*
   HB_FUNC( GETSYSTEMFONT )
   Purpose: Retrieves the default system font used for message boxes and dialogs.
   Return: Array { FontName, FontSize }.
   Reasoning: Allows developers to match the application's UI with the user's 
   operating system theme settings.
*/
HB_FUNC( GETSYSTEMFONT )
{
   NONCLIENTMETRICS  ncm;
   LOGFONT           lf;

   // Get screen DPI for height-to-point conversion
   HDC               hdc = GetDC( NULL );
   int               logPixY = GetDeviceCaps( hdc, LOGPIXELSY );
   ReleaseDC( NULL, hdc );

   ncm.cbSize = sizeof( ncm );

   // Retrieve system-wide UI metrics
   SystemParametersInfo( SPI_GETNONCLIENTMETRICS, ncm.cbSize, &ncm, 0 );

   // We use the MessageFont as the standard reference for system fonts
   lf = ncm.lfMessageFont;

   hb_reta( 2 );

#ifndef UNICODE
   HB_STORC( lf.lfFaceName, -1, 1 );
#else
   {
      LPSTR pStr = WideToAnsi( lf.lfFaceName );
      HB_STORC( pStr, -1, 1 );
      hb_xfree( pStr );
   }
#endif

   // Convert logical height back to point size for Harbour usage
   HB_STORNI( MulDiv( -lf.lfHeight, 72, logPixY ), -1, 2 );
}

/*
   HB_FUNC( ENUMFONTSEX )
   Purpose: Enumerates available system fonts based on criteria.
   Parameters:
      1: Device Context (HDC, Optional)
      2: Face Name (String, Optional filter)
      3: Charset (Numeric, Optional)
      4: Pitch and Family (Numeric, Optional)
      6: Sort Block (Codeblock, Optional)
      7: Names Array (ByRef, Optional)
   Return: Array of arrays containing font details.
   Reasoning: Provides a way to populate font selection dialogs or validate font availability.
*/
HB_FUNC( ENUMFONTSEX )
{
   HDC      hdc;
   LOGFONT  lf = { 0 };
   PHB_ITEM pArray = hb_itemArrayNew( 0 );
   BOOL     bReleaseDC = FALSE;

   // Determine which DC to use for enumeration
   if( GetObjectType( hmg_par_raw_HGDIOBJ( 1 ) ) == OBJ_DC )
   {
      hdc = hmg_par_raw_HDC( 1 );
   }
   else
   {
      hdc = GetDC( NULL );
      bReleaseDC = TRUE;
   }

   // Set up the filter criteria in the LOGFONT structure
   if( hb_parclen( 2 ) > 0 )
   {
      HB_STRNCPY( lf.lfFaceName, ( LPCTSTR ) hb_parc( 2 ), HB_MIN( LF_FACESIZE - 1, hb_parclen( 2 ) ) );
   }

   lf.lfCharSet = HB_ISNUM( 3 ) ? ( BYTE ) hb_parni( 3 ) : ( BYTE ) GetTextCharset( hdc );
   if( lf.lfCharSet == DEFAULT_CHARSET )
   {
      lf.lfCharSet = ( BYTE ) GetTextCharset( hdc );
   }

   lf.lfPitchAndFamily = HB_ISNUM( 4 ) ? ( BYTE ) hb_parni( 4 ) : ( BYTE ) 0;
   if( lf.lfPitchAndFamily == DEFAULT_PITCH )
   {
      lf.lfPitchAndFamily = 0;
   }

   // Execute the Win32 enumeration with our callback
   EnumFontFamiliesEx( hdc, &lf, ( FONTENUMPROC ) EnumFontFamExProc, ( LPARAM ) pArray, 0 );

   if( bReleaseDC )
   {
      ReleaseDC( NULL, hdc );
   }

   // Optional: Sort the resulting array using a Harbour codeblock
   if( HB_ISBLOCK( 6 ) )
   {
      hb_arraySort( pArray, NULL, NULL, hb_param( 6, HB_IT_BLOCK ) );
   }

   // Optional: If the 7th parameter is passed by reference, fill it with just the font names
   if( HB_ISBYREF( 7 ) )
   {
      PHB_ITEM aNames = hb_param( 7, HB_IT_ANY );
      HB_SIZE  nLen = hb_arrayLen( pArray ), i;

      hb_arrayNew( aNames, nLen );

      for( i = 1; i <= nLen; i++ )
      {
         hb_arraySetC( aNames, i, hb_arrayGetC( hb_arrayGetItemPtr( pArray, i ), 1 ) );
      }
   }

   hb_itemReturnRelease( pArray );
}

/*
   EnumFontFamExProc
   Purpose: Internal callback function invoked by Windows for each font found during enumeration.
   Logic: 
      - Filters out vertical fonts (those starting with '@').
      - Packages font metadata (Name, Charset, Pitch, Type) into a Harbour array.
   Return: 1 to continue enumeration, 0 to stop.
*/
int CALLBACK EnumFontFamExProc( ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme, DWORD FontType, LPARAM lParam )
{
   HB_SYMBOL_UNUSED( lpntme );

   // Skip vertical fonts as they are typically not desired in standard UI selection
   if( lpelfe->elfLogFont.lfFaceName[0] == '@' )
   {
      return 1;
   }
   {
      PHB_ITEM pSub = hb_itemArrayNew( 4 );

#ifdef UNICODE
      LPSTR    pStr = WideToAnsi( lpelfe->elfLogFont.lfFaceName );
      hb_arraySetC( pSub, 1, pStr );
      hb_xfree( pStr );
#else
      hb_arraySetC( pSub, 1, lpelfe->elfLogFont.lfFaceName );
#endif
      hb_arraySetNL( pSub, 2, lpelfe->elfLogFont.lfCharSet );
      hb_arraySetNI( pSub, 3, lpelfe->elfLogFont.lfPitchAndFamily & FIXED_PITCH );
      hb_arraySetNI( pSub, 4, FontType & TRUETYPE_FONTTYPE );

      // Add the font info sub-array to the main collection passed via lParam
      hb_arrayAddForward( ( PHB_ITEM ) lParam, pSub );
      hb_itemRelease( pSub );

      return 1;
   }
}
