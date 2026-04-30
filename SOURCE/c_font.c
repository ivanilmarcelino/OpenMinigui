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
 * Harbour 3.0+ Unicode Compatibility Layer
 * 
 * Reasoning:
 * Different versions of Harbour handle string copying differently in Unicode 
 * environments. This block ensures a consistent internal API (HB_STRNCPY) 
 * regardless of the underlying Harbour version or build type.
 */
#if !defined( __XHARBOUR__ ) && ( __HARBOUR__ - 0 > 0x030000 )
#include "hbwinuni.h"
#else
#define HB_STRNCPY   hb_strncpy
#endif
#ifdef UNICODE

/* 
 * String Conversion Prototypes
 * Used to bridge Harbour's internal string representation with Windows Wide-character APIs.
 */
LPWSTR         AnsiToWide( LPCSTR );
LPSTR          WideToAnsi( LPWSTR );
#endif

/* 
 * HMG Resource Tracker
 * 
 * Purpose:
 * Registers GDI objects (like fonts) into HMG's internal cleanup system.
 * This prevents GDI handle leaks by ensuring objects are deleted when 
 * the application or the owning window is destroyed.
 */
void           RegisterResource( HANDLE hResource, LPCSTR szType );

/* Forward declaration for the font enumeration callback used by the Windows API */
int CALLBACK   EnumFontFamExProc( ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme, DWORD FontType, LPARAM lParam );

/*
 * PrepareFont (Internal Helper)
 * 
 * Purpose:
 * Creates a Win32 HFONT handle with proper DPI scaling.
 * 
 * Parameters:
 * - FontName: Typeface name (e.g., "Arial").
 * - FontSize: Desired size in points.
 * - Weight: Win32 weight constant (e.g., FW_BOLD).
 * - Italic, Underline, StrikeOut: Boolean style flags.
 * - Angle: Rotation in tenths of degrees (e.g., 450 = 45 degrees).
 * - charset: Windows character set identifier.
 * 
 * Reasoning:
 * Windows uses logical units for font height. To maintain visual consistency 
 * across different screen resolutions (DPI), we scale the point size using 
 * LOGPIXELSY. We use a negative FontSize to tell Windows to match the 
 * character height rather than the cell height.
 */
HFONT PrepareFont( TCHAR *FontName, int FontSize, int Weight, DWORD Italic, DWORD Underline, DWORD StrikeOut, DWORD Angle, DWORD charset )
{
   HDC   hDC = GetDC( HWND_DESKTOP );

   // Calculate logical height: (Points * DPI) / 72
   FontSize = -MulDiv( FontSize, GetDeviceCaps( hDC, LOGPIXELSY ), 72 );
   ReleaseDC( HWND_DESKTOP, hDC );

   // Create the font using high-quality TrueType precision
   return CreateFont( FontSize, 0, Angle, Angle, Weight, Italic, Underline, StrikeOut, charset, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_DONTCARE, FontName );
}

/*
 * INITFONT
 * 
 * Purpose:
 * Harbour-level function to create a font object.
 * 
 * Parameters:
 *    1: FontName (C)
 *    2: Size (N)
 *    3: Bold (L)
 *    4: Italic (L)
 *    5: Underline (L)
 *    6: StrikeOut (L)
 *    7: Angle (N)
 *    8: Charset (N, optional)
 * 
 * Returns:
 * A numeric handle (HFONT) to the created font.
 */
HB_FUNC( INITFONT )
{
   // Map Harbour logical parameters to Win32 constants
   int   bold = hb_parl( 3 ) ? FW_BOLD : FW_NORMAL;
   DWORD italic = ( DWORD ) hb_parl( 4 );
   DWORD underline = ( DWORD ) hb_parl( 5 );
   DWORD strikeout = ( DWORD ) hb_parl( 6 );
   DWORD angle = hb_parnl( 7 );
   DWORD charset = hb_parnldef( 8, DEFAULT_CHARSET );
   HFONT hFont;

#ifdef UNICODE
   // Convert Harbour string to WideChar for Unicode builds
   LPWSTR   pStr = AnsiToWide( hb_parc( 1 ) );
   hFont = PrepareFont( ( TCHAR * ) pStr, hb_parni( 2 ), bold, italic, underline, strikeout, angle, charset );
   hb_xfree( pStr );
#else
   hFont = PrepareFont( ( TCHAR * ) hb_parc( 1 ), hb_parni( 2 ), bold, italic, underline, strikeout, angle, charset );
#endif

   // Register the font for automatic garbage collection/cleanup
   RegisterResource( hFont, "FONT" );
   hmg_ret_raw_HANDLE( hFont );
}

/*
 * _SETFONT
 * 
 * Purpose:
 * Creates a new font and immediately applies it to a specific UI control.
 * 
 * Parameters:
 *    1: hWnd (H) - Handle to the window or control.
 *    2: FontName (C)
 *    3: Size (N)
 *    4: Bold (L)
 *    ... (Other font attributes)
 * 
 * Returns:
 *    The new HFONT handle.
 * 
 * Side Effects:
 * Updates the control's visual state and triggers a repaint.
 */
HB_FUNC( _SETFONT )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   int   bold = hb_parl( 4 ) ? FW_BOLD : FW_NORMAL;
   DWORD italic = ( DWORD ) hb_parl( 5 );
   DWORD underline = ( DWORD ) hb_parl( 6 );
   DWORD strikeout = ( DWORD ) hb_parl( 7 );
   DWORD angle = hb_parnl( 8 );
   DWORD charset = hb_parnldef( 9, DEFAULT_CHARSET );
   HFONT hFont;

   // Validate window handle to prevent application crashes
   if( !IsWindow( hwnd ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

#ifdef UNICODE
   {
      LPWSTR   pStr = AnsiToWide( hb_parc( 2 ) );
      hFont = PrepareFont( ( TCHAR * ) pStr, hb_parni( 3 ), bold, italic, underline, strikeout, angle, charset );
      hb_xfree( pStr );
   }

#else
   hFont = PrepareFont( ( TCHAR * ) hb_parc( 2 ), hb_parni( 3 ), bold, italic, underline, strikeout, angle, charset );
#endif

   // Apply font to control; TRUE forces an immediate redraw
   SetWindowFont( hwnd, hFont, TRUE );
   RegisterResource( hFont, "FONT" );
   hmg_ret_raw_HANDLE( hFont );
}

/*
 * _SETFONTHANDLE
 * 
 * Purpose:
 * Assigns an existing HFONT handle to a control.
 * 
 * Reasoning:
 * This is more efficient than _SETFONT when multiple controls share the same font, 
 * as it avoids redundant GDI object creation.
 */
HB_FUNC( _SETFONTHANDLE )
{
   HWND     hwnd = hmg_par_raw_HWND( 1 );
   HGDIOBJ  hGdi = hmg_par_raw_HGDIOBJ( 2 );

   if( !IsWindow( hwnd ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   // Verify the handle is actually a font before assignment
   if( GetObjectType( hGdi ) == OBJ_FONT )
   {
      SetWindowFont( hwnd, ( HFONT ) hGdi, TRUE );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 5050 + OBJ_FONT, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/*
 * GETSYSTEMFONT
 * 
 * Purpose:
 * Retrieves the default font used by the operating system for UI elements.
 * 
 * Returns:
 * An array { cFontName, nPointSize }.
 */
HB_FUNC( GETSYSTEMFONT )
{
   NONCLIENTMETRICS  ncm;
   LOGFONT           lf;
   HDC               hdc = GetDC( NULL );
   int               logPixY = GetDeviceCaps( hdc, LOGPIXELSY );
   ReleaseDC( NULL, hdc );

   // Initialize structure size for API compatibility
   ncm.cbSize = sizeof( ncm );

   // Retrieve system-wide UI metrics (MessageFont is the standard UI font)
   SystemParametersInfo( SPI_GETNONCLIENTMETRICS, ncm.cbSize, &ncm, 0 );
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

   // Convert logical pixels back to point size for the Harbour return value
   HB_STORNI( MulDiv( -lf.lfHeight, 72, logPixY ), -1, 2 );
}

/*
 * ENUMFONTSEX
 * 
 * Purpose:
 * Lists all available fonts matching specific criteria.
 * 
 * Parameters:
 *    1: hDC (H, optional) - Device context to query.
 *    2: FamilyName (C, optional) - Filter by family.
 *    3: Charset (N, optional) - Filter by character set.
 *    4: Pitch (N, optional) - Filter by pitch.
 *    5: FontType (N, optional) - Filter by type.
 *    6: SortBlock (B, optional) - Harbour codeblock for custom sorting.
 *    7: @aNames (A, optional) - Array passed by reference to receive names.
 * 
 * Returns:
 * A nested array containing font details (Name, Charset, Pitch, IsTrueType).
 */
HB_FUNC( ENUMFONTSEX )
{
   HDC      hdc;
   LOGFONT  lf = { 0 };
   PHB_ITEM pArray = hb_itemArrayNew( 0 );
   BOOL     bReleaseDC = FALSE;

   // Determine which Device Context to query (provided or screen default)
   if( GetObjectType( hmg_par_raw_HGDIOBJ( 1 ) ) == OBJ_DC )
   {
      hdc = hmg_par_raw_HDC( 1 );
   }
   else
   {
      hdc = GetDC( NULL );
      bReleaseDC = TRUE;
   }

   // Configure search filters in the LOGFONT structure
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

   // Execute the Windows enumeration API
   EnumFontFamiliesEx( hdc, &lf, ( FONTENUMPROC ) EnumFontFamExProc, ( LPARAM ) pArray, ( DWORD ) 0 );

   if( bReleaseDC )
   {
      ReleaseDC( NULL, hdc );
   }

   // Optional: Sort the resulting array using a Harbour codeblock
   if( HB_ISBLOCK( 6 ) )
   {
      hb_arraySort( pArray, NULL, NULL, hb_param( 6, HB_IT_BLOCK ) );
   }

   // Optional: Populate a reference array with just the font names
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
 * EnumFontFamExProc (Internal Callback)
 * 
 * Purpose:
 * Processes each font found by the Windows EnumFontFamiliesEx function.
 * 
 * Logic:
 * Filters out vertical fonts (prefixed with '@') as they are specialized 
 * for Asian vertical text and usually not desired in standard UI lists.
 */
int CALLBACK EnumFontFamExProc( ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme, DWORD FontType, LPARAM lParam )
{
   HB_SYMBOL_UNUSED( lpntme );

   // Ignore vertical-oriented fonts
   if( lpelfe->elfLogFont.lfFaceName[0] != '@' )
   {
      PHB_ITEM pSub = hb_itemArrayNew( 4 );

#ifdef UNICODE
      LPSTR    pStr = WideToAnsi( lpelfe->elfLogFont.lfFaceName );
      hb_arraySetC( pSub, 1, pStr );
      hb_xfree( pStr );
#else
      hb_arraySetC( pSub, 1, lpelfe->elfLogFont.lfFaceName );
#endif

      // Store metadata for the Harbour array
      hb_arraySetNL( pSub, 2, lpelfe->elfLogFont.lfCharSet );
      hb_arraySetNI( pSub, 3, lpelfe->elfLogFont.lfPitchAndFamily & FIXED_PITCH );
      hb_arraySetNI( pSub, 4, FontType & TRUETYPE_FONTTYPE );

      // Append this font's info to the master list
      hb_arrayAddForward( ( PHB_ITEM ) lParam, pSub );
      hb_itemRelease( pSub );
   }

   // Return 1 to continue enumeration to the next font
   return 1;
}
