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

#define _WIN32_IE 0x0501
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0500

#include <mgdefs.h>
#include <commdlg.h>
#include <shlobj.h>
#include <commctrl.h>
#include "hbapiitm.h"

#ifdef UNICODE
LPWSTR   AnsiToWide( LPCSTR );
LPSTR    WideToAnsi( LPWSTR );
#endif

/*
 * HB_FUNC( CHOOSEFONT )
 * ---------------------
 * Purpose: Invokes the standard Windows Font Selection dialog.
 * 
 * Parameters:
 *    1: Face Name (String)
 *    2: Point Size (Numeric)
 *    3: Bold (Logical)
 *    4: Italic (Logical)
 *    5: Color (Numeric/COLORREF)
 *    6: Underline (Logical)
 *    7: Strikeout (Logical)
 *    8: Charset (Numeric, Optional)
 *    9: Flags (Numeric, Optional)
 *
 * Returns: 
 *    An array of 8 elements containing the selected font properties, 
 *    or an array of empty/zero values if the user cancels.
 */
HB_FUNC( CHOOSEFONT )
{
   CHOOSEFONT  cf;
   LOGFONT     lf;
   long        PointSize;

   // Retrieve the handle of the currently active window to act as the dialog owner.
   HWND        hwnd = GetActiveWindow();
   HDC         hdc = GetDC( hwnd );

   // Get the vertical DPI to correctly calculate font height from point size.
   int         dpiY = GetDeviceCaps( hdc, LOGPIXELSY );
   ReleaseDC( hwnd, hdc );

   // Initialize the LOGFONT structure.
   ZeroMemory( &lf, sizeof( lf ) );

   // Initialize the CHOOSEFONT structure.
   ZeroMemory( &cf, sizeof( cf ) );

#ifdef UNICODE
   {
      LPWSTR   pWStr = AnsiToWide( hb_parc( 1 ) );
      lstrcpyW( lf.lfFaceName, pWStr );
      hb_xfree( pWStr );
   }
#else
   lstrcpy( lf.lfFaceName, hb_parc( 1 ) );
#endif

   // Convert point size to logical units (pixels).
   // Formula: Height = -(PointSize * DPI / 72)
   lf.lfHeight = -MulDiv( hb_parnl( 2 ), dpiY, 72 );
   lf.lfWeight = hb_parl( 3 ) ? FW_BOLD : FW_NORMAL;
   lf.lfItalic = ( BYTE ) hb_parl( 4 );
   lf.lfUnderline = ( BYTE ) hb_parl( 6 );
   lf.lfStrikeOut = ( BYTE ) hb_parl( 7 );

   // Default to system charset if not specified.
   lf.lfCharSet = HB_ISNIL( 8 ) ? ( BYTE ) DEFAULT_CHARSET : hmg_par_BYTE( 8 );

   cf.lStructSize = sizeof( CHOOSEFONT );
   cf.hwndOwner = hwnd;
   cf.lpLogFont = &lf;

   // Use provided flags or default to standard screen fonts with effects.
   cf.Flags = ( HB_ISNUM( 9 ) ? hb_parni( 9 ) : 0 ) | CF_SCREENFONTS | CF_EFFECTS | CF_INITTOLOGFONTSTRUCT;
   cf.rgbColors = HB_ISNIL( 5 ) ? RGB( 0, 0, 0 ) : hmg_par_COLORREF( 5 );

   // Display the dialog. If canceled, return an array of default/empty values.
   if( !ChooseFont( &cf ) )
   {
      hb_reta( 8 );
      HB_STORC( "", -1, 1 );
      HB_STORVNL( 0, -1, 2 );
      HB_STORL( 0, -1, 3 );
      HB_STORL( 0, -1, 4 );
      HB_STORVNL( 0, -1, 5 );
      HB_STORL( 0, -1, 6 );
      HB_STORL( 0, -1, 7 );
      HB_STORNI( 0, -1, 8 );
      return;
   }

   // Convert logical height back to point size for the return value.
   PointSize = -MulDiv( lf.lfHeight, 72, dpiY );

   // Populate the return array with the user's selections.
   hb_reta( 8 );
#ifdef UNICODE
   {
      LPSTR pStr = WideToAnsi( lf.lfFaceName );
      HB_STORC( pStr, -1, 1 );
      hb_xfree( pStr );
   }

#else
   HB_STORC( lf.lfFaceName, -1, 1 );
#endif
   HB_STORVNL( ( LONG ) PointSize, -1, 2 );
   HB_STORL( lf.lfWeight >= FW_BOLD, -1, 3 );
   HB_STORL( lf.lfItalic, -1, 4 );
   HB_STORVNL( cf.rgbColors, -1, 5 );
   HB_STORL( lf.lfUnderline, -1, 6 );
   HB_STORL( lf.lfStrikeOut, -1, 7 );
   HB_STORNI( lf.lfCharSet, -1, 8 );
}

// Static buffer to store the window title for the browse callback.
static TCHAR   s_szWinName[MAX_PATH + 1];

/*
 * BrowseCallbackProc
 * ------------------
 * Internal callback for SHBrowseForFolder.
 * Handles dialog initialization and selection changes.
 */
int CALLBACK BrowseCallbackProc( HWND hWnd, UINT uMsg, LPARAM lParam, LPARAM lpData )
{
   TCHAR szPath[MAX_PATH] = { 0 };

   switch( uMsg )
   {
      case BFFM_INITIALIZED:
         // When the dialog is ready, set the initial directory selection if provided.
         if( lpData )
         {
            SendMessage( hWnd, BFFM_SETSELECTION, TRUE, lpData );

            // Restore the window title if a custom one was cached.
            SetWindowText( hWnd, s_szWinName );
         }
         break;

      case BFFM_VALIDATEFAILED:
         // Provide audio feedback if the user enters an invalid path.
         MessageBeep( MB_ICONHAND );
         return 1;

      case BFFM_SELCHANGED:
         // Update the status text area with the currently highlighted path.
         if( lpData && SHGetPathFromIDList( ( LPITEMIDLIST ) lParam, szPath ) )
         {
            SendMessage( hWnd, BFFM_SETSTATUSTEXT, 0, ( LPARAM ) szPath );
         }
         break;
   }

   return 0;
}

/*
 * HB_FUNC( C_BROWSEFORFOLDER )
 * ----------------------------
 * Purpose: Displays the Windows Shell "Browse for Folder" dialog.
 *
 * Parameters:
 *    1: Parent Window Handle (Numeric)
 *    2: Dialog Title (String)
 *    3: Flags (Numeric)
 *    4: Root Folder CSIDL (Numeric, e.g., CSIDL_DRIVES)
 *    5: Initial Path (String)
 *
 * Returns: Selected path as a string, or empty string if canceled.
 */
HB_FUNC( C_BROWSEFORFOLDER )
{
   HWND           hwnd = HB_ISNIL( 1 ) ? GetActiveWindow() : hmg_par_raw_HWND( 1 );
   BROWSEINFO     bi = { 0 };
   TCHAR          lpBuffer[MAX_PATH] = { 0 };
   LPITEMIDLIST   pidlRoot = NULL, pidlResult;
   BOOL           bHasInitPath = HB_ISCHAR( 5 );

#ifdef UNICODE
   LPWSTR         pWTitle = AnsiToWide( hb_parc( 2 ) );
   LPWSTR         pWInitPath = AnsiToWide( hb_parc( 5 ) );
#else
   LPCSTR         pWTitle = hb_parc( 2 );
   LPCSTR         pWInitPath = hb_parc( 5 );
#endif

   // Cache the current window text if an initial path is used,
   // as the callback might need to reset the dialog title.
   if( bHasInitPath )
   {
      GetWindowText( hwnd, s_szWinName, MAX_PATH );
   }

   // Determine the root of the folder tree (e.g., Desktop, My Computer).
   SHGetSpecialFolderLocation( hwnd, HB_ISNIL( 4 ) ? CSIDL_DRIVES : hb_parni( 4 ), &pidlRoot );

   bi.hwndOwner = hwnd;
   bi.pidlRoot = pidlRoot;
   bi.pszDisplayName = lpBuffer;
   bi.lpszTitle = HB_ISNIL( 2 ) ? TEXT( "Select a Folder" ) : pWTitle;

   // Combine user flags with mandatory flags if an initial path is provided.
   bi.ulFlags = hb_parni( 3 ) | ( bHasInitPath ? BIF_STATUSTEXT | BIF_RETURNONLYFSDIRS : 0 );
   bi.lpfn = BrowseCallbackProc;
   bi.lParam = bHasInitPath ? ( LPARAM ) pWInitPath : 0;

   // Execute the shell dialog.
   pidlResult = SHBrowseForFolder( &bi );

   if( pidlResult && SHGetPathFromIDList( pidlResult, lpBuffer ) )
   {
#ifdef UNICODE
      LPSTR pStr = hb_osStrU16Decode( lpBuffer );
      hb_retc( pStr ? pStr : "" );
      if( pStr )
      {
         hb_xfree( pStr );
      }

#else
      hb_retc( lpBuffer );
#endif

      // Free the PIDL allocated by the shell.
      CoTaskMemFree( pidlResult );
   }
   else
   {
      hb_retc( "" );
   }

   // Clean up the root PIDL.
   if( pidlRoot )
   {
      CoTaskMemFree( pidlRoot );
   }

#ifdef UNICODE
   if( pWTitle )
   {
      hb_xfree( pWTitle );
   }

   if( bHasInitPath && pWInitPath )
   {
      hb_xfree( pWInitPath );
   }
#endif
}

#define CUSTOM_COLOR_COUNT 16

/*
 * HB_FUNC( CHOOSECOLOR )
 * ----------------------
 * Purpose: Invokes the standard Windows Color Selection dialog.
 *
 * Parameters:
 *    1: Parent Window Handle (Numeric)
 *    2: Initial Color (Numeric/COLORREF)
 *    3: Custom Colors Array (Array of 16 RGB arrays, Optional, can be passed by reference)
 *    4: Flags (Numeric, Optional)
 *
 * Returns: Selected COLORREF (Numeric) or -1 if canceled.
 * Side Effects: Updates the custom colors array if passed by reference.
 */
HB_FUNC( CHOOSECOLOR )
{
   CHOOSECOLOR cc;
   COLORREF    crCustClr[CUSTOM_COLOR_COUNT];
   HWND        hwndOwner = HB_ISNIL( 1 ) ? GetActiveWindow() : hmg_par_raw_HWND( 1 );
   int         i;

   // Initialize the 16 custom color slots.
   // If no array is provided, use the system button face color as a default.
   for( i = 0; i < CUSTOM_COLOR_COUNT; i++ )
   {
      crCustClr[i] = HB_ISARRAY( 3 ) ? hmg_parv_COLORREF( 3, i + 1 ) : GetSysColor( COLOR_BTNFACE );
   }

   memset( &cc, 0, sizeof( cc ) );
   cc.lStructSize = sizeof( CHOOSECOLOR );
   cc.hwndOwner = hwndOwner;
   cc.rgbResult = hmg_par_COLORREF( 2 );
   cc.lpCustColors = crCustClr;

   // Default flags: allow any color, start fully expanded, and use the initial RGB.
   cc.Flags = HB_ISNIL( 4 ) ? CC_ANYCOLOR | CC_FULLOPEN | CC_RGBINIT : hmg_par_DWORD( 4 );

   if( ChooseColor( &cc ) )
   {
      hmg_ret_COLORREF( cc.rgbResult );

      // If the custom colors array was passed by reference (@),
      // update it so the user's custom palette persists.
      if( HB_ISBYREF( 3 ) && HB_ISARRAY( 3 ) )
      {
         PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );
         PHB_ITEM pSub = hb_itemNew( NULL );
         hb_arrayNew( pArray, CUSTOM_COLOR_COUNT );
         for( i = 0; i < CUSTOM_COLOR_COUNT; i++ )
         {
            hb_arrayNew( pSub, 3 );
            hb_arraySetNL( pSub, 1, GetRValue( crCustClr[i] ) );
            hb_arraySetNL( pSub, 2, GetGValue( crCustClr[i] ) );
            hb_arraySetNL( pSub, 3, GetBValue( crCustClr[i] ) );
            hb_arraySet( pArray, i + 1, pSub );
         }

         hb_itemRelease( pSub );
      }
   }
   else
   {
      hb_retni( -1 );
   }
}

/*
 * HB_FUNC( UNITSTOPIXELSX )
 * -------------------------
 * Purpose: Converts horizontal Dialog Units (DLUs) to pixels.
 * Reasoning: DLUs are based on the average width of the system font. 
 *            Horizontal pixels = (DLUs * baseUnitX) / 4.
 */
HB_FUNC( UNITSTOPIXELSX )
{
   hb_retni( MulDiv( hb_parni( 1 ), LOWORD( GetDialogBaseUnits() ), 4 ) );
}

/*
 * HB_FUNC( UNITSTOPIXELSY )
 * -------------------------
 * Purpose: Converts vertical Dialog Units (DLUs) to pixels.
 * Reasoning: Vertical pixels = (DLUs * baseUnitY) / 8.
 */
HB_FUNC( UNITSTOPIXELSY )
{
   hb_retni( MulDiv( hb_parni( 1 ), HIWORD( GetDialogBaseUnits() ), 8 ) );
}
