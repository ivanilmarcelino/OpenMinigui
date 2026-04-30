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

// Set compatibility for Windows/Internet Explorer features at version 5.01
// This ensures access to common control features available since IE 5.01.
#define _WIN32_IE 0x0501

#include <mgdefs.h>
#include <commctrl.h>

#ifdef UNICODE
// Converts ANSI strings to Wide Character (UTF-16) for Unicode builds.
LPWSTR      AnsiToWide( LPCSTR );
#endif

// Internal HMG function to remove a handle from the internal resource tracking system.
void pascal DelResource( HANDLE hResource );

/* ------------------------------------------------------------------------ */
/* Compatibility Layer                                                      */
/* ------------------------------------------------------------------------ */

#ifndef HMG_LEGACY_OFF
/* 
 * Translates HB_SETCODEPAGE to HB_CDPSELECT for specific Harbour versions 
 * to maintain backward compatibility with older codebases.
 */
#if !defined( __MINGW32__ ) && !defined( __XHARBOUR__ ) && ( __HARBOUR__ - 0 > 0x020000 ) && ( __HARBOUR__ - 0 < 0x030200 )
HB_FUNC_TRANSLATE( HB_SETCODEPAGE, HB_CDPSELECT )
#endif
#endif

/* ------------------------------------------------------------------------ */
/* Utility Wrappers                                                         */
/* ------------------------------------------------------------------------ */

/*
 * HB_FUNC( MAKELONG )
 * Purpose: Combines two 16-bit integers into a single 32-bit unsigned integer.
 * Parameters: 1 (Low-order word), 2 (High-order word).
 * Returns: A 32-bit LONG value.
 * Usage: Often used to pack coordinates or flags into a single message parameter (LPARAM).
 */
HB_FUNC( MAKELONG )
{
   hmg_ret_LONG( MAKELONG( hb_parni( 1 ), hb_parni( 2 ) ) );
}

/*
 * HB_FUNC( _ENABLESCROLLBARS )
 * Purpose: Enables or disables one or both arrows of a scroll bar.
 * Parameters: 
 *    1 - Window Handle (HWND).
 *    2 - Flags (SB_HORZ, SB_VERT, or SB_BOTH).
 *    3 - Arrows (ESB_ENABLE_BOTH, ESB_DISABLE_LTUP, etc.).
 * Side Effects: Updates the UI state of the specified window's scrollbars.
 */
HB_FUNC( _ENABLESCROLLBARS )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   int   nFlags = hb_parni( 2 );
   int   nArrows = hb_parni( 3 );

   if( hWnd )
   {
      EnableScrollBar( hWnd, nFlags, nArrows );
   }
}

/*
 * HB_FUNC( DELETEOBJECT )
 * Purpose: Deletes a logical pen, brush, font, bitmap, region, or palette.
 * Parameters: 1 - Handle to the GDI object (HGDIOBJ).
 * Returns: Logical (.T. if successful).
 * Side Effects: Frees system resources. 
 * Note: Calls DelResource to ensure HMG's internal tracker is synchronized.
 */
HB_FUNC( DELETEOBJECT )
{
   HANDLE   hRes = hmg_par_raw_HANDLE( 1 );

   if( !hRes )
   {
      hb_retl( HB_FALSE );
      return;
   }

   // Remove from HMG internal resource management before system deletion.
   DelResource( hRes );
   hb_retl( DeleteObject( ( HGDIOBJ ) hRes ) );
}

/*
 * HB_FUNC( IMAGELIST_DESTROY )
 * Purpose: Destroys an image list and removes it from memory.
 * Parameters: 1 - Handle to the ImageList (HIMAGELIST).
 * Returns: Logical (.T. if successful).
 * Note: Essential for preventing memory leaks when dynamic image lists are used.
 */
HB_FUNC( IMAGELIST_DESTROY )
{
   HIMAGELIST  hImg = hmg_par_raw_HIMAGELIST( 1 );

   if( !hImg )
   {
      hb_retl( HB_FALSE );
      return;
   }

   // Synchronize with HMG resource tracker.
   DelResource( hImg );
   hb_retl( ImageList_Destroy( hImg ) );
}

/*
 * HB_FUNC( SETFOCUS )
 * Purpose: Sets the keyboard focus to the specified window.
 * Parameters: 1 - Window Handle (HWND).
 * Returns: The handle of the window that previously had the focus.
 */
HB_FUNC( SETFOCUS )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   if( IsWindow( hWnd ) )
   {
      hmg_ret_raw_HWND( SetFocus( hWnd ) );
   }
   else
   {
      hmg_ret_raw_HANDLE( NULL );   // Return null if window is invalid
   }
}

/*
 * HB_FUNC( INSERTSHIFTTAB )
 * Purpose: Simulates a Shift+Tab key combination.
 * Side Effects: Triggers backward tab navigation in the UI.
 * Reasoning: Used to programmatically move focus to the previous control.
 */
HB_FUNC( INSERTSHIFTTAB )
{
   keybd_event( VK_SHIFT, 0, 0, 0 );
   keybd_event( VK_TAB, 0, 0, 0 );
   keybd_event( VK_SHIFT, 0, KEYEVENTF_KEYUP, 0 );
}

/*
 * HB_FUNC( SYSTEMPARAMETERSINFO )
 * Purpose: Retrieves or sets system-wide parameters (accessibility, desktop, etc.).
 * Parameters: 
 *    1 - Action (UINT), 2 - Parameter 1 (UINT), 
 *    3 - Parameter 2 (Pointer/String), 4 - Update Flags (UINT).
 * Returns: Logical (.T. if successful).
 */
HB_FUNC( SYSTEMPARAMETERSINFO )
{
   UINT  uiAction = hmg_par_UINT( 1 );
   UINT  uiParam = hmg_par_UINT( 2 );
   VOID  *pParam = ( VOID * ) hb_parc( 3 );
   UINT  uiFlags = hmg_par_UINT( 4 );

   hb_retl( SystemParametersInfo( uiAction, uiParam, pParam, uiFlags ) );
}

/*
 * HB_FUNC( GETTEXTWIDTH )
 * Purpose: Calculates the width of a string in pixels based on a specific font.
 * Parameters: 
 *    1 - Device Context (HDC, optional).
 *    2 - Text string.
 *    3 - Font Handle (HFONT, optional).
 * Returns: Width in pixels (LONG).
 * Logic: If no HDC is provided, it uses the active window's DC. It temporarily 
 * selects the font into the DC to ensure accurate measurement.
 */
HB_FUNC( GETTEXTWIDTH )
{
   HDC      hDC = hmg_par_raw_HDC( 1 );
   HWND     hWnd = NULL;
   BOOL     bOwnDC = FALSE;
   HFONT    hFont = hmg_par_raw_HFONT( 3 );
   HFONT    hOld = NULL;
   SIZE     sz;

#ifndef UNICODE
   LPCSTR   lpText = hb_parc( 2 );
#else
   LPCWSTR  lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif

   // If no DC is provided, obtain the DC of the currently active window.
   if( !hDC )
   {
      hWnd = GetActiveWindow();
      hDC = GetDC( hWnd );
      bOwnDC = TRUE;
   }

   // Select the custom font if provided, storing the old one for restoration.
   if( hFont )
   {
      hOld = ( HFONT ) SelectObject( hDC, hFont );
   }

   GetTextExtentPoint32( hDC, lpText, ( int ) lstrlen( lpText ), &sz );

   // Restore the original font to prevent GDI leaks.
   if( hFont && hOld )
   {
      SelectObject( hDC, hOld );
   }

   // Release the DC if we created it locally.
   if( bOwnDC )
   {
      ReleaseDC( hWnd, hDC );
   }

   hmg_ret_LONG( sz.cx );

#ifdef UNICODE
   if( lpText )
   {
      hb_xfree( ( TCHAR * ) lpText );
   }
#endif
}

/*
 * HB_FUNC( KEYBD_EVENT )
 * Purpose: Synthesizes a keystroke.
 * Parameters: 
 *    1 - Virtual Key Code (BYTE).
 *    2 - Key Up flag (Logical: .T. for release, .F. for press).
 * Logic: Automatically maps the virtual key to a hardware scan code.
 */
HB_FUNC( KEYBD_EVENT )
{
   BYTE  bVk = hmg_par_BYTE( 1 );
   BOOL  bUp = hb_parl( 2 );
   BYTE  scan = ( BYTE ) MapVirtualKey( bVk, 0 );

   keybd_event( bVk, scan, bUp ? KEYEVENTF_KEYUP : 0, 0 );
}

/*
 * HB_FUNC( INSERTVKEY )
 * Purpose: Simulates a single key press (down event).
 * Parameters: 1 - Virtual Key Code (BYTE).
 */
HB_FUNC( INSERTVKEY )
{
   keybd_event( hmg_par_BYTE( 1 ), 0, 0, 0 );
}

/*
 * HB_FUNC( _HMG_SETVSCROLLVALUE )
 * Purpose: Programmatically sets the vertical scroll position of a window.
 * Parameters: 1 - Window Handle (HWND), 2 - Position (Numeric).
 * Logic: Sends a WM_VSCROLL message with SB_THUMBPOSITION to force the update.
 */
HB_FUNC( _HMG_SETVSCROLLVALUE )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   int   nPos = hb_parni( 2 );

   if( hWnd )
   {
      SendMessage( hWnd, WM_VSCROLL, MAKEWPARAM( SB_THUMBPOSITION, nPos ), 0 );
   }
}

/*
 * HB_FUNC( _HMG_SETHSCROLLVALUE )
 * Purpose: Programmatically sets the horizontal scroll position of a window.
 * Parameters: 1 - Window Handle (HWND), 2 - Position (Numeric).
 */
HB_FUNC( _HMG_SETHSCROLLVALUE )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   int   nPos = hb_parni( 2 );

   if( hWnd )
   {
      SendMessage( hWnd, WM_HSCROLL, MAKEWPARAM( SB_THUMBPOSITION, nPos ), 0 );
   }
}

/*
 * Caret Management Functions
 * Purpose: Control the visibility and creation of the text insertion point (caret).
 */

HB_FUNC( SHOWCARET )
{
   hb_retl( ShowCaret( hmg_par_raw_HWND( 1 ) ) );
}

HB_FUNC( HIDECARET )
{
   hb_retl( HideCaret( hmg_par_raw_HWND( 1 ) ) );
}

HB_FUNC( DESTROYCARET )
{
   hb_retl( DestroyCaret() );
}

HB_FUNC( CREATECARET )
{
   hb_retl( CreateCaret( hmg_par_raw_HWND( 1 ), hmg_par_raw_HBITMAP( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
}

/*
 * HB_FUNC( CHANGESTYLE )
 * Purpose: Dynamically adds or removes window styles (GWL_STYLE or GWL_EXSTYLE).
 * Parameters: 
 *    1 - Window Handle (HWND).
 *    2 - Styles to add (LONG_PTR).
 *    3 - Styles to remove (LONG_PTR).
 *    4 - Extended Style flag (Logical: .T. for EXSTYLE, .F. for STYLE).
 * Returns: The previous style value.
 * Logic: Uses SetWindowLongPtr for compatibility. Calls SetWindowPos with 
 * SWP_FRAMECHANGED to force Windows to recalculate the non-client area (borders).
 */
HB_FUNC( CHANGESTYLE )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   LONG_PTR dwAdd = hmg_par_raw_LONG_PTR( 2 );
   LONG_PTR dwRemove = hmg_par_raw_LONG_PTR( 3 );
   int      iIndex = hb_parl( 4 ) ? GWL_EXSTYLE : GWL_STYLE;

   LONG_PTR dwOld = GetWindowLongPtr( hWnd, iIndex );
   LONG_PTR dwNew = ( dwOld &~dwRemove ) | dwAdd;

   HB_RETNL( ( LONG_PTR ) SetWindowLongPtr( hWnd, iIndex, dwNew ) );

   // Force the window to refresh its frame to reflect style changes.
   SetWindowPos( hWnd, NULL, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER );
}

/*
 * HB_FUNC( MOVEBTNTEXTBOX )
 * Purpose: Internal HMG helper to position buttons inside or adjacent to a textbox.
 * Parameters:
 *    1 - Edit Control Handle (HWND).
 *    2 - Button 1 Handle (HWND).
 *    3 - Button 2 Handle (HWND).
 *    4 - Button 2 Visibility (Logical).
 *    5 - Button Width (Numeric).
 *    6 - Total Width (Numeric).
 *    7 - Total Height (Numeric).
 * Logic: Calculates the layout for controls like DatePickers or Lookup boxes where 
 * buttons are embedded at the end of the text entry area.
 */
HB_FUNC( MOVEBTNTEXTBOX )
{
   HWND  hEdit = hmg_par_raw_HWND( 1 );
   HWND  hBtn1 = hmg_par_raw_HWND( 2 );
   HWND  hBtn2 = hmg_par_raw_HWND( 3 );

   BOOL  fBtn2 = hb_parl( 4 );
   int   nBtnW = hb_parni( 5 );
   int   nWidth = hb_parni( 6 );
   int   nHeight = hb_parni( 7 );

   BOOL  fBtns = ( hb_parnl( 2 ) > 0 );
   int   nMinBtn = GetSystemMetrics( SM_CYSIZE ) - 1;
   int   nBtnW2;

   // Ensure button width meets minimum system standards for usability.
   if( nBtnW < nMinBtn )
   {
      nBtnW = nMinBtn;
   }

   if( !fBtns )
   {
      nBtnW = 0;
   }

   nBtnW2 = fBtn2 ? nBtnW : 0;

   // Resize the main edit control.
   SetWindowPos( hEdit, NULL, 0, 0, nWidth, nHeight, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOZORDER );

   // Position the buttons at the right edge of the textbox.
   if( fBtns )
   {
      SetWindowPos( hBtn1, NULL, nWidth - nBtnW - 4, -1, nBtnW, nHeight - 2, SWP_NOACTIVATE | SWP_NOZORDER );

      if( fBtn2 )
      {
         SetWindowPos( hBtn2, NULL, nWidth - nBtnW - nBtnW2 - 4, -1, nBtnW2, nHeight - 2, SWP_NOACTIVATE | SWP_NOZORDER );
      }
   }
}

/* ------------------------------------------------------------------------ */
/* Compatibility (Date / String)                                            */
/* ------------------------------------------------------------------------ */

#if defined( __XHARBOUR__ ) || ( __HARBOUR__ - 0 < 0x030200 )
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbapierr.h"

/*
 * HB_FUNC( HB_DATE )
 * Purpose: Legacy wrapper to return a Date type from Year, Month, Day.
 */
HB_FUNC( HB_DATE )
{
   hb_retd( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

#if !defined( __XHARBOUR__ ) && ( __HARBOUR__ - 0 < 0x030200 )
#define hb_cdppage   hb_vmCDP
#endif

/*
 * HB_FUNC( HB_LEFTEQI )
 * Purpose: Case-insensitive comparison of the start of two strings.
 * Parameters: 1 - String A, 2 - String B.
 * Returns: Logical .T. if String A starts with String B (case-insensitive).
 */
HB_FUNC( HB_LEFTEQI )
{
   PHB_ITEM p1 = hb_param( 1, HB_IT_STRING );
   PHB_ITEM p2 = hb_param( 2, HB_IT_STRING );

   if( p1 && p2 )
   {
      // Performs a case-insensitive comparison using the current code page.
      hb_retl( hb_cdpicmp( hb_itemGetCPtr( p1 ), hb_itemGetCLen( p1 ), hb_itemGetCPtr( p2 ), hb_itemGetCLen( p2 ), hb_cdppage(), HB_FALSE ) == 0 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1071, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}
#endif
