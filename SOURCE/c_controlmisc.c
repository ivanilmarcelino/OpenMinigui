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

// Set compatibility for Windows/Internet Explorer features at version 5.01
#define _WIN32_IE 0x0501

#include <mgdefs.h>
#include <commctrl.h>

#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );                  // Convert ANSI string to Wide string
#endif

void pascal DelResource( HANDLE hResource );       // Frees resources associated with a handle.

// Compatibility for older Harbour versions with function translation
#ifndef HMG_LEGACY_OFF
#if !defined( __MINGW32__ ) && !defined( __XHARBOUR__ ) && ( __HARBOUR__ - 0 > 0x020000 ) && ( __HARBOUR__ - 0 < 0x030200 )
HB_FUNC_TRANSLATE( HB_SETCODEPAGE, HB_CDPSELECT )  // Translate HB_SETCODEPAGE to HB_CDPSELECT for compatibility.
#endif
#endif

/*
 *  HB_FUNC( MAKELONG )
 *
 *  Combines two 16-bit integers into a 32-bit LONG value.
 *
 *  Parameters:
 *      1: <nLowWord> - The low-order word (16 bits).
 *      2: <nHighWord> - The high-order word (16 bits).
 *
 *  Returns:
 *      A LONG value constructed from the two input integers.
 */
HB_FUNC( MAKELONG )
{
   hmg_ret_LONG( MAKELONG( hb_parni( 1 ), hb_parni( 2 ) ) );  // Return combined LONG value
}

/*
 *  HB_FUNC( _ENABLESCROLLBARS )
 *
 *  Enables or disables scroll bars for a window.
 *
 *  Parameters:
 *      1: <hWnd> - The handle of the window.
 *      2: <wSBflags> - Scroll bar flags (SB_HORZ, SB_VERT, SB_BOTH).
 *      3: <wArrows> - Arrow enabling/disabling flags (ESB_ENABLE_BOTH, ESB_DISABLE_LTUP, ESB_DISABLE_RTDN).
 *
 *  Returns:
 *      None.
 */
HB_FUNC( _ENABLESCROLLBARS )
{
   EnableScrollBar( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 *  HB_FUNC( DELETEOBJECT )
 *
 *  Deletes a GDI object (pen, brush, font, bitmap) and frees associated resources.
 *
 *  Parameters:
 *      1: <hResource> - The handle of the GDI object to delete.
 *
 *  Returns:
 *      .T. (TRUE) on success, .F. (FALSE) on failure.
 */
HB_FUNC( DELETEOBJECT )
{
   HANDLE   hRes = hmg_par_raw_HANDLE( 1 );

   if( hRes )
   {
      DelResource( hRes ); // Release associated resources
      hb_retl( DeleteObject( ( HGDIOBJ ) hRes ) ); // Return deletion success
   }
   else
   {
      hb_retl( HB_FALSE );                // Return false if handle is invalid
   }
}

/*
 *  HB_FUNC( IMAGELIST_DESTROY )
 *
 *  Destroys an image list and frees its memory.
 *
 *  Parameters:
 *      1: <himl> - The handle of the image list to destroy.
 *
 *  Returns:
 *      .T. (TRUE) on success, .F. (FALSE) on failure.
 */
HB_FUNC( IMAGELIST_DESTROY )
{
   HIMAGELIST  himl = hmg_par_raw_HIMAGELIST( 1 );

   DelResource( himl );                   // Free resources related to the image list
   hb_retl( ImageList_Destroy( himl ) );  // Return success status of destruction
}

/*
 *  HB_FUNC( SETFOCUS )
 *
 *  Sets the keyboard focus to the specified window.
 *
 *  Parameters:
 *      1: <hWnd> - The handle of the window to receive focus.
 *
 *  Returns:
 *      The handle of the window that previously had focus, or NULL.
 */
HB_FUNC( SETFOCUS )
{
   hmg_ret_raw_HWND( SetFocus( hmg_par_raw_HWND( 1 ) ) );
}

/*
 *  HB_FUNC( INSERTSHIFTTAB )
 *
 *  Simulates a Shift+Tab key press.
 *
 *  Parameters:
 *      None.
 *
 *  Returns:
 *      None.
 */
HB_FUNC( INSERTSHIFTTAB )
{
   keybd_event( VK_SHIFT, 0, 0, 0 );      // Press Shift key
   keybd_event( VK_TAB, 0, 0, 0 );        // Press Tab key
   keybd_event( VK_SHIFT, 0, KEYEVENTF_KEYUP, 0 ); // Release Shift key
}

/*
 *  HB_FUNC( SYSTEMPARAMETERSINFO )
 *
 *  Retrieves or sets system-wide parameters.
 *
 *  Parameters:
 *      1: <uiAction> - The system parameter to query or set (e.g., SPI_GETSCREENSAVEACTIVE).
 *      2: <uiParam> - Parameter specific to the action.
 *      3: <pvParam> - Pointer to a buffer to receive or set the parameter value.
 *      4: <fWinIni> - Flags controlling update behavior (e.g., SPIF_UPDATEINIFILE).
 *
 *  Returns:
 *      .T. (TRUE) on success, .F. (FALSE) on failure.
 */
HB_FUNC( SYSTEMPARAMETERSINFO )
{
   hb_retl( SystemParametersInfo( hmg_par_UINT( 1 ), hmg_par_UINT( 2 ), ( VOID * ) hb_parc( 3 ), hmg_par_UINT( 4 ) ) );
}

/*
 *  HB_FUNC( GETTEXTWIDTH )
 *
 *  Calculates the width of a text string in pixels, using a specified font.
 *
 *  Parameters:
 *      1: <hDC> - The handle of the device context (HDC). If NULL, a DC is created and destroyed.
 *      2: <lpString> - The text string to measure.
 *      3: <hFont> - The handle of the font (HFONT). If NULL, the current DC font is used.
 *
 *  Returns:
 *      The width of the string in pixels.
 */
HB_FUNC( GETTEXTWIDTH )
{
   HDC      hDC = hmg_par_raw_HDC( 1 );            // Get device context
   HWND     hWnd = ( HWND ) NULL;
   BOOL     bDestroyDC = FALSE;
   HFONT    hFont = hmg_par_raw_HFONT( 3 );
   HFONT    hOldFont = ( HFONT ) NULL;
   SIZE     sz;

#ifndef UNICODE
   LPCSTR   lpString = hb_parc( 2 );
#else
   LPCWSTR  lpString = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   if( !hDC )
   {
      hWnd = GetActiveWindow();
      hDC = GetDC( hWnd ); // Acquire device context if not provided
      bDestroyDC = TRUE;
   }

   if( hFont )
   {
      hOldFont = ( HFONT ) SelectObject( hDC, hFont );   // Select specified font if provided
   }

   GetTextExtentPoint32( hDC, lpString, ( int ) lstrlen( lpString ), &sz );   // Calculate text width
   if( hFont )
   {
      SelectObject( hDC, hOldFont );   // Restore original font
   }

   if( bDestroyDC )
   {
      ReleaseDC( hWnd, hDC );          // Release device context if created
   }

   hmg_ret_LONG( sz.cx );              // Return calculated text width
#ifdef UNICODE
   if( lpString )
   {
      hb_xfree( ( TCHAR * ) lpString );
   }
#endif
}

/*
 *  HB_FUNC( KEYBD_EVENT )
 *
 *  Synthesizes a keystroke event.
 *
 *  Parameters:
 *      1: <bVk> - The virtual-key code (e.g., VK_A for 'A').
 *      2: <bScan> - The hardware scan code for the key.
 *      3: <dwFlags> - Flags controlling the event (KEYEVENTF_KEYUP, KEYEVENTF_SCANCODE, etc.).
 *      4: <dwExtraInfo> - Application-defined extra information.
 *
 *  Returns:
 *      None.
 */
HB_FUNC( KEYBD_EVENT )
{
   UINT  scan = MapVirtualKey( hmg_par_UINT( 1 ), 0 );

   keybd_event( hmg_par_BYTE( 1 ), ( BYTE ) scan, hb_parl( 2 ) ? KEYEVENTF_KEYUP : 0, 0 );
}

/*
 *  HB_FUNC( INSERTVKEY )
 *
 *  Inserts a virtual key press event.
 *
 *  Parameters:
 *      1: <bVk> - The virtual-key code of the key to press.
 *
 *  Returns:
 *      None.
 */
HB_FUNC( INSERTVKEY )
{
   keybd_event( hmg_par_BYTE( 1 ), 0, 0, 0 );
}

/*
 *  HB_FUNC( _HMG_SETVSCROLLVALUE )
 *
 *  Sets the vertical scroll bar position.
 *
 *  Parameters:
 *      1: <hWnd> - The handle of the window.
 *      2: <nPos> - The new scroll position.
 *
 *  Returns:
 *      None.
 */
HB_FUNC( _HMG_SETVSCROLLVALUE )
{
   SendMessage( hmg_par_raw_HWND( 1 ), WM_VSCROLL, MAKEWPARAM( SB_THUMBPOSITION, hb_parni( 2 ) ), 0 );
}

/*
 *  HB_FUNC( _HMG_SETHSCROLLVALUE )
 *
 *  Sets the horizontal scroll bar position.
 *
 *  Parameters:
 *      1: <hWnd> - The handle of the window.
 *      2: <nPos> - The new scroll position.
 *
 *  Returns:
 *      None.
 */
HB_FUNC( _HMG_SETHSCROLLVALUE )
{
   SendMessage( hmg_par_raw_HWND( 1 ), WM_HSCROLL, MAKEWPARAM( SB_THUMBPOSITION, hb_parni( 2 ) ), 0 );
}

/*
 *  HB_FUNC( SHOWCARET )
 *
 *  Displays the caret (cursor) in a window.
 *
 *  Parameters:
 *      1: <hWnd> - The handle of the window.
 *
 *  Returns:
 *      .T. (TRUE) on success, .F. (FALSE) on failure.
 */
HB_FUNC( SHOWCARET )
{
   hb_retl( ShowCaret( hmg_par_raw_HWND( 1 ) ) );
}

/*
 *  HB_FUNC( HIDECARET )
 *
 *  Hides the caret (cursor) in a window.
 *
 *  Parameters:
 *      1: <hWnd> - The handle of the window.
 *
 *  Returns:
 *      .T. (TRUE) on success, .F. (FALSE) on failure.
 */
HB_FUNC( HIDECARET )
{
   hb_retl( HideCaret( hmg_par_raw_HWND( 1 ) ) );
}

/*
 *  HB_FUNC( DESTROYCARET )
 *
 *  Destroys the caret.
 *
 *  Parameters:
 *      None.
 *
 *  Returns:
 *      .T. (TRUE) on success, .F. (FALSE) on failure.
 */
HB_FUNC( DESTROYCARET )
{
   hb_retl( DestroyCaret() );
}

/*
 *  HB_FUNC( CREATECARET )
 *
 *  Creates a caret.
 *
 *  Parameters:
 *      1: <hWnd> - The handle of the window.
 *      2: <hBitmap> - The handle of the bitmap for the caret (or NULL for a solid block).
 *      3: <nWidth> - The width of the caret.
 *      4: <nHeight> - The height of the caret.
 *
 *  Returns:
 *      .T. (TRUE) on success, .F. (FALSE) on failure.
 */
HB_FUNC( CREATECARET )
{
   hb_retl( CreateCaret( hmg_par_raw_HWND( 1 ), hmg_par_raw_HBITMAP( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
}

/*
 *  HB_FUNC( CHANGESTYLE )
 *
 *  Changes the style of a window.
 *
 *  Parameters:
 *      1: <hWnd> - The handle of the window.
 *      2: <dwAdd> - Style bits to add.
 *      3: <dwRemove> - Style bits to remove.
 *      4: <lExStyle> - .T. (TRUE) for extended style, .F. (FALSE) for normal style.
 *
 *  Returns:
 *      The previous style of the window.
 */
HB_FUNC( CHANGESTYLE )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   LONG_PTR dwAdd = hmg_par_raw_LONG_PTR( 2 );
   LONG_PTR dwRemove = hmg_par_raw_LONG_PTR( 3 );
   int      iStyle = hb_parl( 4 ) ? GWL_EXSTYLE : GWL_STYLE;               // Determine style type
   LONG_PTR dwStyle, dwNewStyle;

   dwStyle = GetWindowLongPtr( hWnd, iStyle );
   dwNewStyle = ( dwStyle & ( ~dwRemove ) ) | dwAdd;                       // Calculate new style by adding and removing styles
   HB_RETNL( ( LONG_PTR ) SetWindowLongPtr( hWnd, iStyle, dwNewStyle ) );  // Apply new style
   SetWindowPos( hWnd, NULL, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER ); // Update window style
}

/*
 *  HB_FUNC( MOVEBTNTEXTBOX )
 *
 *  Repositions and resizes a text box and associated buttons.
 *
 *  Parameters:
 *      1: <hEdit> - The handle of the text box.
 *      2: <hBtn1> - The handle of the first button.
 *      3: <hBtn2> - The handle of the second button (optional).
 *      4: <fBtn2> - .T. (TRUE) if a second button is present, .F. (FALSE) otherwise.
 *      5: <BtnWidth> - The desired width of the buttons.
 *      6: <width> - The total width of the area.
 *      7: <height> - The total height of the area.
 *
 *  Returns:
 *      None.
 */
HB_FUNC( MOVEBTNTEXTBOX )
{
   HWND  hedit = hmg_par_raw_HWND( 1 );
   HWND  hBtn1 = hmg_par_raw_HWND( 2 );
   HWND  hBtn2 = hmg_par_raw_HWND( 3 );
   BOOL  fBtn2 = hb_parl( 4 );   // Flag indicating if a second button is used
   int   BtnWidth = hb_parni( 5 );
   int   BtnWidth2;
   int   width = hb_parni( 6 );
   int   height = hb_parni( 7 );
   BOOL  fBtns = ( hb_parnl( 2 ) > 0 );

   BtnWidth = ( BtnWidth >= GetSystemMetrics( SM_CYSIZE ) ? BtnWidth : GetSystemMetrics( SM_CYSIZE ) - 1 );          // Minimum button width
   BtnWidth = fBtns ? BtnWidth : 0;
   BtnWidth2 = fBtn2 ? BtnWidth : 0;

   SetWindowPos( hedit, NULL, 0, 0, width, height, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOZORDER );  // Resize text box
   if( fBtns )
   {
      SetWindowPos( hBtn1, NULL, width - BtnWidth - 4, -1, BtnWidth, height - 2, SWP_NOACTIVATE | SWP_NOZORDER );    // Position first button
      if( fBtn2 )
      {
         SetWindowPos( hBtn2, NULL, width - BtnWidth - BtnWidth2 - 4, -1, BtnWidth2, height - 2, SWP_NOACTIVATE | SWP_NOZORDER );   // Position second button if needed
      }
   }
}

// Compatibility functions for Harbour/xHarbour, handling date and string operations
#if defined( __XHARBOUR__ ) || ( __HARBOUR__ - 0 < 0x030200 )
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbapierr.h"

/*
 *  HB_FUNC( HB_DATE )
 *
 *  Creates a Harbour date value from year, month, and day.
 *
 *  Parameters:
 *      1: <nYear> - The year.
 *      2: <nMonth> - The month (1-12).
 *      3: <nDay> - The day (1-31).
 *
 *  Returns:
 *      A Harbour date value.
 */
HB_FUNC( HB_DATE )
{
   hb_retd( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

#if !defined( __XHARBOUR__ ) && ( __HARBOUR__ - 0 < 0x030200 )
#define hb_cdppage   hb_vmCDP // Define code page pointer
#endif

/*
 *  HB_FUNC( HB_LEFTEQI )
 *
 *  Case-insensitive comparison of the left portion of two strings.
 *
 *  Parameters:
 *      1: <pItem1> - The first string.
 *      2: <pItem2> - The second string.
 *
 *  Returns:
 *      .T. (TRUE) if the left portion of the first string equals the second string (case-insensitive), .F. (FALSE) otherwise.
 */
HB_FUNC( HB_LEFTEQI )
{
   PHB_ITEM pItem1 = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pItem2 = hb_param( 2, HB_IT_STRING );

   if( pItem1 && pItem2 )
   {
      hb_retl( hb_cdpicmp( hb_itemGetCPtr( pItem1 ), hb_itemGetCLen( pItem1 ), hb_itemGetCPtr( pItem2 ), hb_itemGetCLen( pItem2 ), hb_cdppage(), HB_FALSE ) == 0 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1071, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );   // Raise error if arguments are invalid
   }
}
#endif
