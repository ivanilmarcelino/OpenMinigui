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

// Function to free resources associated with graphical objects or handles
void pascal DelResource( HANDLE hResource );

// Compatibility for older Harbour versions with function translation
#ifndef HMG_LEGACY_OFF
#if !defined( __MINGW32__ ) && !defined( __XHARBOUR__ ) && ( __HARBOUR__ - 0 > 0x020000 ) && ( __HARBOUR__ - 0 < 0x030200 )
HB_FUNC_TRANSLATE( HB_SETCODEPAGE, HB_CDPSELECT )  // Translate code page selection function
#endif
#endif

/*
 *  HB_FUNC( MAKELONG )
 *
 *  Combines two 16-bit integers into a single 32-bit LONG value.
 *
 *  Parameters:
 *      1: The low-order word (16 bits) of the LONG value.
 *      2: The high-order word (16 bits) of the LONG value.
 *
 *  Returns:
 *      A LONG value constructed from the two input integers.
 *
 *  Purpose:
 *      This function is used to create a LONG value from two separate integer values,
 *      which is often required when interacting with Windows API functions that expect
 *      a combined value.
 */
HB_FUNC( MAKELONG )
{
   hmg_ret_LONG( MAKELONG( hb_parni( 1 ), hb_parni( 2 ) ) );   // Return combined LONG value
}

/*
 *  HB_FUNC( _ENABLESCROLLBARS )
 *
 *  Enables or disables the scroll bars of a window.
 *
 *  Parameters:
 *      1: hWnd - The handle of the window whose scroll bars are to be modified.
 *      2: wSBflags - Specifies the type of scroll bars to enable or disable.  Can be SB_HORZ, SB_VERT, or SB_BOTH.
 *      3: wArrows - Specifies whether the scroll arrows should be shown or hidden. Can be ESB_ENABLE_BOTH, ESB_DISABLE_LTUP, or ESB_DISABLE_RTDN.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function allows you to dynamically control the visibility and functionality of
 *      a window's scroll bars, which is useful for adapting the user interface based on
 *      the content being displayed.
 */
HB_FUNC( _ENABLESCROLLBARS )
{
   EnableScrollBar( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 *  HB_FUNC( DELETEOBJECT )
 *
 *  Deletes a graphics object (e.g., a pen, brush, font, bitmap) and frees any system resources
 *  associated with the object.
 *
 *  Parameters:
 *      1: hResource - The handle of the graphics object to be deleted.
 *
 *  Returns:
 *      .T. (TRUE) if the object was successfully deleted; otherwise, .F. (FALSE).
 *
 *  Purpose:
 *      This function is essential for managing memory and preventing resource leaks when
 *      working with graphics objects in Windows.  It ensures that the system resources
 *      allocated to the object are released when the object is no longer needed.
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
 *  Destroys an image list and frees all memory associated with it.
 *
 *  Parameters:
 *      1: himl - The handle of the image list to be destroyed.
 *
 *  Returns:
 *      .T. (TRUE) if the image list was successfully destroyed; otherwise, .F. (FALSE).
 *
 *  Purpose:
 *      Image lists are used to store collections of images for use in controls such as
 *      list views and tree views.  This function is used to release the memory occupied
 *      by the image list when it is no longer needed.
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
 *  Sets the keyboard focus to the specified window.  The window with the keyboard focus
 *  receives all keyboard input.
 *
 *  Parameters:
 *      1: hWnd - The handle of the window to which the keyboard focus is to be set.
 *
 *  Returns:
 *      The handle of the window that previously had the keyboard focus, or NULL if there was no previous focus window.
 *
 *  Purpose:
 *      This function is used to programmatically control which window receives keyboard input.
 *      It is often used to ensure that the correct control is active when the user interacts
 *      with the application.
 */
HB_FUNC( SETFOCUS )
{
   hmg_ret_raw_HWND( SetFocus( hmg_par_raw_HWND( 1 ) ) );
}

/*
 *  HB_FUNC( INSERTSHIFTTAB )
 *
 *  Simulates a Shift+Tab key press, which typically moves the keyboard focus to the previous
 *  control in a dialog or form.
 *
 *  Parameters:
 *      None.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function provides a way to programmatically simulate the Shift+Tab key press,
 *      which can be useful for implementing custom navigation behavior in an application.
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
 *  Retrieves or sets the value of a system-wide parameter. This function can be used to
 *  customize the appearance and behavior of Windows.
 *
 *  Parameters:
 *      1: uiAction - The system parameter to be retrieved or set.  See the Windows API documentation for a list of valid values.
 *      2: uiParam - A parameter whose meaning depends on the uiAction parameter.
 *      3: pvParam - A pointer to a buffer that receives the value of the system parameter, or contains the new value to be set.
 *      4: fWinIni - Specifies whether to update the user profile in the WIN.INI file.
 *
 *  Returns:
 *      .T. (TRUE) if the function succeeds; otherwise, .F. (FALSE).
 *
 *  Purpose:
 *      This function provides access to a wide range of system settings, allowing applications
 *      to adapt to the user's preferences and environment.
 */
HB_FUNC( SYSTEMPARAMETERSINFO )
{
   hb_retl( SystemParametersInfoA( hmg_par_UINT( 1 ), hmg_par_UINT( 2 ), ( VOID * ) hb_parc( 3 ), hmg_par_UINT( 4 ) ) );
}

/*
 *  HB_FUNC( GETTEXTWIDTH )
 *
 *  Calculates the width, in pixels, of a specified string when drawn using a particular font
 *  in a given device context.
 *
 *  Parameters:
 *      1: hDC - The handle of the device context (HDC) in which the text will be drawn. If NULL, a DC will be created and destroyed.
 *      2: lpString - The string whose width is to be calculated.
 *      3: hFont - The handle of the font (HFONT) to be used to draw the text. If NULL, the current DC font is used.
 *
 *  Returns:
 *      The width of the string, in pixels.
 *
 *  Purpose:
 *      This function is used to determine the amount of space required to display a string
 *      using a specific font, which is essential for layout and formatting purposes.
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
      bDestroyDC = TRUE;
      hWnd = GetActiveWindow();
      hDC = GetDC( hWnd ); // Acquire device context if not provided
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
   hb_xfree( ( TCHAR * ) lpString );
#endif
}

/*
 *  HB_FUNC( KEYBD_EVENT )
 *
 *  Synthesizes a keystroke. The system can use such a synthesized keystroke to trigger a keyboard event.
 *
 *  Parameters:
 *      1: bVk - A virtual-key code. The code must be a value in the range 1 to 254.
 *      2: bScan - A hardware scan code for the key.
 *      3: dwFlags - Controls various aspects of function operation. This parameter can be certain combinations of the following values:
 *                   KEYEVENTF_EXTENDEDKEY   0x0001  If specified, the scan code was preceded by a prefix byte that has the value 0xE0 (224).
 *                   KEYEVENTF_KEYUP         0x0002  If specified, the key is being released. If not specified, the key is being pressed.
 *                   KEYEVENTF_SCANCODE      0x0008  If specified, wScan identifies the key and bVk is ignored.
 *                   KEYEVENTF_UNICODE       0x0004  Windows 2000/XP: If specified, the system synthesizes a WM_UNICODE character event. The wScan parameter must contain a Unicode character code.
 *      4: dwExtraInfo - An application-defined value that is associated with the key stroke.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function is used to simulate keyboard input, which can be useful for automating tasks
 *      or for testing purposes.
 */
HB_FUNC( KEYBD_EVENT )
{
   keybd_event( hmg_par_BYTE( 1 ), ( BYTE ) MapVirtualKey( hmg_par_UINT( 1 ), 0 ), hb_parl( 2 ) ? KEYEVENTF_KEYUP : 0, 0 );
}

/*
 *  HB_FUNC( INSERTVKEY )
 *
 *  Inserts a single virtual key press event into the input stream.
 *
 *  Parameters:
 *      1: bVk - The virtual-key code of the key to be pressed.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function provides a simple way to simulate a single key press, which can be useful
 *      for triggering specific actions or commands within an application.
 */
HB_FUNC( INSERTVKEY )
{
   keybd_event( hmg_par_BYTE( 1 ), 0, 0, 0 );
}

/*
 *  HB_FUNC( _HMG_SETVSCROLLVALUE )
 *
 *  Sets the position of the vertical scroll bar in a window.
 *
 *  Parameters:
 *      1: hWnd - The handle of the window whose scroll bar is to be set.
 *      2: nPos - The new position of the scroll box.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function allows you to programmatically control the position of the vertical scroll
 *      bar, which is useful for synchronizing the scroll bar with the content being displayed.
 */
HB_FUNC( _HMG_SETVSCROLLVALUE )
{
   SendMessage( hmg_par_raw_HWND( 1 ), WM_VSCROLL, MAKEWPARAM( SB_THUMBPOSITION, hb_parni( 2 ) ), 0 );
}

/*
 *  HB_FUNC( _HMG_SETHSCROLLVALUE )
 *
 *  Sets the position of the horizontal scroll bar in a window.
 *
 *  Parameters:
 *      1: hWnd - The handle of the window whose scroll bar is to be set.
 *      2: nPos - The new position of the scroll box.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function allows you to programmatically control the position of the horizontal scroll
 *      bar, which is useful for synchronizing the scroll bar with the content being displayed.
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
 *      1: hWnd - The handle of the window in which the caret is to be displayed.
 *
 *  Returns:
 *      .T. (TRUE) if the caret was successfully displayed; otherwise, .F. (FALSE).
 *
 *  Purpose:
 *      This function makes the caret visible, allowing the user to see where text will be inserted.
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
 *      1: hWnd - The handle of the window in which the caret is to be hidden.
 *
 *  Returns:
 *      .T. (TRUE) if the caret was successfully hidden; otherwise, .F. (FALSE).
 *
 *  Purpose:
 *      This function makes the caret invisible, preventing it from being displayed in the window.
 */
HB_FUNC( HIDECARET )
{
   hb_retl( HideCaret( hmg_par_raw_HWND( 1 ) ) );
}

/*
 *  HB_FUNC( DESTROYCARET )
 *
 *  Destroys the current caret and releases any system resources associated with it.
 *
 *  Parameters:
 *      None.
 *
 *  Returns:
 *      .T. (TRUE) if the caret was successfully destroyed; otherwise, .F. (FALSE).
 *
 *  Purpose:
 *      This function should be called when the caret is no longer needed to free up system resources.
 */
HB_FUNC( DESTROYCARET )
{
   hb_retl( DestroyCaret() );
}

/*
 *  HB_FUNC( CREATECARET )
 *
 *  Creates a new caret with the specified dimensions and bitmap (optional).
 *
 *  Parameters:
 *      1: hWnd - The handle of the window in which the caret is to be created.
 *      2: hBitmap - The handle of the bitmap to be used for the caret. If NULL, a solid block caret is created.
 *      3: nWidth - The width of the caret, in logical units.
 *      4: nHeight - The height of the caret, in logical units.
 *
 *  Returns:
 *      .T. (TRUE) if the caret was successfully created; otherwise, .F. (FALSE).
 *
 *  Purpose:
 *      This function allows you to create a custom caret with specific dimensions and appearance.
 */
HB_FUNC( CREATECARET )
{
   hb_retl( CreateCaret( hmg_par_raw_HWND( 1 ), hmg_par_raw_HBITMAP( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
}

/*
 *  HB_FUNC( CHANGESTYLE )
 *
 *  Modifies the style of a window by adding and removing style bits.
 *
 *  Parameters:
 *      1: hWnd - The handle of the window whose style is to be modified.
 *      2: dwAdd - The style bits to be added to the window's style.
 *      3: dwRemove - The style bits to be removed from the window's style.
 *      4: lExStyle - A logical value indicating whether to modify the extended style (TRUE) or the normal style (FALSE).
 *
 *  Returns:
 *      The previous style of the window.
 *
 *  Purpose:
 *      This function allows you to dynamically change the appearance and behavior of a window
 *      by modifying its style bits.  This can be useful for enabling or disabling features,
 *      changing the window's appearance, or responding to user actions.
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
 *  Repositions and resizes a text box and associated buttons to fit within a specified area.
 *
 *  Parameters:
 *      1: hEdit - The handle of the text box window.
 *      2: hBtn1 - The handle of the first button window.
 *      3: hBtn2 - The handle of the second button window (optional).
 *      4: fBtn2 - A logical value indicating whether a second button is present (TRUE) or not (FALSE).
 *      5: BtnWidth - The desired width of the buttons.
 *      6: width - The total width of the area to be occupied by the text box and buttons.
 *      7: height - The height of the area to be occupied by the text box and buttons.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function is used to arrange a text box and its associated buttons in a consistent
 *      and visually appealing manner.  It ensures that the controls are properly sized and
 *      positioned, regardless of the size of the available area.
 */
HB_FUNC( MOVEBTNTEXTBOX )
{
   HWND  hedit = hmg_par_raw_HWND( 1 );
   HWND  hBtn1 = hmg_par_raw_HWND( 2 );
   HWND  hBtn2 = hmg_par_raw_HWND( 3 );
   BOOL  fBtn2 = hb_parl( 4 );   // Flag indicating if a second button is used
   int   BtnWidth = ( int ) hb_parni( 5 );
   int   BtnWidth2;
   int   width = ( int ) hb_parni( 6 );
   int   height = ( int ) hb_parni( 7 );
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
 *  Creates a Harbour date value from year, month, and day integers.
 *
 *  Parameters:
 *      1: nYear - The year (e.g., 2023).
 *      2: nMonth - The month (1-12).
 *      3: nDay - The day of the month (1-31).
 *
 *  Returns:
 *      A Harbour date value representing the specified date.
 *
 *  Purpose:
 *      This function provides a way to create Harbour date values from individual year, month,
 *      and day components.  It is used for compatibility with older Harbour versions.
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
 *  Performs a case-insensitive comparison of the left portion of two strings.
 *
 *  Parameters:
 *      1: pItem1 - The first string to compare.
 *      2: pItem2 - The second string to compare.
 *
 *  Returns:
 *      .T. (TRUE) if the left portion of the first string is equal to the second string, ignoring case;
 *      otherwise, .F. (FALSE).
 *
 *  Purpose:
 *      This function is used to perform a case-insensitive comparison of strings, which is useful
 *      for tasks such as searching and filtering data. It is used for compatibility with older Harbour versions.
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
