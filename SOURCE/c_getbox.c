/*----------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   GETBOX Control Source Code
   Copyright 2006 Jacek Kubica <kubica@wssk.wroc.pl>
   http://www.wssk.wroc.pl/~kubica

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
#include <commctrl.h>

#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
// Button Class Name
#define WC_BUTTON "Button"
// Edit Class Name
#define WC_EDIT   "Edit"
#endif

#if defined( _MSC_VER )
#pragma warning( push )
#pragma warning( disable : 4201 )
#endif
#include "richedit.h"
#if defined( _MSC_VER )
#pragma warning( pop )
#endif
#include "hbvm.h"

#define GBB1   2
#define GBB2   3

LRESULT CALLBACK  OwnGetProc( HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam );

#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );
LPSTR             WideToAnsi( LPWSTR );
#endif
HINSTANCE         GetInstance( void );
HINSTANCE         GetResources( void );

/*
 * FUNCTION INITGETBOX()
 *
 * Initializes a GetBox control, which is essentially an extended edit control with optional buttons.
 *
 * Parameters:
 *   1:  Parent Window Handle (HWND): The handle of the parent window where the GetBox will be created.
 *   3:  X Position (INT): The X coordinate of the GetBox control within the parent window.
 *   4:  Y Position (INT): The Y coordinate of the GetBox control within the parent window.
 *   5:  Width (INT): The width of the GetBox control.
 *   6:  Height (INT): The height of the GetBox control.
 *   9:  Max Length (INT): The maximum number of characters allowed in the edit control.
 *   10: UpperCase (LOGICAL): If .T., converts input to uppercase.
 *   11: LowerCase (LOGICAL): If .T., converts input to lowercase.
 *   12: Numeric (LOGICAL): If .T., restricts input to numeric characters.
 *   13: Password (LOGICAL): If .T., displays asterisks instead of characters.
 *   14: Right Align (LOGICAL): If .T., right-aligns the text in the edit control.
 *   15: ReadOnly (LOGICAL): If .T., makes the edit control read-only.
 *   16: Visible (LOGICAL): If .F., makes the edit control initially visible.
 *   17: TabStop (LOGICAL): If .F., removes the edit control from the tab order.
 *   18: Image File 1 (STRING): The filename of the image to display on the first button (optional).
 *   19: Button Width (INT): The width of the buttons (optional, defaults to system size).
 *   20: Has Buttons (LOGICAL): If .T., creates buttons next to the edit control.
 *   21: Image File 2 (STRING): The filename of the image to display on the second button (optional).
 *   22: Has Second Button (LOGICAL): If .T., creates a second button next to the edit control.
 *   23: No Client Edge (LOGICAL): If .T., the edit control will not have a sunken border.
 *
 * Returns:
 *   ARRAY: An array containing the handles of the created controls:
 *          { Edit Control Handle, Button 1 Handle, Button 2 Handle, Image 1 Handle, Image 2 Handle }
 *
 * Purpose:
 *   This function simplifies the creation of a GetBox control, which is a common UI element
 *   used for data input. It encapsulates the creation of the edit control and optional buttons,
 *   handling various styles and properties. The function is used to create a customized input
 *   field with optional buttons for actions like browsing for a file or displaying a calendar.
 *
 * Notes:
 *   - The function uses CreateWindowEx to create the edit control and CreateWindow to create the buttons.
 *   - It uses LoadImage to load the images for the buttons.
 *   - The function stores the handles of the created controls in a Harbour array for later use.
 *   - The function uses SendMessage with EM_SETMARGINS to adjust the margins of the edit control
 *     to accommodate the buttons.
 */
HB_FUNC( INITGETBOX )
{
   HWND  hedit;                  // Handle of the child window/control.
   DWORD iStyle;                 // GETBOX window base style.
   DWORD ibtnStyle1, ibtnStyle2; // BUTTON window base style.
   HWND  himage, himage2;
   HWND  hBtn1, hBtn2;
   BOOL  fBtns, fBtn2;
   int   BtnWidth = 0;
   int   BtnWidth2 = 0;

   fBtns = hb_parl( 20 );
   fBtn2 = hb_parl( 22 );

   if( fBtns )
   {
      BtnWidth = ( HB_ISNIL( 19 ) ? 0 : ( int ) hb_parni( 19 ) );
      BtnWidth = ( BtnWidth >= GetSystemMetrics( SM_CYSIZE ) ? BtnWidth : GetSystemMetrics( SM_CYSIZE ) );
      BtnWidth2 = ( fBtn2 ? BtnWidth : 0 );
   }

   iStyle = WS_CHILD | ES_AUTOHSCROLL | WS_CLIPCHILDREN;

   if( hb_parl( 12 ) )
   {
      // if <lNumeric> is TRUE, then ES_NUMBER style is added.
      iStyle |= ES_NUMBER;
   }

   // Set to a numeric TEXTBOX, so don't worry about other "textual" styles.
   else
   {
      if( hb_parl( 10 ) )
      {
         // if <lUpper> is TRUE, then ES_UPPERCASE style is added.
         iStyle |= ES_UPPERCASE;
      }

      if( hb_parl( 11 ) )
      {
         // if <lLower> is TRUE, then ES_LOWERCASE style is added.
         iStyle |= ES_LOWERCASE;
      }
   }

   if( hb_parl( 13 ) )
   {
      // if <lPassword> is TRUE, then ES_PASSWORD style is added.
      iStyle |= ES_PASSWORD;
   }

   if( hb_parl( 14 ) )
   {
      iStyle |= ES_RIGHT;
   }

   if( hb_parl( 15 ) )
   {
      iStyle |= ES_READONLY;
   }

   if( !hb_parl( 16 ) )
   {
      iStyle |= WS_VISIBLE;
   }

   if( !hb_parl( 17 ) )
   {
      iStyle |= WS_TABSTOP;
   }

   // Creates the child control.
   hedit = CreateWindowEx
      (
         hb_parl( 23 ) ? 0 : WS_EX_CLIENTEDGE,
         WC_EDIT,
         TEXT( "" ),
         iStyle,
         hb_parni( 3 ),
         hb_parni( 4 ),
         hb_parni( 5 ),
         hb_parni( 6 ),
         hmg_par_raw_HWND( 1 ),
         ( HMENU ) NULL,
         GetInstance(),
         NULL
      );

   SetProp( hedit, TEXT( "OldWndProc" ), ( HANDLE ) ( LONG_PTR ) GetWindowLongPtr( hedit, GWLP_WNDPROC ) );
   SubclassWindow2( hedit, OwnGetProc );

   SendMessage( hedit, EM_LIMITTEXT, hmg_par_WPARAM( 9 ), ( LPARAM ) 0 );

   if( hb_parc( 18 ) != NULL )
   {
#ifndef UNICODE
      LPCSTR   lpImageName = hb_parc( 18 );
#else
      LPWSTR   lpImageName = AnsiToWide( ( char * ) hb_parc( 18 ) );
#endif
      himage = ( HWND ) LoadImage( GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );

      if( himage == NULL )
      {
         himage = ( HWND ) LoadImage( NULL, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
      }

      if( himage != NULL )
      {
         BITMAP   bm;
         GetObject( himage, sizeof( BITMAP ), &bm );
         if( bm.bmWidth > BtnWidth - 4 || bm.bmHeight > hb_parni( 6 ) - 5 )
         {
            DeleteObject( himage );
            himage = ( HWND ) LoadImage( GetResources(), lpImageName, IMAGE_BITMAP, BtnWidth - 4, hb_parni( 6 ) - 6, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
            if( himage == NULL )
            {
               himage = ( HWND ) LoadImage
                  (
                     NULL,
                     lpImageName,
                     IMAGE_BITMAP,
                     BtnWidth - 4,
                     hb_parni( 6 ) - 6,
                     LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT
                  );
            }
         }
      }

#ifdef UNICODE
      hb_xfree( lpImageName );
#endif
   }
   else
   {
      himage = NULL;
   }

   if( hb_parc( 21 ) != NULL )
   {
#ifndef UNICODE
      LPCSTR   lpImageName2 = hb_parc( 21 );
#else
      LPWSTR   lpImageName2 = AnsiToWide( ( char * ) hb_parc( 21 ) );
#endif
      himage2 = ( HWND ) LoadImage( GetResources(), lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );

      if( himage2 == NULL )
      {
         himage2 = ( HWND ) LoadImage( NULL, lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
      }

      if( himage2 != NULL )
      {
         BITMAP   bm;
         GetObject( himage2, sizeof( BITMAP ), &bm );
         if( bm.bmWidth > BtnWidth2 - 4 || bm.bmHeight > hb_parni( 6 ) - 5 )
         {
            DeleteObject( himage2 );
            himage2 = ( HWND ) LoadImage
               (
                  GetResources(),
                  lpImageName2,
                  IMAGE_BITMAP,
                  BtnWidth2 - 4,
                  hb_parni( 6 ) - 6,
                  LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT
               );

            if( himage2 == NULL )
            {
               himage2 = ( HWND ) LoadImage
                  (
                     NULL,
                     lpImageName2,
                     IMAGE_BITMAP,
                     BtnWidth2 - 4,
                     hb_parni( 6 ) - 6,
                     LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT
                  );
            }
         }
      }

#ifdef UNICODE
      hb_xfree( lpImageName2 );
#endif
   }
   else
   {
      himage2 = NULL;
   }

   ibtnStyle1 = BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE;

   if( himage != NULL )
   {
      ibtnStyle1 |= BS_BITMAP;
   }

   ibtnStyle2 = BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE;

   if( himage2 != NULL )
   {
      ibtnStyle2 |= BS_BITMAP;
   }

   if( fBtns )
   {
      hBtn1 = CreateWindow
         (
            WC_BUTTON,
            TEXT( "..." ),
            ibtnStyle1,
            hb_parni( 5 ) - BtnWidth - 3,
            -1,
            BtnWidth,
            hb_parni( 6 ) - 2,
            ( HWND ) hedit,
            ( HMENU ) GBB1,
            GetInstance(),
            NULL
         );
   }
   else
   {
      hBtn1 = 0;
   }

   if( fBtn2 )
   {
      hBtn2 = CreateWindow
         (
            WC_BUTTON,
            TEXT( "..." ),
            ibtnStyle2,
            hb_parni( 5 ) - BtnWidth - BtnWidth2 - 3,
            -1,
            BtnWidth,
            hb_parni( 6 ) - 2,
            ( HWND ) hedit,
            ( HMENU ) GBB2,
            GetInstance(),
            NULL
         );
   }
   else
   {
      hBtn2 = 0;
   }

   if( himage != NULL )
   {
      SendMessage( hBtn1, BM_SETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) himage );
   }

   if( himage2 != NULL )
   {
      SendMessage( hBtn2, BM_SETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) himage2 );
   }

   SendMessage( hedit, EM_SETMARGINS, EC_LEFTMARGIN | EC_RIGHTMARGIN, MAKELONG( 0, BtnWidth + BtnWidth2 + 2 ) );

   hb_reta( 5 );
   hmg_storvnl_HANDLE( hedit, -1, 1 );
   hmg_storvnl_HANDLE( hBtn1, -1, 2 );
   hmg_storvnl_HANDLE( hBtn2, -1, 3 );
   hmg_storvnl_HANDLE( himage, -1, 4 );
   hmg_storvnl_HANDLE( himage2, -1, 5 );
}

/*
 * FUNCTION CHECKBIT( nNumber, nBit )
 *
 * Checks if a specific bit is set in a given number.
 *
 * Parameters:
 *   nNumber : The number to check. (NUMERIC)
 *   nBit    : The bit position to check (1-based). (NUMERIC)
 *
 * Returns:
 *   LOGICAL: .T. if the bit is set, .F. otherwise.
 *
 * Purpose:
 *   This function provides a simple way to determine if a particular bit is set in a numeric value.
 *   This is often used for checking flags or options that are stored as bit fields.
 *
 * Notes:
 *   - The function uses the bitwise AND operator (&) to check if the bit is set.
 *   - The bit position is 1-based, so the least significant bit is bit 1.
 */
HB_FUNC( CHECKBIT )
{
   hb_retl( hb_parnl( 1 ) & ( 1 << ( hb_parni( 2 ) - 1 ) ) );
}

/*
 * FUNCTION GETTEXTHEIGHT( hDC, cString, hFont )
 *
 * Calculates the height of a string in pixels, using the specified device context and font.
 *
 * Parameters:
 *   hDC     : The handle of the device context to use for the calculation. If NULL, a DC will be created and destroyed. (HDC)
 *   cString : The string to measure. (STRING)
 *   hFont   : The handle of the font to use for the calculation. If NULL, the default font is used. (HFONT)
 *
 * Returns:
 *   NUMERIC: The height of the string in pixels.
 *
 * Purpose:
 *   This function is used to determine the vertical space required to display a string with a specific font.
 *   This is useful for calculating the size of UI elements that need to accommodate text.
 *
 * Notes:
 *   - The function uses GetTextExtentPoint32 to calculate the string dimensions.
 *   - If hDC is NULL, the function obtains a device context for the active window and releases it after use.
 *   - If hFont is not NULL, the function selects the font into the device context and restores the original font after use.
 */
HB_FUNC( GETTEXTHEIGHT )
{
   HDC      hDC = hmg_par_raw_HDC( 1 );
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
      hDC = GetDC( hWnd );
   }

   if( hFont )
   {
      hOldFont = ( HFONT ) SelectObject( hDC, hFont );
   }

   GetTextExtentPoint32( hDC, lpString, ( int ) lstrlen( lpString ), &sz );

   if( hFont )
   {
      SelectObject( hDC, hOldFont );
   }

   if( bDestroyDC )
   {
      ReleaseDC( hWnd, hDC );
   }

   hb_retni( sz.cy );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpString );
#endif
}

/*
 * FUNCTION HMG_EDITCONTROLGETCHAR( hWnd, nPos )
 *
 * Retrieves a character from an edit control at a specified position.
 *
 * Parameters:
 *   hWnd : The handle of the edit control. (HWND)
 *   nPos : The position of the character to retrieve (0-based). (NUMERIC)
 *
 * Returns:
 *   STRING: The character at the specified position in the edit control's text.
 *           Returns an empty string if the position is out of bounds or if an error occurs.
 *
 * Purpose:
 *   This function allows accessing individual characters within the text of an edit control.
 *   This can be useful for validating input or performing other text processing tasks.
 *
 * Notes:
 *   - The function uses SendMessage with EM_GETHANDLE to get a handle to the edit control's text buffer.
 *   - It then uses LocalLock and LocalUnlock to access the buffer.
 *   - The function handles both ANSI and Unicode edit controls.
 */
HB_FUNC( HMG_EDITCONTROLGETCHAR )
{
   HLOCAL   hMemLocal;
   TCHAR    *pCh, Text[2] = { 0, 0 };
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   int      nPos = hb_parni( 2 );

#ifdef UNICODE
   LPSTR    pStr;
#endif
   hMemLocal = ( HLOCAL ) SendMessage( hWnd, EM_GETHANDLE, 0, 0 );
   if( hMemLocal )
   {
      pCh = ( TCHAR * ) LocalLock( hMemLocal );
      Text[0] = *( pCh + nPos );
      LocalUnlock( hMemLocal );
   }

#ifndef UNICODE
   hb_retc( Text );
#else
   pStr = WideToAnsi( Text );
   hb_retc( pStr );
   hb_xfree( pStr );
#endif
}

/*
 * FUNCTION HMG_GETCHARWIDTH( hWnd, cChar )
 *
 * Retrieves the width of a character in pixels, using the font of the specified window.
 *
 * Parameters:
 *   hWnd  : The handle of the window. (HWND)
 *   cChar : The character to measure. (STRING)
 *
 * Returns:
 *   NUMERIC: The width of the character in pixels.
 *
 * Purpose:
 *   This function is used to determine the horizontal space required to display a character with the font
 *   used by a specific window. This is useful for calculating the layout of text in UI elements.
 *
 * Notes:
 *   - The function uses GetCharABCWidthsFloat to get the character's ABC widths (A, B, and C spacing).
 *   - The character width is calculated as the sum of the B width and half of the A and C widths.
 *   - The function handles both ANSI and Unicode characters.
 */
HB_FUNC( HMG_GETCHARWIDTH )
{
   HWND        hWnd = hmg_par_raw_HWND( 1 );

#ifndef UNICODE
   const char  *lpChar = ( const char * ) hb_parc( 2 );
#else
   TCHAR       *lpChar = ( TCHAR * ) hb_osStrU16Encode( hb_parc( 2 ) );
#endif
   UINT        iFirstChar, iLastChar;
   HDC         hDC = GetDC( hWnd );
   ABCFLOAT    ABCfloat;

   iFirstChar = ( UINT ) lpChar[0];
   iLastChar = ( UINT ) lpChar[0];
   GetCharABCWidthsFloat( hDC, iFirstChar, iLastChar, &ABCfloat );
   ReleaseDC( hWnd, hDC );

   hb_retnd( ( double ) ( FLOAT ) ( ABCfloat.abcfB + ( ABCfloat.abcfA + ABCfloat.abcfC ) * 0.5 ) );
}

/*
 * FUNCTION OwnGetProc( hwnd, msg, wParam, lParam )
 *
 * This function is a custom window procedure for the GetBox control. It intercepts
 * specific Windows messages to provide custom behavior and event handling.
 *
 * Parameters:
 *   hwnd    : The handle of the window to which the message is directed. (HWND)
 *   msg     : The message being sent. (UINT)
 *   wParam  : Additional message-specific information. (WPARAM)
 *   lParam  : Additional message-specific information. (LPARAM)
 *
 * Returns:
 *   LRESULT: The result of the message processing.  This value depends on the message.
 *            It can be a value returned by DefWindowProc or a custom value to indicate
 *            how the message was handled.
 *
 * Purpose:
 *   This function is crucial for extending the functionality of the standard EDIT control
 *   used in the GetBox. It allows intercepting messages like WM_CHAR, WM_KEYDOWN,
 *   WM_COMMAND (button clicks), and others to trigger custom events in the Harbour
 *   application. This enables features like validating input, handling button clicks
 *   associated with the GetBox, and providing custom context menus.
 *
 * Notes:
 *   - The function uses SubclassWindow2 to replace the original window procedure of the
 *     EDIT control with this custom procedure.
 *   - It uses GetProp and SetProp to store and retrieve the original window procedure
 *     address, allowing it to call the original procedure for messages that are not
 *     handled by the custom procedure.
 *   - The function uses hb_dynsymSymbol and related functions to call a Harbour function
 *     named "OGETEVENTS" when certain messages are received. This allows the Harbour
 *     application to handle events triggered by the GetBox control.
 *   - The function checks the return value from the "OGETEVENTS" Harbour function. If the
 *     return value is non-zero, it assumes that the event has been handled and returns
 *     the value. Otherwise, it calls the original window procedure to handle the message.
 */
LRESULT CALLBACK OwnGetProc( HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB   pSymbol = NULL;
   LRESULT           r;
   WNDPROC           OldWndProc;
   OldWndProc = ( WNDPROC ) ( LONG_PTR ) GetProp( hwnd, TEXT( "OldWndProc" ) );
   switch( Msg )
   {
      case WM_NCDESTROY:
         SubclassWindow2( hwnd, OldWndProc );
         RemoveProp( hwnd, TEXT( "OldWndProc" ) );
         return CallWindowProc( OldWndProc, hwnd, Msg, wParam, lParam );
      case WM_GETDLGCODE:
         return DLGC_WANTALLKEYS + DLGC_WANTARROWS + DLGC_WANTCHARS + DLGC_HASSETSEL;
      case EM_DISPLAYBAND:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OGETEVENTS" ) );
         }
         if( pSymbol )
         {
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }
         r = hmg_par_LRESULT( -1 );
         if( r != 0 )
         {
            return r;
         }
         else
         {
            return DefWindowProc( hwnd, Msg, wParam, lParam );
         }
      case WM_CHAR:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OGETEVENTS" ) );
         }
         if( pSymbol )
         {
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }
         r = hmg_par_LRESULT( -1 );
         if( r != 0 )
         {
            return r;
         }
         else
         {
            return DefWindowProc( hwnd, Msg, wParam, lParam );
         }
      case EM_CANPASTE:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OGETEVENTS" ) );
         }
         if( pSymbol )
         {
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }
         r = hmg_par_LRESULT( -1 );
         if( r != 0 )
         {
            return r;
         }
         else
         {
            return CallWindowProc( OldWndProc, hwnd, Msg, wParam, lParam );
         }
      case WM_PASTE:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OGETEVENTS" ) );
         }
         if( pSymbol )
         {
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }
         r = hmg_par_LRESULT( -1 );
         if( r != 0 )
         {
            return r;
         }
         else
         {
            return DefWindowProc( hwnd, Msg, wParam, lParam );
         }
      case WM_CONTEXTMENU:
      case WM_KILLFOCUS:
      case WM_SETFOCUS:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OGETEVENTS" ) );
         }
         if( pSymbol )
         {
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }
         r = hmg_par_LRESULT( -1 );
         if( r != 0 )
         {
            return r;
         }
         else
         {
            return CallWindowProc( OldWndProc, hwnd, Msg, wParam, lParam );
         }
      case WM_LBUTTONDBLCLK:
      case WM_KEYDOWN:
      case WM_KEYUP:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OGETEVENTS" ) );
         }
         if( pSymbol )
         {
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }
         r = hmg_par_LRESULT( -1 );
         if( r != 0 )
         {
            return r;
         }
         else
         {
            return DefWindowProc( hwnd, Msg, wParam, lParam );
         }
      case WM_CUT:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OGETEVENTS" ) );
         }
         if( pSymbol )
         {
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }
         r = hmg_par_LRESULT( -1 );
         if( r != 0 )
         {
            return r;
         }
         else
         {
            return DefWindowProc( hwnd, Msg, wParam, lParam );
         }
      case WM_COMMAND:
         if( lParam != 0 && HIWORD( wParam ) == BN_CLICKED )
         {
            if( !pSymbol )
            {
               pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OGETEVENTS" ) );
            }
            if( pSymbol )
            {
               hb_vmPushSymbol( pSymbol );
               hb_vmPushNil();
               hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
               hb_vmPushLong( Msg );
               hb_vmPushNumInt( wParam );
               hb_vmPushNumInt( lParam );
               hb_vmDo( 4 );
            }
            r = hmg_par_LRESULT( -1 );
            if( r )
            {
               return TRUE;
            }
            else
            {
               return CallWindowProc( OldWndProc, hwnd, Msg, wParam, lParam );
            }
         }
   }
   return CallWindowProc( OldWndProc, hwnd, Msg, wParam, lParam );
}
