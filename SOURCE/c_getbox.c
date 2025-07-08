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

   SetProp( ( HWND ) hedit, TEXT( "OldWndProc" ), ( HWND ) GetWindowLongPtr( ( HWND ) hedit, GWLP_WNDPROC ) );
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

HB_FUNC( CHECKBIT )
{
   hb_retl( hb_parnl( 1 ) & ( 1 << ( hb_parni( 2 ) - 1 ) ) );
}

HB_FUNC( GETTEXTHEIGHT )   // returns the height of a string in pixels
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

//       HMG_EditControlGetChar (hWnd, nPos)
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

//       HMG_GetCharWidth (hWnd, char)
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

LRESULT CALLBACK OwnGetProc( HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB   pSymbol = NULL;
   LRESULT           r;
   WNDPROC           OldWndProc;

   OldWndProc = ( WNDPROC ) ( HB_PTRUINT ) GetProp( hwnd, TEXT( "OldWndProc" ) );

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
