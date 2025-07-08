/*-------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This  program is free software; you can redistribute it and/or modify it
   under  the  terms  of the GNU General Public License as published by the
   Free  Software  Foundation; either version 2 of the License, or (at your
   option) any later version.

   This  program  is  distributed  in  the hope that it will be useful, but
   WITHOUT   ANY   WARRANTY;   without   even   the   implied  warranty  of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You  should have received a copy of the GNU General Public License along
   with  this  software;  see  the  file COPYING. If not, write to the Free
   Software  Foundation,  Inc.,  59  Temple  Place,  Suite  330, Boston, MA
   02111-1307 USA (or visit the web site http://www.gnu.org/).

   As  a  special exception, you have permission for additional uses of the
   text contained in this release of Harbour Minigui.

   The  exception  is  that,  if  you link the Harbour Minigui library with
   other  files to produce an executable, this does not by itself cause the
   resulting  executable  to  be covered by the GNU General Public License.
   Your  use  of  that  executable  is  in  no way restricted on account of
   linking the Harbour-Minigui library code into it.

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

   Parts of this code  is contributed and used here under permission of his
   author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
   ----------------------------------------------------------------------*/
#define _WIN32_IE    0x0501
#define _WIN32_WINNT 0x0600

#include <mgdefs.h>

#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapicdp.h"

#include <commctrl.h>

#ifndef TTS_CLOSE
#define TTS_CLOSE 0x80
#endif
#ifndef TTM_POPUP
#define TTM_POPUP ( WM_USER + 34 )
#endif
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
typedef struct _tagEDITBALLOONTIP
{
   DWORD    cbStruct;
   LPCWSTR  pszTitle;
   LPCWSTR  pszText;
   INT      ttiIcon; // From TTI_*
} EDITBALLOONTIP, *PEDITBALLOONTIP;

#define ECM_FIRST                                     0x1500            // Edit control messages
#define EM_SHOWBALLOONTIP                             ( ECM_FIRST + 3 ) // Show a balloon tip associated to the edit control
#define Edit_ShowBalloonTip( hwnd, peditballoontip )  ( BOOL ) SNDMSG( ( hwnd ), EM_SHOWBALLOONTIP, 0, ( LPARAM ) ( peditballoontip ) )
#define EM_HIDEBALLOONTIP                             ( ECM_FIRST + 4 ) // Hide any balloon tip associated with the edit control
#define Edit_HideBalloonTip( hwnd )                   ( BOOL ) SNDMSG( ( hwnd ), EM_HIDEBALLOONTIP, 0, 0 )
#endif
#ifndef __XHARBOUR__
#define HB_cdpGetU16( cdp, fCtrl, ch ) hb_cdpGetU16( cdp, ch )
#else
#define HB_cdpGetU16( cdp, fCtrl, ch ) hb_cdpGetU16( cdp, fCtrl, ch )
#define hb_vmCDP()                     hb_cdppage()
#endif
extern BOOL _isValidCtrlClass( HWND, LPCTSTR );

extern BOOL Array2Point( PHB_ITEM aPoint, POINT *pt );
extern BOOL Array2Rect( PHB_ITEM aPoint, RECT *rect );
extern BOOL Array2ColorRef( PHB_ITEM aCRef, COLORREF *cr );
extern HB_EXPORT PHB_ITEM Rect2Array( RECT *rc );

#ifdef UNICODE
LPWSTR AnsiToWide( LPCSTR );
#endif
HINSTANCE GetInstance( void );

static HB_BOOL g_bIsToolTipActive = TRUE;
static HB_BOOL g_bIsToolTipBalloon = FALSE;

static int g_iToolTipMaxWidth = -1;

/*
   HB_FUNC( SETTOOLTIPACTIVATE )

   Description:
      Sets the global tooltip activation state.  When set to .F., tooltips will not appear,
      regardless of individual tooltip settings.

   Parameters:
      1: lActivate (HB_BOOL) -  A logical value indicating whether tooltips should be active (TRUE) or inactive (FALSE).

   Returns:
      HB_BOOL: The previous tooltip activation state.
*/
HB_FUNC( SETTOOLTIPACTIVATE )
{
   HB_BOOL  g_bOldToolTipActive = g_bIsToolTipActive;

   if( HB_ISLOG( 1 ) )
   {
      g_bIsToolTipActive = hb_parl( 1 );
   }

   hb_retl( g_bOldToolTipActive );
}

/*
   HB_FUNC( SETTOOLTIPBALLOON )

   Description:
      Sets the global tooltip balloon style.  When set to .T., newly created tooltips will
      default to the balloon style.

   Parameters:
      1: lBalloon (HB_BOOL) - A logical value indicating whether tooltips should use the balloon style (TRUE) or not (FALSE).

   Returns:
      HB_BOOL: The previous tooltip balloon style setting.
*/
HB_FUNC( SETTOOLTIPBALLOON )
{
   HB_BOOL  g_bOldToolTipBalloon = g_bIsToolTipBalloon;

   if( HB_ISLOG( 1 ) )
   {
      g_bIsToolTipBalloon = hb_parl( 1 );
   }

   hb_retl( g_bOldToolTipBalloon );
}

/*
   HB_FUNC( SETTOOLTIPMAXWIDTH )

   Description:
      Sets the global maximum width for tooltips.  This value is used as the default
      maximum width for newly created tooltips.

   Parameters:
      1: nMaxWidth (INT) - The maximum width for tooltips, in pixels.

   Returns:
      INT: The previous tooltip maximum width setting.
*/
HB_FUNC( SETTOOLTIPMAXWIDTH )
{
   HB_BOOL  g_iOldToolTipMaxWidth = g_iToolTipMaxWidth;

   if( HB_ISNUM( 1 ) )
   {
      g_iToolTipMaxWidth = hb_parni( 1 );
   }

   hb_retni( g_iOldToolTipMaxWidth );
}

/*
   HB_FUNC( INITTOOLTIP )

   Description:
      Initializes a tooltip control.  This function creates a tooltip window that can be
      associated with other controls to display help text.

   Parameters:
      1: nFormHandle (HWND) - The handle of the parent window for the tooltip.  If NIL, the tooltip is created for a modal window.
      2: lBalloon (HB_BOOL) - Optional. A logical value indicating whether the tooltip should use the balloon style (TRUE) or not (FALSE).
                               If omitted, the global tooltip balloon style (set by SETTOOLTIPBALLOON) is used.

   Returns:
      HWND: The handle of the newly created tooltip window.
*/
HB_FUNC( INITTOOLTIP )
{
   HWND  hwndParent = HB_ISNUM( 1 ) ? hmg_par_raw_HWND( 1 ) : ( HWND ) NULL;

   if( HB_ISNIL( 1 ) ? TRUE : IsWindow( hwndParent ) )                  // hack for ModalWindow
   {
      DWORD                dwStyle = WS_POPUP | TTS_ALWAYSTIP;
      INITCOMMONCONTROLSEX icex = { sizeof( INITCOMMONCONTROLSEX ), ICC_BAR_CLASSES };

      if( hb_pcount() > 1 )
      {
         if( HB_ISLOG( 2 ) && hb_parl( 2 ) )
         {
            dwStyle |= TTS_BALLOON;
         }
      }
      else if( g_bIsToolTipBalloon )
      {
         dwStyle |= TTS_BALLOON;
      }

      InitCommonControlsEx( &icex );

      /* Create a tooltip */
      hmg_ret_raw_HWND
      (
         CreateWindow
            (
               TOOLTIPS_CLASS,
               NULL,
               dwStyle,
               CW_USEDEFAULT,
               CW_USEDEFAULT,
               CW_USEDEFAULT,
               CW_USEDEFAULT,
               hwndParent,
               ( HMENU ) NULL,
               GetInstance(),
               NULL
            )
      );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( SETTOOLTIP )

   Description:
      Associates a tooltip with a control.  This function adds or updates the tooltip text
      for a specific control.

   Parameters:
      1: hwndTool (HWND) - The handle of the control to associate the tooltip with.
      2: cText (STRING) - The text to display in the tooltip.
      3: hwndToolTip (HWND) - The handle of the tooltip window.

   Returns:
      HB_BOOL: TRUE if the tooltip was successfully added or updated, FALSE otherwise.
*/
HB_FUNC( SETTOOLTIP )
{
   HWND  hwndTool = hmg_par_raw_HWND( 1 );
   HWND  hwndToolTip = hmg_par_raw_HWND( 3 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      if( IsWindow( hwndTool ) )
      {
#ifndef UNICODE
         LPSTR    lpText = ( LPSTR ) hb_parc( 2 );
#else
         LPWSTR   lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
         TOOLINFO ti;

         /* Set up "tool" information */
         memset( &ti, 0, sizeof( ti ) );
         ti.cbSize = sizeof( TOOLINFO );
         ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
         ti.hwnd = GetParent( hwndTool );
         ti.uId = ( UINT_PTR ) hwndTool;

         if( SendMessage( hwndToolTip, TTM_GETTOOLINFO, ( WPARAM ) 0, ( LPARAM ) ( LPTOOLINFO ) &ti ) )
         {
            SendMessage( hwndToolTip, TTM_DELTOOL, ( WPARAM ) 0, ( LPARAM ) ( LPTOOLINFO ) &ti );
         }

         if( hb_parclen( 2 ) > 0 )
         {
            ti.lpszText = lpText;
         }

         hmg_ret_L( SendMessage( hwndToolTip, TTM_ADDTOOL, ( WPARAM ) 0, ( LPARAM ) ( LPTOOLINFO ) &ti ) );

         SendMessage( hwndToolTip, TTM_ACTIVATE, ( WPARAM ) ( BOOL ) g_bIsToolTipActive, 0 );
#ifdef UNICODE
         hb_xfree( ( TCHAR * ) lpText );
#endif
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/*
   HB_FUNC( SHOWBALLOONTIP )

   Description:
      Displays a balloon tooltip for an edit control.  This function shows a balloon-style
      tooltip associated with the specified edit control.

   Parameters:
      1: hWnd (HWND) - The handle of the edit control.
      2: cText (STRING) - The text to display in the balloon tooltip.
      3: cTitle (STRING) - Optional. The title to display in the balloon tooltip.
      4: nTypeIcon (INT) - Optional. The icon to display in the balloon tooltip.  Valid values are TTI_NONE, TTI_INFO, TTI_WARNING, and TTI_ERROR.

   Returns:
      None.
*/
HB_FUNC( SHOWBALLOONTIP )
{
   WCHAR          Text[512];
   WCHAR          Title[512];
   EDITBALLOONTIP bl;
   const char     *s;
   int            i, k;

   PHB_CODEPAGE   s_cdpHost = hb_vmCDP();

   HWND           hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      bl.cbStruct = sizeof( EDITBALLOONTIP );
      bl.pszTitle = NULL;
      bl.pszText = NULL;
      bl.ttiIcon = hb_parnidef( 4, 0 /*TTI_NONE*/ );

      if( HB_ISCHAR( 2 ) )
      {
         ZeroMemory( Text, sizeof( Text ) );
         k = ( int ) hb_parclen( 2 );
         s = ( const char * ) hb_parc( 2 );
         for( i = 0; i < k; i++ )
         {
            Text[i] = HB_cdpGetU16( s_cdpHost, TRUE, s[i] );
         }

         bl.pszText = Text;
      }

      if( HB_ISCHAR( 3 ) )
      {
         ZeroMemory( Title, sizeof( Title ) );
         k = ( int ) hb_parclen( 3 );
         s = ( const char * ) hb_parc( 3 );
         for( i = 0; i < k; i++ )
         {
            Title[i] = HB_cdpGetU16( s_cdpHost, TRUE, s[i] );
         }

         bl.pszTitle = Title;
      }

      Edit_ShowBalloonTip( hWnd, &bl );
   }
}

/*
   HB_FUNC( HIDEBALLOONTIP )

   Description:
      Hides a balloon tooltip for an edit control.  This function hides any balloon-style
      tooltip currently associated with the specified edit control.

   Parameters:
      1: hWnd (HWND) - The handle of the edit control.

   Returns:
      None.
*/
HB_FUNC( HIDEBALLOONTIP )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      Edit_HideBalloonTip( hWnd );
   }
}

/*
   HB_FUNC( INITTOOLTIPEX )

   Description:
      Initializes an extended tooltip control.  This function creates a tooltip window with
      more advanced options, such as specifying a rectangle for the tooltip to be associated with,
      a title, an icon, and custom styles and flags.

   Parameters:
      1: nFormHandle (HWND) - The handle of the parent window for the tooltip.
      2: aRect (ARRAY) - Optional. An array representing the rectangle to associate the tooltip with.
                         If omitted, the entire client area of the parent window is used.  The array should contain
                         four numeric elements: { Left, Top, Right, Bottom }.
      3: cToolTip (STRING or NUMERIC) - Optional. The text to display in the tooltip.  Can also be a resource ID (numeric).
      4: cTitle (STRING) - Optional. The title to display in the tooltip.
      5: nIcon (INT) - Optional. The icon to display in the tooltip.  Valid values are TTI_NONE, TTI_INFO, TTI_WARNING, and TTI_ERROR.
      6: nStyle (DWORD) - Optional. The style of the tooltip window.  See the Windows API documentation for valid styles.
      7: nFlags (UINT) - Optional. The flags for the tooltip.  See the Windows API documentation for valid flags.

   Returns:
      HWND: The handle of the newly created tooltip window.
*/
HB_FUNC( INITTOOLTIPEX )
{
   HWND  hwndParent = hmg_par_raw_HWND( 1 );

   if( IsWindow( hwndParent ) )
   {
      PHB_ITEM             aRect = hb_param( 2, HB_IT_ANY );
      RECT                 rect;
#ifndef UNICODE
      LPSTR                lpszText = ( LPSTR ) NULL;
      LPSTR                lpszTitle = ( LPSTR ) ( HB_ISCHAR( 4 ) ? hb_parc( 4 ) : NULL );
#else
      LPWSTR               lpszText = ( LPWSTR ) NULL;
      LPWSTR               lpszTitle = HB_ISCHAR( 4 ) ? AnsiToWide( ( char * ) hb_parc( 4 ) ) : NULL;
#endif
      int                  nIcon = hb_parnidef( 5, TTI_NONE );
      DWORD                dwStyle = WS_POPUP;
      HWND                 hwndToolTip;
      TOOLINFO             ti;
      UINT                 uFlags = 0;
      INITCOMMONCONTROLSEX icex = { sizeof( INITCOMMONCONTROLSEX ), ICC_BAR_CLASSES };

      if( !Array2Rect( aRect, &rect ) )
      {
         GetClientRect( hwndParent, &rect );
      }

      if( hb_parclen( 3 ) > 0 )
      {
#ifndef UNICODE
         lpszText = ( LPSTR ) hb_parc( 3 );
#else
         lpszText = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
      }
      else if( HB_ISNUM( 3 ) )
      {
         lpszText = MAKEINTRESOURCE( hb_parni( 3 ) );
      }

      if( HB_ISNUM( 6 ) )
      {
         dwStyle |= hmg_par_DWORD( 6 );
      }

      if( HB_ISNUM( 7 ) )
      {
         uFlags = hmg_par_UINT( 7 );
      }

      InitCommonControlsEx( &icex );

      /* Create a tooltip */
      hwndToolTip = CreateWindowEx
         (
            WS_EX_TOPMOST,
            TOOLTIPS_CLASS,
            NULL,
            dwStyle,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            hwndParent,
            ( HMENU ) NULL,
            GetInstance(),
            NULL
         );

      SetWindowPos( hwndToolTip, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE );

      /* Set up "tool" information. In this case, the "tool" is the entire parent window. */
      memset( &ti, 0, sizeof( ti ) );
      ti.cbSize = sizeof( ti );
      ti.uFlags = uFlags;
      ti.hwnd = hwndParent;
      ti.uId = ( UINT_PTR ) hwndParent;
      ti.rect = rect;
      ti.hinst = GetInstance();
      ti.lpszText = lpszText;

      // Associate the tooltip with the "tool" window.
      SendMessage( hwndToolTip, TTM_ADDTOOL, 0, ( LPARAM ) ( LPTOOLINFO ) &ti );

      if( NULL != lpszTitle )
      {
         SendMessage( hwndToolTip, TTM_SETTITLE, nIcon, ( LPARAM ) lpszTitle );
      }

      if( g_iToolTipMaxWidth != -1 )
      {
         SendMessage( hwndToolTip, TTM_SETMAXTIPWIDTH, 0, ( LPARAM ) g_iToolTipMaxWidth );
      }

      SendMessage( hwndToolTip, TTM_ACTIVATE, ( WPARAM ) ( BOOL ) g_bIsToolTipActive, 0 );

      hmg_ret_raw_HWND( hwndToolTip );

#ifdef UNICODE
      if( lpszText != NULL )
      {
         hb_xfree( ( TCHAR * ) lpszText );
      }

      if( lpszTitle != NULL )
      {
         hb_xfree( ( TCHAR * ) lpszTitle );
      }
#endif
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_ACTIVATE )

   Description:
      Activates or deactivates a tooltip control.

   Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.
      2: lActivate (HB_BOOL) - A logical value indicating whether the tooltip should be active (TRUE) or inactive (FALSE).

   Returns:
      None.
*/
HB_FUNC( TTM_ACTIVATE )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      if( g_bIsToolTipActive )
      {
         SendMessage( hwndToolTip, TTM_ACTIVATE, ( WPARAM ) ( BOOL ) hb_parl( 2 ), 0 );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_GETDELAYTIME )

   Description:
      Retrieves the delay time for a tooltip control.

   Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.
      2: nDelay (INT) - Optional.  A flag specifying which delay time to retrieve.  Valid values are TTDT_AUTOPOP, TTDT_INITIAL, and TTDT_RESHOW.
                         If omitted, TTDT_AUTOPOP is used.

   Returns:
      INT: The delay time, in milliseconds.
*/
HB_FUNC( TTM_GETDELAYTIME )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      hb_retni( ( int ) SendMessage( hwndToolTip, TTM_GETDELAYTIME, hb_parnidef( 2, TTDT_AUTOPOP ), 0 ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_GETMARGIN )

   Description:
      Retrieves the margins for a tooltip window.

   Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.

   Returns:
      ARRAY: An array representing the margins of the tooltip window.  The array contains four numeric elements:
             { Left, Top, Right, Bottom }.
*/
HB_FUNC( TTM_GETMARGIN )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      RECT  rect;

      SendMessage( hwndToolTip, TTM_GETMARGIN, 0, ( LPARAM ) &rect );

      hb_itemReturnRelease( Rect2Array( &rect ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_GETMAXTIPWIDTH )

   Description:
      Retrieves the maximum width for a tooltip window.

   Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.

   Returns:
      INT: The maximum width of the tooltip window, in pixels.
*/
HB_FUNC( TTM_GETMAXTIPWIDTH )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      hb_retni( ( int ) SendMessage( hwndToolTip, TTM_GETMAXTIPWIDTH, 0, 0 ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_GETTIPBKCOLOR )

   Description:
      Retrieves the background color of a tooltip window.

   Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.

   Returns:
      COLORREF: The background color of the tooltip window.
*/
HB_FUNC( TTM_GETTIPBKCOLOR )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      hmg_ret_COLORREF( SendMessage( hwndToolTip, TTM_GETTIPBKCOLOR, 0, 0 ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_GETTIPTEXTCOLOR )

   Description:
      Retrieves the text color of a tooltip window.

   Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.

   Returns:
      COLORREF: The text color of the tooltip window.
*/
HB_FUNC( TTM_GETTIPTEXTCOLOR )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      hmg_ret_COLORREF( SendMessage( hwndToolTip, TTM_GETTIPTEXTCOLOR, 0, 0 ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_GETTOOLCOUNT )

   Description:
      Retrieves the number of tools associated with a tooltip control.

   Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.

   Returns:
      INT: The number of tools associated with the tooltip control.
*/
HB_FUNC( TTM_GETTOOLCOUNT )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      hb_retni( ( int ) SendMessage( hwndToolTip, TTM_GETTOOLCOUNT, 0, 0 ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_POP )

   Description:
      Removes a displayed tooltip window from view.

   Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.

   Returns:
      None.
*/
HB_FUNC( TTM_POP )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      SendMessage( hwndToolTip, TTM_POP, ( WPARAM ) 0, ( LPARAM ) 0 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_POPUP )

   Description:
      Causes the tooltip to display at the coordinates of the last mouse message.

   Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.

   Returns:
      None.
*/
HB_FUNC( TTM_POPUP )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      SendMessage( hwndToolTip, TTM_POPUP, 0, 0 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_SETDELAYTIME )

   Description:
      Sets the delay time for a tooltip control.

   Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.
      2: nDelayType (INT) - A flag specifying which delay time to set.  Valid values are TTDT_AUTOPOP, TTDT_INITIAL, and TTDT_RESHOW.
      3: nMilliSec (INT) - The delay time, in milliseconds.  If -1 is specified, the default delay time is used.

   Returns:
      None.
*/
HB_FUNC( TTM_SETDELAYTIME )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      int   nMilliSec = hb_parnidef( 3, -1 );

      if( nMilliSec < 0 )
      {
         SendMessage( hwndToolTip, TTM_SETDELAYTIME, hb_parnidef( 2, TTDT_AUTOPOP ), -1 );
      }
      else
      {
         SendMessage( hwndToolTip, TTM_SETDELAYTIME, hb_parnidef( 2, TTDT_AUTOPOP ), ( LPARAM ) ( DWORD ) nMilliSec );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_SETMARGIN )

   Description:
      Sets the margins for a tooltip window.

   Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.
      2: aRect (ARRAY) - An array representing the margins of the tooltip window.  The array should contain four numeric elements:
                         { Left, Top, Right, Bottom }.

   Returns:
      None.
*/
HB_FUNC( TTM_SETMARGIN )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      RECT  rect;

      if( Array2Rect( hb_param( 2, HB_IT_ANY ), &rect ) )
      {
         SendMessage( hwndToolTip, TTM_SETMARGIN, 0, ( LPARAM ) &rect );
      }
      else
      {
         hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 2 ) );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_SETMAXTIPWIDTH )

   Sets the maximum width for a tooltip window.

   This function allows you to control the maximum width of a tooltip window.
   This is useful for preventing tooltips from becoming too wide and obscuring
   other parts of the user interface.

   Input Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window to modify.
      2: nMaxWidth (INT) - The maximum width of the tooltip window, in pixels.
                           If this parameter is omitted, the global tooltip maximum width is used.

   Return Value:
      INT: The previous maximum width of the tooltip window, in pixels.
           Returns the current maximum width if the operation is successful.
*/
HB_FUNC( TTM_SETMAXTIPWIDTH )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   // Check if the provided handle is a valid tooltip control.
   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      // Call the Windows API SendMessage to set the maximum tooltip width.
      // TTM_SETMAXTIPWIDTH is the message ID.
      // The WPARAM is unused (set to 0).
      // The LPARAM is the new maximum width. If the second parameter is not passed, the global default is used.
      hb_retni( ( int ) SendMessage( hwndToolTip, TTM_SETMAXTIPWIDTH, 0, ( LPARAM ) hb_parnidef( 2, g_iToolTipMaxWidth ) ) );
   }
   else
   {
      // If the handle is not a valid tooltip control, raise an error.
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_SETTIPBKCOLOR )

   Sets the background color of a tooltip window.

   This function allows you to customize the background color of a tooltip window.
   This can be useful for improving the visibility of tooltips or for matching
   the color scheme of your application.

   Input Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window to modify.
      2: cr (NUMERIC or ARRAY) - The background color of the tooltip window.
                                 This can be specified as either:
                                 - A numeric COLORREF value (e.g., RGB(255, 0, 0) for red).
                                 - An array representing the RGB color components (e.g., {255, 0, 0} for red).

   Return Value:
      None.
*/
HB_FUNC( TTM_SETTIPBKCOLOR )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   // Check if the provided handle is a valid tooltip control.
   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      COLORREF cr = ( COLORREF ) 0;

      // Check if the second parameter is a number or an array that can be converted to a COLORREF.
      if( HB_ISNUM( 2 ) || Array2ColorRef( hb_param( 2, HB_IT_ARRAY ), &cr ) )
      {
         // If the second parameter is a number, convert it to a COLORREF.
         if( HB_ISNUM( 2 ) )
         {
            cr = hmg_par_COLORREF( 2 );
         }

         // Call the Windows API SendMessage to set the tooltip background color.
         // TTM_SETTIPBKCOLOR is the message ID.
         // The WPARAM is the COLORREF value.
         // The LPARAM is unused (set to 0).
         SendMessage( hwndToolTip, TTM_SETTIPBKCOLOR, ( WPARAM ) cr, 0 );
      }
      else
      {
         // If the second parameter is not a valid color, raise an error.
         hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 2 ) );
      }
   }
   else
   {
      // If the handle is not a valid tooltip control, raise an error.
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_SETTIPTEXTCOLOR )

   Sets the text color in a tooltip window.

   This function allows you to customize the text color of a tooltip window.
   This can be useful for improving the readability of tooltips or for matching
   the color scheme of your application.

   Input Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window to modify.
      2: cr (NUMERIC or ARRAY) - The text color of the tooltip window.
                                 This can be specified as either:
                                 - A numeric COLORREF value (e.g., RGB(255, 0, 0) for red).
                                 - An array representing the RGB color components (e.g., {255, 0, 0} for red).

   Return Value:
      None.
*/
HB_FUNC( TTM_SETTIPTEXTCOLOR )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   // Check if the provided handle is a valid tooltip control.
   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      COLORREF cr = ( COLORREF ) 0;

      // Check if the second parameter is a number or an array that can be converted to a COLORREF.
      if( HB_ISNUM( 2 ) || Array2ColorRef( hb_param( 2, HB_IT_ANY ), &cr ) )
      {
         // If the second parameter is a number, convert it to a COLORREF.
         if( HB_ISNUM( 2 ) )
         {
            cr = hmg_par_COLORREF( 2 );
         }

         // Call the Windows API SendMessage to set the tooltip text color.
         // TTM_SETTIPTEXTCOLOR is the message ID.
         // The WPARAM is the COLORREF value.
         // The LPARAM is unused (set to 0).
         SendMessage( hwndToolTip, TTM_SETTIPTEXTCOLOR, ( WPARAM ) cr, 0 );
      }
      else
      {
         // If the second parameter is not a valid color, raise an error.
         hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 2 ) );
      }
   }
   else
   {
      // If the handle is not a valid tooltip control, raise an error.
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_TRACKACTIVATE )

   Activates or deactivates a tracking tooltip.

   Tracking tooltips are tooltips that follow the mouse cursor. This function
   allows you to enable or disable this behavior.

   Input Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window to modify.
      2: hwndTool (HWND) - The handle of the window to which the tooltip is associated.
      3: bActivate (LOGICAL) - A logical value indicating whether to activate (TRUE) or deactivate (FALSE) the tracking tooltip.

   Return Value:
      None.
*/
HB_FUNC( TTM_TRACKACTIVATE )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );
   HWND  hwndTool = hmg_par_raw_HWND( 2 );

   // Check if the provided handles are valid.
   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) && IsWindow( hwndTool ) )
   {
      TOOLINFO ti;

      // Initialize the TOOLINFO structure.  This structure is used to pass information
      // about the tool to the tooltip control.
      memset( &ti, 0, sizeof( ti ) );
      ti.cbSize = sizeof( TOOLINFO );
      ti.hwnd = hwndTool;
      ti.uId = ( UINT_PTR ) hwndTool;

      // Call the Windows API SendMessage to activate or deactivate the tracking tooltip.
      // TTM_TRACKACTIVATE is the message ID.
      // The WPARAM is a logical value indicating whether to activate or deactivate the tooltip.
      // The LPARAM is a pointer to the TOOLINFO structure.
      SendMessage( hwndToolTip, TTM_TRACKACTIVATE, ( WPARAM ) hb_parl( 3 ), ( LPARAM ) ( LPTOOLINFO ) &ti );
   }
   else
   {
      // If the handles are not valid, raise an error.
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/*
   HB_FUNC( TTM_TRACKPOSITION )

   Sets the position of a tracking tooltip.

   This function allows you to control the position of a tracking tooltip.
   This is useful for ensuring that the tooltip is displayed in a location
   that is visible and does not obscure other parts of the user interface.

   Input Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window to modify.
      2: hwndTool (HWND) - The handle of the window to which the tooltip is associated.
      3: aPoint (ARRAY) - An array containing the x and y coordinates of the new position of the tooltip.
                         The coordinates are relative to the client area of the tool window.

   Return Value:
      None.
*/
HB_FUNC( TTM_TRACKPOSITION )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );
   HWND  hwndTool = hmg_par_raw_HWND( 2 );

   // Check if the provided handles are valid.
   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) && IsWindow( hwndTool ) )
   {
      POINT point;

      // Check if the third parameter is a valid array that can be converted to a POINT structure.
      if( Array2Point( hb_param( 3, HB_IT_ARRAY ), &point ) )
      {
         // Convert the client coordinates to screen coordinates.
         ClientToScreen( hwndTool, &point );

         // Call the Windows API SendMessage to set the position of the tracking tooltip.
         // TTM_TRACKPOSITION is the message ID.
         // The WPARAM is unused (set to 0).
         // The LPARAM is a MAKELONG value containing the x and y coordinates.
         SendMessage( hwndToolTip, TTM_TRACKPOSITION, 0, ( LPARAM ) MAKELONG( point.x, point.y ) );
      }
      else
      {
         // If the third parameter is not a valid point, raise an error.
         hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 3 ) );
      }
   }
   else
   {
      // If the handles are not valid, raise an error.
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/*
   HB_FUNC( TTM_UPDATE )

   Forces the current tooltip to be redrawn.

   This function forces the tooltip control to redraw itself. This is useful
   if the contents of the tooltip have changed and you want to ensure that
   the changes are immediately reflected in the display.

   Input Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window to update.

   Return Value:
      None.
*/
HB_FUNC( TTM_UPDATE )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );

   // Check if the provided handle is a valid tooltip control.
   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) )
   {
      // Call the Windows API SendMessage to force the tooltip to update.
      // TTM_UPDATE is the message ID.
      // The WPARAM and LPARAM are unused (set to 0).
      SendMessage( hwndToolTip, TTM_UPDATE, 0, 0 );
   }
   else
   {
      // If the handle is not a valid tooltip control, raise an error.
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
   HB_FUNC( TTM_UPDATETIPTEXT )

   Sets the tooltip text for a tool.

   This function allows you to change the text that is displayed in a tooltip
   window. This is useful for providing dynamic information to the user based
   on the current state of the application.

   Input Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window to modify.
      2: hwndTool (HWND) - The handle of the window to which the tooltip is associated.
      3: cText (STRING) - The new text to display in the tooltip.

   Return Value:
      None.
*/
HB_FUNC( TTM_UPDATETIPTEXT )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );
   HWND  hwndTool = hmg_par_raw_HWND( 2 );

   // Check if the provided handles are valid.
   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) && IsWindow( hwndTool ) )
   {
      // Check if the text parameter is not empty.
      if( hb_parclen( 3 ) > 0 )
      {
         // Get the text from the third parameter.
#ifndef UNICODE
         LPSTR    lpszText = ( LPSTR ) hb_parc( 3 );
#else
         LPWSTR   lpszText = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
         TOOLINFO ti;

         // Initialize the TOOLINFO structure.
         memset( &ti, 0, sizeof( ti ) );
         ti.cbSize = sizeof( TOOLINFO );
         ti.hinst = ( HINSTANCE ) 0;
         ti.hwnd = hwndTool;
         ti.uId = ( UINT_PTR ) hwndTool;
         ti.lpszText = lpszText;

         // Call the Windows API SendMessage to update the tooltip text.
         // TTM_UPDATETIPTEXT is the message ID.
         // The WPARAM is unused (set to 0).
         // The LPARAM is a pointer to the TOOLINFO structure.
         SendMessage( hwndToolTip, TTM_UPDATETIPTEXT, 0, ( LPARAM ) ( LPTOOLINFO ) &ti );

         // Free the allocated memory for the Unicode string if UNICODE is defined.
#ifdef UNICODE
         if( lpszText != NULL )
         {
            hb_xfree( ( TCHAR * ) lpszText );
         }
#endif
      }
   }
   else
   {
      // If the handles are not valid, raise an error.
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/*
   HB_FUNC( TTM_WINDOWFROMPOINT )

   Allows a subclass procedure to cause a tooltip to display text for a
   window other than the one beneath the mouse cursor.

   This function allows you to programmatically determine which window's
   tooltip should be displayed, even if the mouse cursor is not directly
   over that window. This is useful for implementing custom tooltip behavior.

   Input Parameters:
      1: hwndToolTip (HWND) - The handle of the tooltip window.
      2: hwndTool (HWND) - The handle of the window to which the tooltip is associated.
      3: aPoint (ARRAY) - An array containing the x and y coordinates of the point to check.
                         The coordinates are relative to the client area of the tool window.

   Return Value:
      HWND: The handle of the window for which the tooltip should be displayed.
            Returns NULL if no window is found at the specified point.
*/
HB_FUNC( TTM_WINDOWFROMPOINT )
{
   HWND  hwndToolTip = hmg_par_raw_HWND( 1 );
   HWND  hwndTool = hmg_par_raw_HWND( 2 );

   // Check if the provided handles are valid.
   if( _isValidCtrlClass( hwndToolTip, TOOLTIPS_CLASS ) && IsWindow( hwndTool ) )
   {
      POINT point;

      // Check if the third parameter is a valid array that can be converted to a POINT structure.
      if( Array2Point( hb_param( 3, HB_IT_ARRAY ), &point ) )
      {
         // Convert the client coordinates to screen coordinates.
         ClientToScreen( hwndTool, &point );

         // Call the Windows API SendMessage to get the window from the specified point.
         // TTM_WINDOWFROMPOINT is the message ID.
         // The WPARAM is unused (set to 0).
         // The LPARAM is a MAKELONG value containing the x and y coordinates.
         hmg_ret_raw_HWND( SendMessage( hwndToolTip, TTM_WINDOWFROMPOINT, 0, ( LPARAM ) MAKELONG( point.x, point.y ) ) );
      }
      else
      {
         // If the third parameter is not a valid point, raise an error.
         hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 3 ) );
      }
   }
   else
   {
      // If the handles are not valid, raise an error.
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}
