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

   Parts of this code are contributed and used here under permission of the author:
       Copyright 2006 (C) Grigory Filatov <gfilatov@gmail.com>
 ---------------------------------------------------------------------------*/
#define _WIN32_IE 0x0501

#include <mgdefs.h>
#include <windowsx.h>
#include <commctrl.h>
#include "hbvm.h"
#include "hbdate.h"

#ifdef UNICODE
LPWSTR   AnsiToWide( LPCSTR );
#endif
#ifdef __cplusplus
extern "C"
{
#endif
extern HFONT   PrepareFont( TCHAR *, int, int, int, int, int, int, int );
#ifdef __cplusplus
}
#endif
LRESULT CALLBACK  OwnMCProc( HWND hmonthcal, UINT Msg, WPARAM wParam, LPARAM lParam );
HINSTANCE         GetInstance( void );

/*
 * FUNCTION: INITMONTHCAL
 *
 * Creates and initializes a Windows Month Calendar control with custom styles and font settings.
 *
 * Parameters:
 *   1: HWND     - Handle of the parent window.
 *   2: HMENU    - Menu handle (used as control ID).
 *   3: INT      - X coordinate of the control.
 *   4: INT      - Y coordinate of the control.
 *   7: CHAR*    - Font face name (ANSI string; converted to wide string if UNICODE).
 *   8: INT      - Font size in points.
 *   9: LOGICAL  - TRUE to hide the "Today" date.
 *  10: LOGICAL  - TRUE to hide the circle around "Today".
 *  11: LOGICAL  - TRUE to display week numbers.
 *  12: LOGICAL  - FALSE to make the control visible initially.
 *  13: LOGICAL  - FALSE to enable WS_TABSTOP.
 *  14: LOGICAL  - TRUE to set font weight to bold.
 *  15: LOGICAL  - TRUE to make font italic.
 *  16: LOGICAL  - TRUE to underline font.
 *  17: LOGICAL  - TRUE to strike through font.
 *
 * Returns:
 *   Array[2]:
 *     [1]: HANDLE - Handle of the created Month Calendar control.
 *     [2]: HANDLE - Handle of the created font.
 *
 * Purpose:
 *   Creates a Month Calendar control using Windows common controls with styles
 *   and font attributes specified from Harbour parameters. Subclasses the control
 *   to route messages through a custom window procedure for Harbour event handling.
 *   Automatically sizes the control to the minimum required dimensions.
 */
HB_FUNC( INITMONTHCAL )
{
   HWND                 hmonthcal;
   RECT                 rc;
   INITCOMMONCONTROLSEX icex;
   DWORD                Style = WS_BORDER | WS_CHILD | MCS_DAYSTATE;
   HFONT                hfont;
   DWORD                bold = FW_NORMAL;
   DWORD                italic = 0;
   DWORD                underline = 0;
   DWORD                strikeout = 0;
   DWORD                angle = 0;

#ifdef UNICODE
   LPWSTR               pStr;
#endif
   icex.dwSize = sizeof( icex );
   icex.dwICC = ICC_DATE_CLASSES;
   InitCommonControlsEx( &icex );

   if( hb_parl( 9 ) )
   {
      Style |= MCS_NOTODAY;
   }

   if( hb_parl( 10 ) )
   {
      Style |= MCS_NOTODAYCIRCLE;
   }

   if( hb_parl( 11 ) )
   {
      Style |= MCS_WEEKNUMBERS;
   }

   if( !hb_parl( 12 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 13 ) )
   {
      Style |= WS_TABSTOP;
   }

   /* create month calendar */
   hmonthcal = CreateWindowEx( 0, MONTHCAL_CLASS, TEXT( "" ), Style, 0, 0, 0, 0, hmg_par_raw_HWND( 1 ), hmg_par_raw_HMENU( 2 ), GetInstance(), NULL );

   if( hmonthcal )
   {
      /* store original WndProc in a window property safely:
         GetWindowLongPtr returns a pointer-sized integer (the original WndProc).
         Store it as a HANDLE by converting via LONG_PTR to avoid pointer/handle mismatches.
      */
      SetProp( hmonthcal, TEXT( "oldmcproc" ), ( HANDLE ) ( LONG_PTR ) GetWindowLongPtr( hmonthcal, GWLP_WNDPROC ) );

      /* subclass the control (your helper SubclassWindow2 used previously) */
      SubclassWindow2( hmonthcal, OwnMCProc );
   }

   /* font attributes */
   if( hb_parl( 14 ) )
   {
      bold = FW_BOLD;
   }

   if( hb_parl( 15 ) )
   {
      italic = 1;
   }

   if( hb_parl( 16 ) )
   {
      underline = 1;
   }

   if( hb_parl( 17 ) )
   {
      strikeout = 1;
   }

#ifdef UNICODE
   pStr = AnsiToWide( hb_parc( 7 ) );
   hfont = PrepareFont( ( TCHAR * ) pStr, hb_parni( 8 ), bold, italic, underline, strikeout, angle, DEFAULT_CHARSET );
   hb_xfree( pStr );
#else
   hfont = PrepareFont( ( TCHAR * ) hb_parc( 7 ), hb_parni( 8 ), bold, italic, underline, strikeout, angle, DEFAULT_CHARSET );
#endif
   if( hfont )
   {
      SetWindowFont( hmonthcal, hfont, TRUE );
   }

   /* size to minimum required rectangle */
   MonthCal_GetMinReqRect( hmonthcal, &rc );
   SetWindowPos( hmonthcal, NULL, hb_parni( 3 ), hb_parni( 4 ), rc.right, rc.bottom, SWP_NOZORDER );

   hb_reta( 2 );
   hmg_storvnl_HANDLE( hmonthcal, -1, 1 );
   hmg_storvnl_HANDLE( hfont, -1, 2 );
}

/*
 * FUNCTION: SETMONTHCALVALUE
 *
 * Sets the selected date in a Month Calendar control.
 *
 * Parameters:
 *   1: HWND  - Handle of the Month Calendar control.
 *   2: WORD  - Year to set.
 *   3: WORD  - Month to set (1-12).
 *   4: WORD  - Day to set (1-31).
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Allows Harbour code to programmatically select a specific date in the
 *   Month Calendar control using year, month, and day parameters.
 */
HB_FUNC( SETMONTHCALVALUE )
{
   SYSTEMTIME  sysTime;
   HWND        hwnd = hmg_par_raw_HWND( 1 );

   sysTime.wYear = hmg_par_WORD( 2 );
   sysTime.wMonth = hmg_par_WORD( 3 );
   sysTime.wDay = hmg_par_WORD( 4 );
   sysTime.wDayOfWeek = 0;
   sysTime.wHour = 0;
   sysTime.wMinute = 0;
   sysTime.wSecond = 0;
   sysTime.wMilliseconds = 0;

   MonthCal_SetCurSel( hwnd, &sysTime );
}

/*
 * FUNCTION: GETMONTHCALVALUE
 *
 * Retrieves a specific date component (year, month, or day) from the Month Calendar control.
 *
 * Parameters:
 *   1: HWND - Handle of the Month Calendar control.
 *   2: INT  - Component selector:
 *             1 = Year
 *             2 = Month
 *             3 = Day
 *
 * Returns:
 *   INT - Value of the requested date component.
 *
 * Purpose:
 *   Enables Harbour code to extract individual components of the currently
 *   selected date in the Month Calendar control without retrieving the full date.
 */
HB_FUNC( GETMONTHCALVALUE )
{
   SYSTEMTIME  st;
   SendMessage( hmg_par_raw_HWND( 1 ), MCM_GETCURSEL, 0, ( LPARAM ) & st );

   switch( hb_parni( 2 ) )
   {
      case 1:
         hb_retni( st.wYear );
         break;

      case 2:
         hb_retni( st.wMonth );
         break;

      case 3:
         hb_retni( st.wDay );
         break;
   }
}

/*
 * FUNCTION: GETMONTHCALDATE
 *
 * Retrieves the currently selected date from the Month Calendar control
 * as a Harbour date value.
 *
 * Parameters:
 *   1: HWND - Handle of the Month Calendar control.
 *
 * Returns:
 *   Harbour Date - Encoded date corresponding to the selected day.
 *
 * Purpose:
 *   Provides Harbour applications with the full date in a format directly
 *   usable in Harbour date operations, simplifying integration with date logic.
 */
HB_FUNC( GETMONTHCALDATE )
{
   SYSTEMTIME  st;
   SendMessage( hmg_par_raw_HWND( 1 ), MCM_GETCURSEL, 0, ( LPARAM ) & st );
   hb_retdl( hb_dateEncode( st.wYear, st.wMonth, st.wDay ) );
}

/*
 * FUNCTION: SETPOSMONTHCAL
 *
 * Adjusts the position and size of a Month Calendar control based on its
 * minimum required rectangle and optional padding.
 *
 * Parameters:
 *   1: HWND    - Handle of the Month Calendar control.
 *   2: INT     - X coordinate of the control.
 *   3: INT     - Y coordinate of the control.
 *   4: LOGICAL - TRUE to add padding around the control, FALSE otherwise (optional).
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Ensures that the Month Calendar is positioned correctly and large enough
 *   to display all its content, including the "Today" area if present.
 */
HB_FUNC( SETPOSMONTHCAL )
{
   HWND  hWndMonthCal = hmg_par_raw_HWND( 1 );
   RECT  rc;
   DWORD dwWidth;

   MonthCal_GetMinReqRect( hWndMonthCal, &rc );
   dwWidth = MonthCal_GetMaxTodayWidth( hWndMonthCal );

   if( dwWidth > ( DWORD ) rc.right )
   {
      rc.right = dwWidth;
   }

   if( hb_parldef( 4, HB_FALSE ) )
   {
      InflateRect( &rc, 6, 6 );
   }

   SetWindowPos( hWndMonthCal, NULL, hb_parni( 2 ), hb_parni( 3 ), rc.right, rc.bottom, SWP_NOZORDER );
}

/*
 * FUNCTION: GETMONTHRANGE
 *
 * Retrieves the date range currently displayed in the Month Calendar control.
 *
 * Parameters:
 *   1: HWND - Handle of the Month Calendar control.
 *
 * Returns:
 *   Array[3]:
 *     [1]: INT      - Number of months in the displayed range.
 *     [2]: Date     - Start date of the displayed range.
 *     [3]: Date     - End date of the displayed range.
 *
 * Purpose:
 *   Allows Harbour code to determine which dates are currently visible in the
 *   Month Calendar, useful for updating day states or validating displayed content.
 */
HB_FUNC( GETMONTHRANGE )
{
   SYSTEMTIME  sysTime[2];
   int         iCount;

   iCount = MonthCal_GetMonthRange( hmg_par_raw_HWND( 1 ), GMR_DAYSTATE, sysTime );

   hb_reta( 3 );
   HB_STORNI( iCount, -1, 1 );
   HB_STORDL( hb_dateEncode( sysTime[0].wYear, sysTime[0].wMonth, sysTime[0].wDay ), -1, 2 );
   HB_STORDL( hb_dateEncode( sysTime[1].wYear, sysTime[1].wMonth, sysTime[1].wDay ), -1, 3 );
}

#ifndef BOLDDAY
#define BOLDDAY( ds, iDay ) \
   if( iDay > 0 && iDay < 32 ) ( ds ) |= ( 0x00000001 << ( iDay - 1 ) )
#endif

   /*
 * FUNCTION: C_SETDAYSTATE
 *
 * Sets custom "bold" days in the Month Calendar control.
 *
 * Parameters:
 *   1: HWND        - Handle of the Month Calendar control.
 *   2: INT         - Number of months for which to set day states.
 *   3: ARRAY(INT)  - Array of day states where each entry is 1 (bold) or 0 (normal).
 *                    Indexed as: monthIndex * 32 + dayNumber.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Allows Harbour applications to visually emphasize specific days (e.g., events,
 *   holidays) by rendering them in bold on the Month Calendar.
 */
   HB_FUNC( C_SETDAYSTATE )
{
   int               iCount = hb_parni( 2 );
   PHB_ITEM          hArray = hb_param( 3, HB_IT_ARRAY );
   LPMONTHDAYSTATE   rgMonths;
   int               i, j, iSize;

   iSize = sizeof( MONTHDAYSTATE ) * iCount;
   rgMonths = ( LPMONTHDAYSTATE ) hb_xgrab( iSize );
   memset( rgMonths, 0, iSize );

   for( i = 0; i < iCount; i++ )
   {
      for( j = 1; j <= 32; j++ )
      {
         if( hb_arrayGetNI( hArray, i * 32 + j ) == 1 )
         {
            BOLDDAY( rgMonths[i], j );
         }
      }
   }
   SendMessage( hmg_par_raw_HWND( 1 ), MCM_SETDAYSTATE, ( WPARAM ) iCount, ( LPARAM ) rgMonths );
   hb_xfree( rgMonths );
}

/*
 * FUNCTION: C_RETDAYSTATE
 *
 * Prepares a MONTHDAYSTATE array from a Harbour day state array and stores
 * it in an NMDAYSTATE structure.
 *
 * Parameters:
 *   1: LONG_PTR   - Pointer to NMDAYSTATE structure.
 *   2: INT        - Number of months.
 *   3: ARRAY(INT) - Array of day states (1 = bold, 0 = normal).
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Converts Harbour day-state array format into the Windows MONTHDAYSTATE
 *   format for use in custom draw operations or notifications.
 */
HB_FUNC( C_RETDAYSTATE )
{
   LPNMDAYSTATE      pData = ( NMDAYSTATE * ) HB_PARNL( 1 );
   int               iCount = hb_parni( 2 );
   PHB_ITEM          hArray = hb_param( 3, HB_IT_ARRAY );
   LPMONTHDAYSTATE   rgMonths;
   int               i, j, iSize;

   if( pData == NULL || iCount <= 0 || hArray == NULL )
   {
      /* nothing to do */
      return;
   }

   iSize = sizeof( MONTHDAYSTATE ) * iCount;
   rgMonths = ( LPMONTHDAYSTATE ) hb_xgrab( iSize );
   memset( rgMonths, 0, iSize );

   for( i = 0; i < iCount; i++ )
   {
      for( j = 1; j <= 32; j++ )
      {
         if( hb_arrayGetNI( hArray, i * 32 + j ) == 1 )
         {
            BOLDDAY( rgMonths[i], j );
         }
      }
   }

   /* assign buffer to notification structure - do NOT free rgMonths here.
      The caller or appropriate cleanup code must free it after Windows is done. */
   pData->prgDayState = rgMonths;
}

/*
 * FUNCTION: FREEDAYSTATE
 *
 * Frees the memory previously allocated and assigned to pData->prgDayState
 * in C_RETDAYSTATE.
 *
 * Parameters:
 *   1: LONG_PTR - Pointer to an NMDAYSTATE structure.
 *
 * Returns:
 *   None.
 *
 * Usage:
 *   Call this after you finish processing the MCN_GETDAYSTATE notification.
 */
HB_FUNC( FREEDAYSTATE )
{
   LPNMDAYSTATE   pData = ( NMDAYSTATE * ) HB_PARNL( 1 );

   if( pData && pData->prgDayState )
   {
      hb_xfree( ( void * ) pData->prgDayState );
      pData->prgDayState = NULL;
   }
}

/*
 * FUNCTION: GETDAYSTATEDATA
 *
 * Retrieves the number of bold day states and the start date from an NMDAYSTATE structure.
 *
 * Parameters:
 *   1: LONG_PTR - Pointer to NMDAYSTATE structure.
 *
 * Returns:
 *   Array[2]:
 *     [1]: INT  - Count of bold day states.
 *     [2]: Date - Start date for the bold day states.
 *
 * Purpose:
 *   Allows Harbour code to inspect day state notification data sent by
 *   the Month Calendar control, useful for dynamically updating bold days.
 */
HB_FUNC( GETDAYSTATEDATA )
{
   LPNMDAYSTATE   pData = ( NMDAYSTATE * ) HB_PARNL( 1 );

   hb_reta( 2 );
   HB_STORNI( ( int ) pData->cDayState, -1, 1 );
   HB_STORDL( hb_dateEncode( pData->stStart.wYear, pData->stStart.wMonth, pData->stStart.wDay ), -1, 2 );
}

/*
 * FUNCTION: OwnMCProc
 *
 * Custom window procedure for Month Calendar control to handle specific
 * Windows messages and forward them to a Harbour event handler.
 *
 * Parameters:
 *   hwnd   - Handle of the Month Calendar control.
 *   Msg    - Windows message identifier.
 *   wParam - Additional message information (depends on Msg).
 *   lParam - Additional message information (depends on Msg).
 *
 * Returns:
 *   LRESULT - Result of message processing (0 or non-zero depending on handling).
 *
 * Purpose:
 *   Subclasses the Month Calendar control to intercept focus and mouse activation
 *   messages, passing them to a Harbour function ("OMONTHCALEVENTS") for custom handling.
 *   Restores the original procedure on WM_DESTROY.
 */
LRESULT CALLBACK OwnMCProc( HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB   pSymbol = NULL;
   LRESULT           r;
   WNDPROC           OldWndProc = NULL;
   HANDLE            hOldProp;

   /* Retrieve the stored original WndProc from the window property.
      It was stored as (HANDLE)(LONG_PTR)originalProc in INITMONTHCAL.
   */
   hOldProp = GetProp( hwnd, TEXT( "oldmcproc" ) );
   if( hOldProp )
   {
      OldWndProc = ( WNDPROC ) ( LONG_PTR ) hOldProp;
   }

   switch( Msg )
   {
      case WM_DESTROY:
         /* restore original wndproc and remove property */
         if( OldWndProc )
         {
            SubclassWindow2( hwnd, OldWndProc );
         }

         RemoveProp( hwnd, TEXT( "oldmcproc" ) );
         break;

      case WM_MOUSEACTIVATE:
      case WM_SETFOCUS:
      case WM_KILLFOCUS:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OMONTHCALEVENTS" ) );
         }

         if( pSymbol )
         {
            /* push symbol and parameters for Harbour callback */
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }

         /* Read possible Harbour return */
         r = hmg_par_LRESULT( -1 );

         /* if harbour returned non-zero, use it; otherwise call original wndproc */
         return( r != 0 ) ? r : CallWindowProc( OldWndProc, hwnd, Msg, wParam, lParam );
   }

   /* default: forward to original procedure */
   return CallWindowProc( OldWndProc, hwnd, Msg, wParam, lParam );
}
