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

   Parts of this code is contributed and used here under permission of his author:
       Copyright 2006 (C) Grigory Filatov <gfilatov@gmail.com>
   ---------------------------------------------------------------------------*/

// Define the minimum required Internet Explorer version to be 5.01, which is needed by some Windows controls.
#define _WIN32_IE 0x0501

// Include necessary headers for Harbour library definitions, Windows macros, and common controls.
#include <mgdefs.h>
#include <windowsx.h>
#include <commctrl.h>

// Include Harbour-specific headers for VM and date functions.
#include "hbvm.h"
#include "hbdate.h"

// If UNICODE is enabled, declare a function to convert ANSI strings to wide character strings.
#ifdef UNICODE
LPWSTR   AnsiToWide( LPCSTR );
#endif

// Declare the PrepareFont function for C linkage.
#ifdef __cplusplus
extern "C"
{
#endif
extern HFONT   PrepareFont( TCHAR *, int, int, int, int, int, int, int );

#ifdef __cplusplus
}
#endif

// Custom Window Procedure for the Month Calendar control.
LRESULT CALLBACK  OwnMCProc( HWND hmonthcal, UINT Msg, WPARAM wParam, LPARAM lParam );

// Function to retrieve the instance handle for the application.
HINSTANCE         GetInstance( void );

// Harbour function to initialize a Month Calendar control.
HB_FUNC( INITMONTHCAL )
{
   HWND                 hmonthcal;
   RECT                 rc;
   INITCOMMONCONTROLSEX icex;
   DWORD                Style = WS_BORDER | WS_CHILD | MCS_DAYSTATE; // Define initial window styles.
   HFONT                hfont;
   DWORD                bold = FW_NORMAL;
   DWORD                italic = 0;
   DWORD                underline = 0;
   DWORD                strikeout = 0;
   DWORD                angle = 0;

#ifdef UNICODE
   LPWSTR               pStr;
#endif

   // Initialize common controls with date class for Month Calendar support.
   icex.dwSize = sizeof( icex );
   icex.dwICC = ICC_DATE_CLASSES;
   InitCommonControlsEx( &icex );

   // Check optional style flags based on Harbour parameters.
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

   // Create the Month Calendar window.
   hmonthcal = CreateWindowEx( 0, MONTHCAL_CLASS, TEXT( "" ), Style, 0, 0, 0, 0, hmg_par_raw_HWND( 1 ), hmg_par_raw_HMENU( 2 ), GetInstance(), NULL );

   // Store the original window procedure and subclass the control.
   SetProp( ( HWND ) hmonthcal, TEXT( "oldmcproc" ), ( HWND ) GetWindowLongPtr( ( HWND ) hmonthcal, GWLP_WNDPROC ) );
   SubclassWindow2( hmonthcal, OwnMCProc );

   // Check font properties based on Harbour parameters.
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

   // Prepare the font for the Month Calendar control.
#ifdef UNICODE
   pStr = AnsiToWide( hb_parc( 7 ) );           // Convert the font name to wide characters if needed.
   hfont = PrepareFont( ( TCHAR * ) pStr, hb_parni( 8 ), bold, italic, underline, strikeout, angle, DEFAULT_CHARSET );
   hb_xfree( pStr );                            // Free the allocated wide string.
#else
   hfont = PrepareFont( ( TCHAR * ) hb_parc( 7 ), hb_parni( 8 ), bold, italic, underline, strikeout, angle, DEFAULT_CHARSET );
#endif

   // Set the custom font for the Month Calendar.
   SetWindowFont( hmonthcal, hfont, TRUE );

   // Get the minimum required rectangle size for the calendar.
   MonthCal_GetMinReqRect( hmonthcal, &rc );

   // Position the Month Calendar window with the calculated rectangle size.
   SetWindowPos( hmonthcal, NULL, hb_parni( 3 ), hb_parni( 4 ), rc.right, rc.bottom, SWP_NOZORDER );

   // Return the Month Calendar handle and font handle as an array.
   hb_reta( 2 );
   hmg_storvnl_HANDLE( hmonthcal, -1, 1 );
   hmg_storvnl_HANDLE( hfont, -1, 2 );
}

// Harbour function to set a specific date on the Month Calendar control.
HB_FUNC( SETMONTHCALVALUE )
{
   SYSTEMTIME  sysTime;
   HWND        hwnd = hmg_par_raw_HWND( 1 );

   // Set the year, month, and day based on Harbour parameters.
   sysTime.wYear = hmg_par_WORD( 2 );
   sysTime.wMonth = hmg_par_WORD( 3 );
   sysTime.wDay = hmg_par_WORD( 4 );
   sysTime.wDayOfWeek = LOWORD( SendMessage( hwnd, MCM_GETFIRSTDAYOFWEEK, 0, 0 ) );

   // Initialize time fields to zero as only the date is relevant.
   sysTime.wHour = 0;
   sysTime.wMinute = 0;
   sysTime.wSecond = 0;
   sysTime.wMilliseconds = 0;

   // Set the selected date on the Month Calendar.
   MonthCal_SetCurSel( hwnd, &sysTime );
}

// Harbour function to retrieve the current selected year, month, or day from the Month Calendar.
HB_FUNC( GETMONTHCALVALUE )
{
   SYSTEMTIME  st;

   // Get the current selected date from the Month Calendar control.
   SendMessage( hmg_par_raw_HWND( 1 ), MCM_GETCURSEL, 0, ( LPARAM ) & st );

   // Return the requested date component based on the Harbour parameter.
   switch( hb_parni( 2 ) )
   {
      case 1:
         hb_retni( st.wYear );                  // Return the year.
         break;

      case 2:
         hb_retni( st.wMonth );                 // Return the month.
         break;

      case 3:
         hb_retni( st.wDay );                   // Return the day.
   }
}

// Harbour function to retrieve the selected date as a Harbour date object.
HB_FUNC( GETMONTHCALDATE )
{
   SYSTEMTIME  st;

   // Get the current selected date from the Month Calendar control.
   SendMessage( hmg_par_raw_HWND( 1 ), MCM_GETCURSEL, 0, ( LPARAM ) & st );

   // Convert SYSTEMTIME to a Harbour date and return it.
   hb_retdl( hb_dateEncode( st.wYear, st.wMonth, st.wDay ) );
}

// Harbour function to set the position and dimensions of a Month Calendar control.
HB_FUNC( SETPOSMONTHCAL )
{
   HWND  hWndMonthCal = hmg_par_raw_HWND( 1 );  // Retrieve Month Calendar window handle from the first parameter.
   RECT  rc;
   DWORD dwWidth;

   // Get the minimum required rectangle size for the Month Calendar control.
   MonthCal_GetMinReqRect( hWndMonthCal, &rc );

   // Get the maximum width of the "Today" date indicator and adjust the control width if needed.
   dwWidth = MonthCal_GetMaxTodayWidth( hWndMonthCal );
   if( dwWidth > ( DWORD ) rc.right )
   {
      rc.right = dwWidth;
   }

   // If the fourth parameter is true, inflate the rectangle slightly to add padding.
   if( hb_parldef( 4, HB_FALSE ) )
   {
      InflateRect( &rc, 6, 6 );
   }

   // Set the position and size of the Month Calendar control based on the provided coordinates and dimensions.
   SetWindowPos( hWndMonthCal, NULL, hb_parni( 2 ), hb_parni( 3 ), rc.right, rc.bottom, SWP_NOZORDER );
}

// Harbour function to retrieve the date range displayed in the Month Calendar control.
HB_FUNC( GETMONTHRANGE )
{
   SYSTEMTIME  sysTime[2];       // Array to store the start and end dates of the displayed range.
   int         iCount;

   // Get the displayed month range and populate the SYSTEMTIME array with start and end dates.
   iCount = MonthCal_GetMonthRange( hmg_par_raw_HWND( 1 ), GMR_DAYSTATE, sysTime );

   // Return the count and date range as an array to Harbour.
   hb_reta( 3 );
   HB_STORNI( iCount, -1, 1 );   // Store the number of displayed months.
   HB_STORDL( hb_dateEncode( sysTime[0].wYear, sysTime[0].wMonth, sysTime[0].wDay ), -1, 2 );   // Store start date.
   HB_STORDL( hb_dateEncode( sysTime[1].wYear, sysTime[1].wMonth, sysTime[1].wDay ), -1, 3 );   // Store end date.
}

// Macro to mark a day as bold in the MONTHDAYSTATE structure if the day is valid.
#ifndef BOLDDAY
#define BOLDDAY( ds, iDay ) \
   if( iDay > 0 && iDay < 32 ) ( ds ) |= ( 0x00000001 << ( iDay - 1 ) )
#endif

// Harbour function to set custom bold days in the Month Calendar control.
HB_FUNC( C_SETDAYSTATE )
{
   int               iCount = hb_parni( 2 );                // Number of months for day state.
   PHB_ITEM          hArray = hb_param( 3, HB_IT_ARRAY );   // Array containing day states.
   LPMONTHDAYSTATE   rgMonths;         // Array to hold day states.
   int               i, j, iSize;

   // Allocate and initialize memory for the day states array.
   iSize = sizeof( MONTHDAYSTATE ) * iCount;
   rgMonths = ( LPMONTHDAYSTATE ) hb_xgrab( iSize );
   memset( rgMonths, 0, iSize );

   // Iterate over each day in each month to set bold days based on the array values.
   for( i = 0; i < iCount; i++ )
   {
      for( j = 1; j <= 32; j++ )
      {
         if( hb_arrayGetNI( hArray, i * 32 + j ) == 1 )
         {
            BOLDDAY( rgMonths[i], j ); // Mark day as bold if array entry is 1.
         }
      }
   }

   // Send the day states to the Month Calendar control.
   SendMessage( hmg_par_raw_HWND( 1 ), MCM_SETDAYSTATE, ( WPARAM ) iCount, ( LPARAM ) rgMonths );

   // Free the allocated day state memory.
   hb_xfree( rgMonths );
}

// Harbour function to return the bold day state configuration.
HB_FUNC( C_RETDAYSTATE )
{
   LPNMDAYSTATE      pData = ( NMDAYSTATE * ) HB_PARNL( 1 );   // Pointer to day state data.
   int               iCount = hb_parni( 2 );                   // Number of months for day state.
   PHB_ITEM          hArray = hb_param( 3, HB_IT_ARRAY );      // Array containing day states.
   LPMONTHDAYSTATE   rgMonths;         // Array to hold day states.
   int               i, j, iSize;

   // Allocate and initialize memory for the day states array.
   iSize = sizeof( MONTHDAYSTATE ) * iCount;
   rgMonths = ( LPMONTHDAYSTATE ) hb_xgrab( iSize );
   memset( rgMonths, 0, iSize );

   // Iterate over each day in each month to retrieve the day states from the array.
   for( i = 0; i < iCount; i++ )
   {
      for( j = 1; j <= 32; j++ )
      {
         if( hb_arrayGetNI( hArray, i * 32 + j ) == 1 )
         {
            BOLDDAY( rgMonths[i], j ); // Mark day as bold if array entry is 1.
         }
      }
   }

   // Store the bold day state information in the provided pointer structure.
   pData->prgDayState = rgMonths;

   // Free the allocated day state memory.
   hb_xfree( rgMonths );
}

// Harbour function to get the number of bold days and starting date.
HB_FUNC( GETDAYSTATEDATA )
{
   LPNMDAYSTATE   pData = ( NMDAYSTATE * ) HB_PARNL( 1 );   // Pointer to day state data.

   // Return array with the count of bold days and start date.
   hb_reta( 2 );
   HB_STORNI( ( int ) pData->cDayState, -1, 1 );            // Store the count of bold day states.
   HB_STORDL( hb_dateEncode( pData->stStart.wYear, pData->stStart.wMonth, pData->stStart.wDay ), -1, 2 );   // Store start date.
}

// Custom Window Procedure for the Month Calendar control to handle specific messages.
LRESULT CALLBACK OwnMCProc( HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB   pSymbol = NULL;   // Static symbol for the Harbour function.
   LRESULT           r;
   WNDPROC           OldWndProc;

   // Retrieve the original window procedure.
   OldWndProc = ( WNDPROC ) ( HB_PTRUINT ) GetProp( hwnd, TEXT( "oldmcproc" ) );

   // Handle specific messages to manage focus and mouse interactions.
   switch( Msg )
   {
      case WM_DESTROY:
         // Restore the original window procedure and remove the subclass.
         SubclassWindow2( hwnd, OldWndProc );
         RemoveProp( hwnd, TEXT( "oldmcproc" ) );
         break;

      case WM_MOUSEACTIVATE:
      case WM_SETFOCUS:
      case WM_KILLFOCUS:
         // Dynamically retrieve and cache the symbol for handling events in Harbour.
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OMONTHCALEVENTS" ) );
         }

         // If the symbol exists, call the Harbour event handler function.
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

         // Get the result from the Harbour function and return it, or call the original window procedure.
         r = hmg_par_LRESULT( -1 );
         return( r != 0 ) ? r : CallWindowProc( OldWndProc, hwnd, Msg, wParam, lParam );
   }

   // Default message handling.
   return CallWindowProc( OldWndProc, hwnd, Msg, wParam, lParam );
}
