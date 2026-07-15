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

   Parts of this code are contributed and used here under permission of the author:
   Copyright 2005 (C) Jacek Kubica <kubica@wssk.wroc.pl>
 ---------------------------------------------------------------------------*/

// Define the minimum required version of Internet Explorer
#define _WIN32_IE 0x0501

#include <mgdefs.h>
#include <commctrl.h>

#include "hbvm.h"
#include "hbdate.h"

#ifdef UNICODE
LPWSTR                  AnsiToWide( LPCSTR );
#endif
HINSTANCE               GetInstance( void );

LRESULT CALLBACK        OwnPickProc( HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam );

#ifdef __XHARBOUR__
#define HB_ISDATETIME   ISDATETIME
#else
extern HB_EXPORT double hb_timeStampPack( int iYear, int iMonth, int iDay, int iHour, int iMinutes, int iSeconds, int iMSec );
#endif

// Common helper to initialize common controls
static void InitDateTimeControls( void )
{
   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof( INITCOMMONCONTROLSEX );
   i.dwICC = ICC_DATE_CLASSES;
   InitCommonControlsEx( &i );
}

// Common helper for picker creation
static HWND CreatePickerControl( HWND hParent, HMENU hMenu, DWORD baseStyle, int x, int y, int w, int h )
{
   return CreateWindowEx( WS_EX_CLIENTEDGE, DATETIMEPICK_CLASS, TEXT( "DateTime" ), baseStyle, x, y, w, h, hParent, hMenu, GetInstance(), NULL );
}

// Initialize date picker
HB_FUNC( INITDATEPICK )
{
   DWORD Style = WS_CHILD;
   HWND  hbutton;

   InitDateTimeControls();

   if( hb_parl( 9 ) )
   {
      Style |= DTS_SHOWNONE;
   }

   if( hb_parl( 10 ) )
   {
      Style |= DTS_UPDOWN;
   }

   if( hb_parl( 11 ) )
   {
      Style |= DTS_RIGHTALIGN;
   }

   if( !hb_parl( 12 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 13 ) )
   {
      Style |= WS_TABSTOP;
   }

   hbutton = CreatePickerControl( hmg_par_raw_HWND( 1 ), hmg_par_raw_HMENU( 2 ), Style, hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ) );

   SetProp( hbutton, TEXT( "oldpickproc" ), ( HANDLE ) ( LONG_PTR ) GetWindowLongPtr( hbutton, GWLP_WNDPROC ) );
   SubclassWindow2( hbutton, OwnPickProc );

   hmg_ret_raw_HWND( hbutton );
}

// Initialize time picker
HB_FUNC( INITTIMEPICK )
{
   DWORD Style = WS_CHILD | DTS_TIMEFORMAT;
   HWND  hbutton;

   InitDateTimeControls();

   if( hb_parl( 9 ) )
   {
      Style |= DTS_SHOWNONE;
   }

   if( !hb_parl( 10 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 11 ) )
   {
      Style |= WS_TABSTOP;
   }

   hbutton = CreatePickerControl( hmg_par_raw_HWND( 1 ), hmg_par_raw_HMENU( 2 ), Style, hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ) );

   SetProp( hbutton, TEXT( "oldpickproc" ), ( HANDLE ) ( LONG_PTR ) GetWindowLongPtr( hbutton, GWLP_WNDPROC ) );
   SubclassWindow2( hbutton, OwnPickProc );

   hmg_ret_raw_HWND( hbutton );
}

// Custom window procedure
LRESULT CALLBACK OwnPickProc( HWND hButton, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB   pSymbol = NULL;
   WNDPROC           OldWndProc;

   OldWndProc = ( WNDPROC ) ( LONG_PTR ) GetProp( hButton, TEXT( "oldpickproc" ) );

   switch( Msg )
   {
      case WM_DESTROY:
         SubclassWindow2( hButton, OldWndProc );
         RemoveProp( hButton, TEXT( "oldpickproc" ) );
         break;

      case WM_ERASEBKGND:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OPICKEVENTS" ) );
         }

         if( pSymbol )
         {
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hButton );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }
         {
            LRESULT  r = hmg_par_LRESULT( -1 );
            if( r != 0 )
            {
               return r;
            }
         }
         break;
   }

   return CallWindowProc( OldWndProc, hButton, Msg, wParam, lParam );
}

// Helper to fill SYSTEMTIME from date/datetime
static void FillSystemTime( SYSTEMTIME *sysTime, BOOL *bTimeToZero )
{
   if( HB_ISDATETIME( 2 ) )
   {
      int   iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;
#ifndef __XHARBOUR__
      hb_timeStampUnpack( hb_partd( 2 ), &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );
#else
      long  lJulian, lMilliSec;
      if( hb_partdt( &lJulian, &lMilliSec, 2 ) )
      {
         hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
         hb_timeStampDecode( lMilliSec, &iHour, &iMinute, &iSecond, &iMSec );
      }
#endif
      sysTime->wYear = ( WORD ) iYear;
      sysTime->wMonth = ( WORD ) iMonth;
      sysTime->wDay = ( WORD ) iDay;
      sysTime->wHour = ( WORD ) iHour;
      sysTime->wMinute = ( WORD ) iMinute;
      sysTime->wSecond = ( WORD ) iSecond;
      sysTime->wMilliseconds = ( WORD ) iMSec;
   }
   else if( HB_ISDATE( 2 ) )
   {
      long  lJulian = hb_pardl( 2 );
      int   iYear, iMonth, iDay;
      hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
      sysTime->wYear = ( WORD ) iYear;
      sysTime->wMonth = ( WORD ) iMonth;
      sysTime->wDay = ( WORD ) iDay;
      *bTimeToZero = TRUE;
   }
   else
   {
      sysTime->wYear = hmg_par_WORD_def( 2, 2005 );
      sysTime->wMonth = hmg_par_WORD_def( 3, 1 );
      sysTime->wDay = hmg_par_WORD_def( 4, 1 );
      if( hb_pcount() >= 7 )
      {
         sysTime->wHour = hmg_par_WORD( 5 );
         sysTime->wMinute = hmg_par_WORD( 6 );
         sysTime->wSecond = hmg_par_WORD( 7 );
         sysTime->wMilliseconds = hmg_par_WORD( 8 );
      }
      else
      {
         *bTimeToZero = TRUE;
      }
   }

   sysTime->wDayOfWeek = 0;
   if( *bTimeToZero )
   {
      sysTime->wHour = 0;
      sysTime->wMinute = 0;
      sysTime->wSecond = 0;
      sysTime->wMilliseconds = 0;
   }
}

// Set date
HB_FUNC( SETDATEPICK )
{
   SYSTEMTIME  sysTime = { 0 };

   if( hb_pcount() == 2 && HB_ISDATE( 2 ) )
   {
      long  lJulian = hb_pardl( 2 );
      int   iYear, iMonth, iDay;
      hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
      sysTime.wYear = ( WORD ) iYear;
      sysTime.wMonth = ( WORD ) iMonth;
      sysTime.wDay = ( WORD ) iDay;
   }
   else if( hb_pcount() > 2 )
   {
      sysTime.wYear = hmg_par_WORD( 2 );
      sysTime.wMonth = hmg_par_WORD( 3 );
      sysTime.wDay = hmg_par_WORD( 4 );
   }
   else
   {
      sysTime.wYear = 2005;
      sysTime.wMonth = 1;
      sysTime.wDay = 1;
   }

   hmg_ret_L( SendMessage( hmg_par_raw_HWND( 1 ), DTM_SETSYSTEMTIME, GDT_VALID, ( LPARAM ) &sysTime ) == GDT_VALID );
}

// Set time
HB_FUNC( SETTIMEPICK )
{
   SYSTEMTIME  sysTime = { 0 };
   sysTime.wYear = 2005;
   sysTime.wMonth = 1;
   sysTime.wDay = 1;
   sysTime.wDayOfWeek = 0;
   sysTime.wHour = hmg_par_WORD( 2 );
   sysTime.wMinute = hmg_par_WORD( 3 );
   sysTime.wSecond = hmg_par_WORD( 4 );
   sysTime.wMilliseconds = 0;

   hmg_ret_L( SendMessage( hmg_par_raw_HWND( 1 ), DTM_SETSYSTEMTIME, GDT_VALID, ( LPARAM ) &sysTime ) == GDT_VALID );
}

// Set full datetime
HB_FUNC( DTP_SETDATETIME )
{
   HWND        hwnd = hmg_par_raw_HWND( 1 );
   SYSTEMTIME  sysTime = { 0 };
   BOOL        bTimeToZero = FALSE;

   FillSystemTime( &sysTime, &bTimeToZero );

   hmg_ret_L( SendMessage( hwnd, DTM_SETSYSTEMTIME, GDT_VALID, ( LPARAM ) &sysTime ) == GDT_VALID );
}

// Get full datetime
HB_FUNC( DTP_GETDATETIME )
{
   SYSTEMTIME  st;
   SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) &st );

#ifdef __XHARBOUR__
   hb_retdtl( hb_dateEncode( st.wYear, st.wMonth, st.wDay ), hb_timeStampEncode( st.wHour, st.wMinute, st.wSecond, st.wMilliseconds ) );
#else
   hb_rettd( hb_timeStampPack( st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond, st.wMilliseconds ) );
#endif
}

// Simple getters
HB_FUNC( GETDATEPICKDATE )
{
   SYSTEMTIME  st;
   SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) &st );
   hb_retd( st.wYear, st.wMonth, st.wDay );
}

HB_FUNC( GETDATEPICKYEAR )
{
   SYSTEMTIME  st;
   SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) &st );
   hmg_ret_WORD( st.wYear );
}

HB_FUNC( GETDATEPICKMONTH )
{
   SYSTEMTIME  st;
   SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) &st );
   hmg_ret_WORD( st.wMonth );
}

HB_FUNC( GETDATEPICKDAY )
{
   SYSTEMTIME  st;
   SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) &st );
   hmg_ret_WORD( st.wDay );
}

HB_FUNC( GETDATEPICKHOUR )
{
   SYSTEMTIME  st;

   // Check if system time is valid for the date picker control.
   if( SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) &st ) == GDT_VALID )
   {
      // Return the hour part (wHour) from SYSTEMTIME.
      hmg_ret_WORD( st.wHour );
   }
   else
   {
      hb_retni( -1 );  // Return -1 if invalid.
   }
}

HB_FUNC( GETDATEPICKMINUTE )
{
   SYSTEMTIME  st;

   // Check if system time is valid for the date picker control.
   if( SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) &st ) == GDT_VALID )
   {
      // Return the minute part (wMinute) from SYSTEMTIME.
      hmg_ret_WORD( st.wMinute );
   }
   else
   {
      hb_retni( -1 );  // Return -1 if invalid.
   }
}

HB_FUNC( GETDATEPICKSECOND )
{
   SYSTEMTIME  st;

   // Check if system time is valid for the date picker control.
   if( SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) &st ) == GDT_VALID )
   {
      // Return the second part (wSecond) from SYSTEMTIME.
      hmg_ret_WORD( st.wSecond );
   }
   else
   {
      hb_retni( -1 );  // Return -1 if invalid.
   }
}

// Other functions
HB_FUNC( SETDATEPICKNULL )
{
   hb_retl( ( BOOL ) SendMessage( hmg_par_raw_HWND( 1 ), DTM_SETSYSTEMTIME, GDT_NONE, 0 ) );
}

HB_FUNC( SETDATEPICKRANGE )
{
   SYSTEMTIME  sysTime[2];
   char        *cDate;
   DWORD       y, m, d;
   WPARAM      wLimit = 0;

   if( HB_ISDATE( 2 ) && HB_ISDATE( 3 ) )
   {
      cDate = ( char * ) hb_pards( 2 );
      if( cDate[0] != ' ' )
      {
         y = ( DWORD ) ( ( cDate[0] - '0' ) * 1000 + ( cDate[1] - '0' ) * 100 + ( cDate[2] - '0' ) * 10 + ( cDate[3] - '0' ) );
         sysTime[0].wYear = ( WORD ) y;
         m = ( DWORD ) ( ( cDate[4] - '0' ) * 10 + ( cDate[5] - '0' ) );
         sysTime[0].wMonth = ( WORD ) m;
         d = ( DWORD ) ( ( cDate[6] - '0' ) * 10 + ( cDate[7] - '0' ) );
         sysTime[0].wDay = ( WORD ) d;
         wLimit |= GDTR_MIN;
      }

      cDate = ( char * ) hb_pards( 3 );
      if( cDate[0] != ' ' )
      {
         y = ( DWORD ) ( ( cDate[0] - '0' ) * 1000 + ( cDate[1] - '0' ) * 100 + ( cDate[2] - '0' ) * 10 + ( cDate[3] - '0' ) );
         sysTime[1].wYear = ( WORD ) y;
         m = ( DWORD ) ( ( cDate[4] - '0' ) * 10 + ( cDate[5] - '0' ) );
         sysTime[1].wMonth = ( WORD ) m;
         d = ( DWORD ) ( ( cDate[6] - '0' ) * 10 + ( cDate[7] - '0' ) );
         sysTime[1].wDay = ( WORD ) d;
         wLimit |= GDTR_MAX;
      }

      hb_retl( DateTime_SetRange( hmg_par_raw_HWND( 1 ), wLimit, sysTime ) );
   }
}

HB_FUNC( SETDATEPICKERDATEFORMAT )
{
#ifndef UNICODE
   LPCSTR   lpFormat = hb_parc( 2 );
#else
   LPCWSTR  lpFormat = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   hb_retl( ( BOOL ) SendMessage( hmg_par_raw_HWND( 1 ), DTM_SETFORMAT, 0, ( LPARAM ) lpFormat ) );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpFormat );
#endif
}

HB_FUNC( DTP_ISCHECKED )
{
   SYSTEMTIME  st;
   hmg_ret_L( SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) &st ) == GDT_VALID );
}
