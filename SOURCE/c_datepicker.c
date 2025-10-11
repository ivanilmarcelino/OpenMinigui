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
   Copyright 2005 (C) Jacek Kubica <kubica@wssk.wroc.pl>
 ---------------------------------------------------------------------------*/
// Define the minimum required version of Internet Explorer (IE 5.01 in this case)
#define _WIN32_IE 0x0501

// Include essential header files needed for the program
#include <mgdefs.h>        // Contains basic macro definitions and common includes
#include <commctrl.h>      // Contains definitions for common controls, such as date picker

// Harbour language header files for VM and date handling functions
#include "hbvm.h"
#include "hbdate.h"

// Declare function prototypes and definitions
#ifdef UNICODE
// Function to convert ANSI string to Wide (Unicode) string
LPWSTR AnsiToWide( LPCSTR );
#endif

// Function to retrieve the current instance handle of the application
HINSTANCE GetInstance( void );

// Custom window procedure for handling events specific to the date picker control
LRESULT CALLBACK OwnPickProc( HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam );

// XHarbour and Harbour compatibility
#ifdef __XHARBOUR__
// If using xHarbour, define the date-time check macro directly
#define HB_ISDATETIME ISDATETIME
#else
// If using Harbour, declare an external function for creating a timestamp from date and time components
extern HB_EXPORT double hb_timeStampPack( int iYear, int iMonth, int iDay, int iHour, int iMinutes, int iSeconds, int iMSec );
#endif

// Function to initialize a date picker control
HB_FUNC( INITDATEPICK )
{
   HWND hbutton;          // Handle to the date picker control
   DWORD Style = WS_CHILD; // Base window style for the control

   // Structure to initialize the common controls library
   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof( INITCOMMONCONTROLSEX ); // Size of the structure
   i.dwICC = ICC_DATE_CLASSES;                // Specify that date picker controls are to be initialized
   InitCommonControlsEx( &i );                // Initialize the common controls library

   // Conditional checks for various styles based on parameters passed to INITDATEPICK
   if( hb_parl( 9 ) ) // If the 9th parameter is true, set the DTS_SHOWNONE style (to allow a "none" state)
   {
      Style |= DTS_SHOWNONE;
   }

   if( hb_parl( 10 ) ) // If the 10th parameter is true, set the DTS_UPDOWN style (for an up/down arrow button)
   {
      Style |= DTS_UPDOWN;
   }

   if( hb_parl( 11 ) ) // If the 11th parameter is true, set the DTS_RIGHTALIGN style (right-aligned dropdown)
   {
      Style |= DTS_RIGHTALIGN;
   }

   if( !hb_parl( 12 ) ) // If the 12th parameter is false, set WS_VISIBLE style to make the control visible
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 13 ) ) // If the 13th parameter is false, set WS_TABSTOP to enable tabbing to the control
   {
      Style |= WS_TABSTOP;
   }

   // Create the date picker control with the specified styles and dimensions from parameters
   hbutton = CreateWindowEx
      (
         WS_EX_CLIENTEDGE,                // Extended window style for a border around the control
         DATETIMEPICK_CLASS,              // Class name for date-time picker control
         TEXT( "DateTime" ),              // Window name (title), set to "DateTime" here
         Style,                           // Combined window style
         hb_parni( 3 ),                   // X-position from 3rd parameter
         hb_parni( 4 ),                   // Y-position from 4th parameter
         hb_parni( 5 ),                   // Width from 5th parameter
         hb_parni( 6 ),                   // Height from 6th parameter
         hmg_par_raw_HWND( 1 ),           // Parent window handle from 1st parameter
         hmg_par_raw_HMENU( 2 ),          // Menu handle from 2nd parameter
         GetInstance(),                   // Application instance handle
         NULL                             // No additional data passed to the control
      );

   // Store the original window procedure for the date picker control, which will be needed for subclassing
   SetProp( hbutton, TEXT( "oldpickproc" ), ( HANDLE ) ( LONG_PTR ) GetWindowLongPtr( hbutton, GWLP_WNDPROC ) );

   // Subclass the control to use the custom window procedure for handling specific messages
   SubclassWindow2( hbutton, OwnPickProc );

   // Return the handle to the created date picker control
   hmg_ret_raw_HWND( hbutton );
}

// Function to initialize a time picker control
HB_FUNC( INITTIMEPICK )
{
   HWND hbutton;               // Handle for the time picker control
   DWORD Style = WS_CHILD | DTS_TIMEFORMAT; // Initial style with child and time format display

   // Initialize common controls for date and time picker classes
   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof( INITCOMMONCONTROLSEX ); // Size of the INITCOMMONCONTROLSEX structure
   i.dwICC = ICC_DATE_CLASSES;                // Indicate we need date/time control classes
   InitCommonControlsEx( &i );                // Initialize the common controls library

   // Additional style flags based on input parameters
   if( hb_parl( 9 ) ) // If parameter 9 is true, allow a "None" option
   {
      Style |= DTS_SHOWNONE;
   }

   if( !hb_parl( 10 ) ) // If parameter 10 is false, make the control visible
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 11 ) ) // If parameter 11 is false, add tab-stop functionality
   {
      Style |= WS_TABSTOP;
   }

   // Create the time picker control with specified styles and positions from parameters
   hbutton = CreateWindowEx
      (
         WS_EX_CLIENTEDGE,                // Extended style with a client edge border
         DATETIMEPICK_CLASS,              // Class name for date-time picker control
         TEXT( "DateTime" ),              // Control title
         Style,                           // Combined styles for the control
         hb_parni( 3 ),                   // X-position from parameter 3
         hb_parni( 4 ),                   // Y-position from parameter 4
         hb_parni( 5 ),                   // Width from parameter 5
         hb_parni( 6 ),                   // Height from parameter 6
         hmg_par_raw_HWND( 1 ),           // Parent window handle from parameter 1
         hmg_par_raw_HMENU( 2 ),          // Menu handle from parameter 2
         GetInstance(),                   // Current application instance
         NULL                             // No extra parameters
      );

   // Store the original window procedure for subclassing later
   SetProp( hbutton, TEXT( "oldpickproc" ), ( HANDLE ) ( LONG_PTR ) GetWindowLongPtr( hbutton, GWLP_WNDPROC ) );

   // Subclass the control to allow custom event handling
   SubclassWindow2( hbutton, OwnPickProc );

   // Return the handle to the created control
   hmg_ret_raw_HWND( hbutton );
}

// Custom window procedure for handling time picker messages
LRESULT CALLBACK OwnPickProc( HWND hButton, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB pSymbol = NULL;  // Static symbol for Harbor VM events
   LRESULT r;                       // Result of message processing
   WNDPROC OldWndProc;              // Pointer to original window procedure

   // Retrieve the stored original window procedure
   OldWndProc = ( WNDPROC ) ( LONG_PTR ) GetProp( hButton, TEXT( "oldpickproc" ) );

   // Handle specific messages
   switch( Msg )
   {
      case WM_DESTROY: // When the control is being destroyed
         SubclassWindow2( hButton, OldWndProc );          // Restore the original window procedure
         RemoveProp( hButton, TEXT( "oldpickproc" ) );    // Remove the old procedure property
         break;

      case WM_ERASEBKGND: // When erasing the background
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OPICKEVENTS" ) ); // Initialize the symbol if needed
         }

         if( pSymbol ) // If symbol is set, push parameters to the VM and call event handler
         {
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hButton );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }

         r = hmg_par_LRESULT( -1 ); // Get the return result for handling

         if( r != 0 ) // If custom result is not zero, return it
         {
            return r;
         }
         else
         {
            return CallWindowProc( OldWndProc, hButton, Msg, wParam, lParam ); // Otherwise, call the original procedure
         }
   }

   // Default case: call the original window procedure
   return CallWindowProc( OldWndProc, hButton, Msg, wParam, lParam );
}

// Function to set a specific date in a date picker control
HB_FUNC( SETDATEPICK )
{
   SYSTEMTIME sysTime;  // System time structure to hold date values

   if( hb_pcount() == 2 && HB_ISDATE( 2 ) ) // If two parameters and the second is a date
   {
      long lJulian;
      int iYear, iMonth, iDay;

      lJulian = hb_pardl( 2 );              // Get Julian date
      hb_dateDecode( lJulian, &iYear, &iMonth, &iDay ); // Decode to year, month, day

      sysTime.wYear = ( WORD ) iYear;
      sysTime.wMonth = ( WORD ) iMonth;
      sysTime.wDay = ( WORD ) iDay;
   }
   else if( hb_pcount() > 2 ) // If more parameters, extract date from parameters
   {
      sysTime.wYear = hmg_par_WORD( 2 );
      sysTime.wMonth = hmg_par_WORD( 3 );
      sysTime.wDay = hmg_par_WORD( 4 );
   }
   else // Default date if no parameters
   {
      sysTime.wYear = 2005;
      sysTime.wMonth = 1;
      sysTime.wDay = 1;
   }

   sysTime.wDayOfWeek = 0;
   sysTime.wHour = 0;
   sysTime.wMinute = 0;
   sysTime.wSecond = 0;
   sysTime.wMilliseconds = 0;

   // Set the time in the date picker control and return success/failure as boolean
   hmg_ret_L( SendMessage( hmg_par_raw_HWND( 1 ), DTM_SETSYSTEMTIME, GDT_VALID, ( LPARAM ) & sysTime ) == GDT_VALID );
}

// Function to set time in a time picker control
HB_FUNC( SETTIMEPICK )
{
   SYSTEMTIME sysTime; // Structure to hold system time

   sysTime.wYear = 2005;        // Default year
   sysTime.wMonth = 1;          // Default month
   sysTime.wDay = 1;            // Default day
   sysTime.wDayOfWeek = 0;

   sysTime.wHour = hmg_par_WORD( 2 );       // Hour from parameter
   sysTime.wMinute = hmg_par_WORD( 3 );     // Minute from parameter
   sysTime.wSecond = hmg_par_WORD( 4 );     // Second from parameter
   sysTime.wMilliseconds = 0;

   // Set the time in the control and return success/failure
   hmg_ret_L( SendMessage( hmg_par_raw_HWND( 1 ), DTM_SETSYSTEMTIME, GDT_VALID, ( LPARAM ) & sysTime ) == GDT_VALID );
}

// Function to get date from a date picker control and return as date value
HB_FUNC( GETDATEPICKDATE )
{
   SYSTEMTIME st; // Structure to hold system time

   SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) & st ); // Retrieve date

   hb_retd( st.wYear, st.wMonth, st.wDay ); // Return date as encoded format
}

// Function to get the year from a date picker control and return it as a WORD value.
HB_FUNC( GETDATEPICKYEAR )
{
   SYSTEMTIME  st;

   // Send a message to get the current system time of the date picker control.
   SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) & st );

   // Return the year part (wYear) from SYSTEMTIME.
   hmg_ret_WORD( st.wYear );
}

// Function to get the month from a date picker control and return it as a WORD value.
HB_FUNC( GETDATEPICKMONTH )
{
   SYSTEMTIME  st;

   // Send a message to get the current system time of the date picker control.
   SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) & st );

   // Return the month part (wMonth) from SYSTEMTIME.
   hmg_ret_WORD( st.wMonth );
}

// Function to get the day from a date picker control and return it as a WORD value.
HB_FUNC( GETDATEPICKDAY )
{
   SYSTEMTIME  st;

   // Send a message to get the current system time of the date picker control.
   SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) & st );

   // Return the day part (wDay) from SYSTEMTIME.
   hmg_ret_WORD( st.wDay );
}

// Function to get the hour from a date picker control; returns -1 if invalid.
HB_FUNC( GETDATEPICKHOUR )
{
   SYSTEMTIME  st;

   // Send a message to retrieve the system time and check if it is valid.
   if( SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) & st ) == GDT_VALID )
   {
      // Return the hour part (wHour) from SYSTEMTIME.
      hmg_ret_WORD( st.wHour );
   }
   else
   {
      // Return -1 if the date picker value is invalid.
      hb_retni( -1 );
   }
}

// Function to get the minute from a date picker control; returns -1 if invalid.
HB_FUNC( GETDATEPICKMINUTE )
{
   SYSTEMTIME  st;

   // Check if system time is valid for the date picker control.
   if( SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) & st ) == GDT_VALID )
   {
      // Return the minute part (wMinute) from SYSTEMTIME.
      hmg_ret_WORD( st.wMinute );
   }
   else
   {
      hb_retni( -1 );  // Return -1 if invalid.
   }
}

// Function to get the second from a date picker control; returns -1 if invalid.
HB_FUNC( GETDATEPICKSECOND )
{
   SYSTEMTIME  st;

   // Retrieve and check system time validity for the date picker control.
   if( SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) & st ) == GDT_VALID )
   {
      // Return the second part (wSecond) from SYSTEMTIME.
      hmg_ret_WORD( st.wSecond );
   }
   else
   {
      hb_retni( -1 );  // Return -1 if invalid.
   }
}

// Function to set the date and time for a date picker control.
HB_FUNC( DTP_SETDATETIME )
{
   HWND        hwnd;
   SYSTEMTIME  sysTime;
   BOOL        bTimeToZero = FALSE;

   hwnd = hmg_par_raw_HWND( 1 );  // Get handle for the date picker control.

   // Check if input is a valid date and time value.
   if( HB_ISDATETIME( 2 ) )
   {
      int iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;
#ifdef __XHARBOUR__
      long lJulian, lMilliSec;
#endif
#ifndef __XHARBOUR__
      // Unpack the date and time into individual components.
      hb_timeStampUnpack( hb_partd( 2 ), &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );
#else
      if( hb_partdt( &lJulian, &lMilliSec, 2 ) )
      {
         // Decode date and timestamp if using xHarbour.
         hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
         hb_timeStampDecode( lMilliSec, &iHour, &iMinute, &iSecond, &iMSec );
      }
#endif
      // Set SYSTEMTIME structure fields for year, month, day, hour, minute, second, and milliseconds.
      sysTime.wYear = ( WORD ) iYear;
      sysTime.wMonth = ( WORD ) iMonth;
      sysTime.wDay = ( WORD ) iDay;
      sysTime.wDayOfWeek = 0;
      sysTime.wHour = ( WORD ) iHour;
      sysTime.wMinute = ( WORD ) iMinute;
      sysTime.wSecond = ( WORD ) iSecond;
      sysTime.wMilliseconds = ( WORD ) iMSec;
   }
   else if( HB_ISDATE( 2 ) )
   {
      long  lJulian;
      int   iYear, iMonth, iDay;

      lJulian = hb_pardl( 2 ); // Extract Julian date.
      hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );

      // Set year, month, and day, and indicate no time is specified.
      sysTime.wYear = ( WORD ) iYear;
      sysTime.wMonth = ( WORD ) iMonth;
      sysTime.wDay = ( WORD ) iDay;
      sysTime.wDayOfWeek = 0;
      bTimeToZero = TRUE;
   }
   else
   {
      // Set default date if no valid date or datetime is provided.
      sysTime.wYear = hmg_par_WORD_def( 2, 2005 );
      sysTime.wMonth = hmg_par_WORD_def( 3, 1 );
      sysTime.wDay = hmg_par_WORD_def( 4, 1 );
      sysTime.wDayOfWeek = 0;

      if( hb_pcount() >= 7 )
      {
         // Set hour, minute, second, and milliseconds if specified.
         sysTime.wHour = hmg_par_WORD( 5 );
         sysTime.wMinute = hmg_par_WORD( 6 );
         sysTime.wSecond = hmg_par_WORD( 7 );
         sysTime.wMilliseconds = hmg_par_WORD( 8 );
      }
      else
      {
         bTimeToZero = TRUE;
      }
   }

   // Zero out time if specified.
   if( bTimeToZero )
   {
      sysTime.wHour = 0;
      sysTime.wMinute = 0;
      sysTime.wSecond = 0;
      sysTime.wMilliseconds = 0;
   }

   // Send message to set system time on the date picker control.
   hmg_ret_L( SendMessage( hwnd, DTM_SETSYSTEMTIME, GDT_VALID, ( LPARAM ) & sysTime ) == GDT_VALID );
}

// Function to get the full date and time from a date picker control.
HB_FUNC( DTP_GETDATETIME )
{
   SYSTEMTIME  st;

   // Send message to retrieve system time for the date picker control.
   SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) & st );

#ifdef __XHARBOUR__
   // Return date and time as a Julian and timestamp combination in xHarbour.
   hb_retdtl( hb_dateEncode( st.wYear, st.wMonth, st.wDay ), hb_timeStampEncode( st.wHour, st.wMinute, st.wSecond, st.wMilliseconds ) );
#else
   // Return date and time as a timestamp.
   hb_rettd( hb_timeStampPack( st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond, st.wMilliseconds ) );
#endif
}

// Function to set the date picker control to null.
HB_FUNC( SETDATEPICKNULL )
{
   // Send message to set the date picker control value to null.
   hb_retl( ( BOOL ) SendMessage( hmg_par_raw_HWND( 1 ), DTM_SETSYSTEMTIME, GDT_NONE, ( LPARAM ) 0 ) );
}

// Function to set the date range on a date picker control.
HB_FUNC( SETDATEPICKRANGE )
{
   SYSTEMTIME  sysTime[2];
   char        *cDate;
   DWORD       y, m, d;
   WPARAM      wLimit = 0;

   if( HB_ISDATE( 2 ) && HB_ISDATE( 3 ) )
   {
      cDate = ( char * ) hb_pards( 2 );
      if( !( cDate[0] == ' ' ) )
      {
         // Extract and set minimum date (sysTime[0]).
         y = ( DWORD ) ( ( cDate[0] - '0' ) * 1000 ) + ( ( cDate[1] - '0' ) * 100 ) + ( ( cDate[2] - '0' ) * 10 ) + ( cDate[3] - '0' );
         sysTime[0].wYear = ( WORD ) y;
         m = ( DWORD ) ( ( cDate[4] - '0' ) * 10 ) + ( cDate[5] - '0' );
         sysTime[0].wMonth = ( WORD ) m;
         d = ( DWORD ) ( ( cDate[6] - '0' ) * 10 ) + ( cDate[7] - '0' );
         sysTime[0].wDay = ( WORD ) d;
         wLimit |= GDTR_MIN;
      }

      // Extract and set maximum date (sysTime[1]).
      cDate = ( char * ) hb_pards( 3 );
      if( !( cDate[0] == ' ' ) )
      {
         y = ( DWORD ) ( ( cDate[0] - '0' ) * 1000 ) + ( ( cDate[1] - '0' ) * 100 ) + ( ( cDate[2] - '0' ) * 10 ) + ( cDate[3] - '0' );
         sysTime[1].wYear = ( WORD ) y;
         m = ( DWORD ) ( ( cDate[4] - '0' ) * 10 ) + ( cDate[5] - '0' );
         sysTime[1].wMonth = ( WORD ) m;
         d = ( DWORD ) ( ( cDate[6] - '0' ) * 10 ) + ( cDate[7] - '0' );
         sysTime[1].wDay = ( WORD ) d;
         wLimit |= GDTR_MAX;
      }

      // Apply date range to date picker control.
      hb_retl( DateTime_SetRange( hmg_par_raw_HWND( 1 ), wLimit, sysTime ) );
   }
}

// Function to set the display format of a date picker control.
HB_FUNC( SETDATEPICKERDATEFORMAT )
{
#ifndef UNICODE
   LPCSTR   lpFormat = hb_parc( 2 );
#else
   // Convert ANSI string to wide string if using Unicode.
   LPCWSTR  lpFormat = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   // Set the format string for the date picker control.
   hb_retl( ( BOOL ) SendMessage( hmg_par_raw_HWND( 1 ), DTM_SETFORMAT, 0, ( LPARAM ) lpFormat ) );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpFormat );  // Free memory if Unicode conversion was used.
#endif
}

// Function to check if the date in the date picker control is valid and set.
HB_FUNC( DTP_ISCHECKED )
{
   SYSTEMTIME  st;

   // Return TRUE if system time is valid, FALSE otherwise.
   hmg_ret_L( SendMessage( hmg_par_raw_HWND( 1 ), DTM_GETSYSTEMTIME, 0, ( LPARAM ) & st ) == GDT_VALID );
}
