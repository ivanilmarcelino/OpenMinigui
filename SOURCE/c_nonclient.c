/*----------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   (c) Grigory Filatov <gfilatov@gmail.com> HMG 18.05

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
#define _WIN32_IE 0x0501

#include <mgdefs.h>

#ifdef UNICODE
LPWSTR   AnsiToWide( LPCSTR );
LPSTR    WideToAnsi( LPWSTR );
#endif

/*
   HB_FUNC(GETNONCLIENT)

   Retrieves the current non-client metrics settings from the system.
   These metrics define the sizes and dimensions of various non-client elements of windows,
   such as borders, scroll bars, and caption areas.

   Parameters:
       None

   Return Value:
       Returns an array containing the non-client metrics. The array elements are:
       1: iBorderWidth - Width of the window border.
       2: iScrollWidth - Width of the vertical scroll bar.
       3: iScrollHeight - Height of the horizontal scroll bar.
       4: iCaptionWidth - Width of the caption button.
       5: iCaptionHeight - Height of the caption area.
       6: iMenuWidth - Width of the menu button.
       7: iMenuHeight - Height of the menu area.

   Purpose:
       This function is used to obtain the system-wide settings for non-client areas of windows,
       allowing applications to adapt their user interface to match the user's preferences and system settings.
       It retrieves the NONCLIENTMETRICS structure using SystemParametersInfo and then populates a Harbour array
       with the relevant values.
*/
HB_FUNC( GETNONCLIENT )
{
   NONCLIENTMETRICS  ncm;

   memset( ( char * ) &ncm, 0, sizeof( NONCLIENTMETRICS ) );
   ncm.cbSize = sizeof( NONCLIENTMETRICS );

   SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, &ncm, 0 );

   hb_reta( 7 );
   HB_STORVNL( ncm.iBorderWidth, -1, 1 );
   HB_STORVNL( ncm.iScrollWidth, -1, 2 );
   HB_STORVNL( ncm.iScrollHeight, -1, 3 );
   HB_STORVNL( ncm.iCaptionWidth, -1, 4 );
   HB_STORVNL( ncm.iCaptionHeight, -1, 5 );
   HB_STORVNL( ncm.iMenuWidth, -1, 6 );
   HB_STORVNL( ncm.iMenuHeight, -1, 7 );
}

/*
   HB_FUNC(GETNONCLIENTFONT)

   Retrieves the font information for non-client areas of windows, such as the caption, menu, status, and message fonts.

   Parameters:
       1: nArea (Numeric) - Specifies the area for which to retrieve the font information.
          1: Caption Font
          2: Menu Font
          3: Status Font
          4: Message Font

   Return Value:
       Returns an array containing the font information for the specified area. The array elements are:
       1: Font Face Name (Character)
       2: Font Height (Numeric)
       3: Font Bold (Logical) - .T. if the font is bold, .F. otherwise.
       4: Font CharSet (Numeric)

   Purpose:
       This function allows applications to retrieve the system-wide font settings for different non-client areas.
       This enables applications to use the same fonts as the system for a consistent look and feel.
       It retrieves the NONCLIENTMETRICS structure using SystemParametersInfo, extracts the font information
       for the specified area, and then populates a Harbour array with the font's face name, height, weight (bold), and charset.
       The function handles Unicode conversions if the UNICODE preprocessor directive is defined.
*/
HB_FUNC( GETNONCLIENTFONT )
{
#ifdef UNICODE
   LPSTR             pStr;
#endif
   NONCLIENTMETRICS  ncm;

   memset( ( char * ) &ncm, 0, sizeof( NONCLIENTMETRICS ) );
   ncm.cbSize = sizeof( NONCLIENTMETRICS );

   SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, &ncm, 0 );

   hb_reta( 4 );

   switch( hb_parni( 1 ) )
   {
      case 1:
   #ifndef UNICODE
         HB_STORC( ncm.lfCaptionFont.lfFaceName, -1, 1 );
   #else
         pStr = WideToAnsi( ncm.lfCaptionFont.lfFaceName );
         HB_STORC( pStr, -1, 1 );
         hb_xfree( pStr );
   #endif
         HB_STORNI( ( int ) -0.75 * ncm.lfCaptionFont.lfHeight, -1, 2 );
         HB_STORL( ncm.lfCaptionFont.lfWeight == 700 ? TRUE : FALSE, -1, 3 );
         HB_STORNI( ncm.lfCaptionFont.lfCharSet, -1, 4 );
         break;

      case 2:
   #ifndef UNICODE
         HB_STORC( ncm.lfMenuFont.lfFaceName, -1, 1 );
   #else
         pStr = WideToAnsi( ncm.lfMenuFont.lfFaceName );
         HB_STORC( pStr, -1, 1 );
         hb_xfree( pStr );
   #endif
         HB_STORNI( ( int ) -0.75 * ncm.lfMenuFont.lfHeight, -1, 2 );
         HB_STORL( ncm.lfMenuFont.lfWeight == 700 ? TRUE : FALSE, -1, 3 );
         HB_STORNI( ncm.lfMenuFont.lfCharSet, -1, 4 );
         break;

      case 3:
   #ifndef UNICODE
         HB_STORC( ncm.lfStatusFont.lfFaceName, -1, 1 );
   #else
         pStr = WideToAnsi( ncm.lfStatusFont.lfFaceName );
         HB_STORC( pStr, -1, 1 );
         hb_xfree( pStr );
   #endif
         HB_STORNI( ( int ) -0.75 * ncm.lfStatusFont.lfHeight, -1, 2 );
         HB_STORL( ncm.lfStatusFont.lfWeight == 700 ? TRUE : FALSE, -1, 3 );
         HB_STORNI( ncm.lfStatusFont.lfCharSet, -1, 4 );
         break;

      case 4:
   #ifndef UNICODE
         HB_STORC( ncm.lfMessageFont.lfFaceName, -1, 1 );
   #else
         pStr = WideToAnsi( ncm.lfMessageFont.lfFaceName );
         HB_STORC( pStr, -1, 1 );
         hb_xfree( pStr );
   #endif
         HB_STORNI( ( int ) -0.75 * ncm.lfMessageFont.lfHeight, -1, 2 );
         HB_STORL( ncm.lfMessageFont.lfWeight == 700 ? TRUE : FALSE, -1, 3 );
         HB_STORNI( ncm.lfMessageFont.lfCharSet, -1, 4 );
         break;
   }
}

/*
   HB_FUNC(SETNONCLIENT)

   Sets the non-client metrics settings for the system.
   These metrics define the sizes and dimensions of various non-client elements of windows,
   such as borders, scroll bars, and caption areas.

   Parameters:
       1: nArea (Numeric) - Specifies the area to set.
          1: Border Width
          2: Scroll Bar Width/Height
          3: Caption Width
          4: Caption Height
          5: Menu Width/Height
       2: nValue (Numeric) - The new value for the specified area.

   Return Value:
       None

   Purpose:
       This function allows applications to modify the system-wide settings for non-client areas of windows.
       This can be used to customize the appearance of windows.
       It retrieves the current NONCLIENTMETRICS structure using SystemParametersInfo, modifies the specified
       metric, and then sets the updated structure using SystemParametersInfo.
       The function limits the values to reasonable ranges to prevent unexpected behavior.
*/
HB_FUNC( SETNONCLIENT )
{
   NONCLIENTMETRICS  ncm;

   memset( ( char * ) &ncm, 0, sizeof( NONCLIENTMETRICS ) );
   ncm.cbSize = sizeof( NONCLIENTMETRICS );
   SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, &ncm, 0 );

   switch( hb_parni( 1 ) )
   {
      case 1:
         ncm.iBorderWidth = HB_MIN( 50, HB_MAX( 1, hb_parni( 2 ) ) );
         break;

      case 2:
         ncm.iScrollWidth = HB_MIN( 100, HB_MAX( 8, hb_parni( 2 ) ) );
         ncm.iScrollHeight = HB_MIN( 100, HB_MAX( 8, hb_parni( 2 ) ) );
         break;

      case 3:
         ncm.iCaptionWidth = HB_MIN( 100, HB_MAX( 17, hb_parni( 2 ) ) );
         break;

      case 4:
         ncm.iCaptionHeight = HB_MIN( 100, HB_MAX( 17, hb_parni( 2 ) ) );
         break;

      case 5:
         ncm.iMenuWidth = HB_MIN( 100, HB_MAX( 17, hb_parni( 2 ) ) );
         ncm.iMenuHeight = HB_MIN( 100, HB_MAX( 17, hb_parni( 2 ) ) );
         break;
   }

   SystemParametersInfo( SPI_SETNONCLIENTMETRICS, sizeof( ncm ), &ncm, 0 );
}

/*
   HB_FUNC(SETNONCLIENTFONT)

   Sets the font information for non-client areas of windows, such as the caption, menu, status, and message fonts.

   Parameters:
       1: nArea (Numeric) - Specifies the area for which to set the font information.
          1: Caption Font
          2: Menu Font
          3: Status Font
          4: Message Font
       2: cFaceName (Character) - The name of the font face.
       3: nHeight (Numeric) - The height of the font in points.
       4: lBold (Logical) - .T. if the font should be bold, .F. otherwise.
       5: nCharSet (Numeric) - The character set of the font.

   Return Value:
       None

   Purpose:
       This function allows applications to modify the system-wide font settings for different non-client areas.
       This can be used to customize the appearance of windows.
       It retrieves the current NONCLIENTMETRICS structure using SystemParametersInfo, modifies the font information
       for the specified area, and then sets the updated structure using SystemParametersInfo.
       The function converts the font height from points to logical units using MulDiv and GetDeviceCaps.
       It also handles Unicode conversions if the UNICODE preprocessor directive is defined.
*/
HB_FUNC( SETNONCLIENTFONT )
{
#ifdef UNICODE
   LPWSTR            pWStr;
#endif
   HDC               hDC = GetDC( HWND_DESKTOP );
   NONCLIENTMETRICS  ncm;
   LOGFONT           lf;

   memset( ( char * ) &ncm, 0, sizeof( NONCLIENTMETRICS ) );
   ncm.cbSize = sizeof( NONCLIENTMETRICS );
   SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, &ncm, 0 );

   memset( &lf, 0, sizeof( LOGFONT ) );
#ifdef UNICODE
   pWStr = AnsiToWide( hb_parc( 2 ) );
   lstrcpy( lf.lfFaceName, pWStr );
   hb_xfree( pWStr );
#else
   lstrcpy( lf.lfFaceName, hb_parc( 2 ) );
#endif
   lf.lfHeight = -MulDiv( hb_parni( 3 ), GetDeviceCaps( hDC, LOGPIXELSY ), 72 );
   lf.lfWeight = ( HB_ISLOG( 4 ) && hb_parl( 4 ) ) ? 700 : 400;
   lf.lfCharSet = HB_ISNIL( 5 ) ? ( BYTE ) 0 : hmg_par_BYTE( 5 );

   switch( hb_parni( 1 ) )
   {
      case 1:
         ncm.lfCaptionFont = lf;
         break;

      case 2:
         ncm.lfMenuFont = lf;
         break;

      case 3:
         ncm.lfStatusFont = lf;
         break;

      case 4:
         ncm.lfMessageFont = lf;
         break;
   }

   SystemParametersInfo( SPI_SETNONCLIENTMETRICS, sizeof( ncm ), &ncm, 0 );

   ReleaseDC( HWND_DESKTOP, hDC );
}
