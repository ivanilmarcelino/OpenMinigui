/*----------------------------------------------------------------------------
   HMG - Harbour Windows GUI library source code

   Copyright 2002-2017 Roberto Lopez <mail.box.hmg@gmail.com>
   http://sites.google.com/site/hmgweb/

   Head of HMG project:

      2002-2012 Roberto Lopez <mail.box.hmg@gmail.com>
      http://sites.google.com/site/hmgweb/

      2012-2017 Dr. Claudio Soto <srvet@adinet.com.uy>
      http://srvet.blogspot.com

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
   contained in this release of HMG.

   The exception is that, if you link the HMG library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   HMG library code into it.

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
        Copyright 2001-2008 Alexander S.Kresin <alex@kresin.ru>

---------------------------------------------------------------------------*/
#include <mgdefs.h>
#include <commdlg.h>
#include <commctrl.h>

#if defined( _MSC_VER )
#pragma warning( push )
#pragma warning( disable : 4201 )   // warning C4201: nonstandard extension used: nameless struct/union
#endif
#include <richedit.h>
#if defined( _MSC_VER )
#pragma warning( pop )
#endif
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define PFNS_NEWNUMBER  0x8000      // Start new number with wNumberingStart
#endif
#include <shlwapi.h>

#ifdef __POCC__
#define PFNS_NEWNUMBER  0x8000
#endif
#if defined( __WATCOMC__ )
#define SF_USECODEPAGE  0x0020      // CodePage given by high word
#endif

// by Dr. Claudio Soto, January 2014
#ifndef CP_UNICODE
#define CP_UNICODE   1200           // The text is UTF-16 (the WCHAR data type)
#endif
#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );
LPSTR             WideToAnsi( LPWSTR );
#endif
HINSTANCE         GetInstance( void );
static HINSTANCE  hRELib = NULL;

/*
 * HB_FUNC( INITRICHEDITBOXEX )
 *
 * Initializes a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWnd: Handle to the parent window.
 *   2. HMENU hMenu: Handle to the menu.
 *   3. int nX: X-coordinate of the control.
 *   4. int nY: Y-coordinate of the control.
 *   5. int nWidth: Width of the control.
 *   6. int nHeight: Height of the control.
 *   7. int nID: Identifier of the control.
 *   8. int nMaxLength: Maximum length of the text.
 *   9. BOOL lReadOnly: Read-only flag.
 *   10. BOOL lVisible: Visibility flag.
 *   11. BOOL lTabStop: Tab stop flag.
 *   12. BOOL lHScroll: Horizontal scroll flag.
 *   13. BOOL lVScroll: Vertical scroll flag.
 *
 * Return Value:
 *   HWND: Handle to the created Rich Edit control.
 *
 * Purpose:
 *   This function creates and initializes a Rich Edit control with specified properties.
 */
HB_FUNC( INITRICHEDITBOXEX )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   HMENU hMenu = hmg_par_raw_HMENU( 2 );
   HWND  hWndControl = NULL;
   DWORD Style = ES_MULTILINE | ES_WANTRETURN | WS_CHILD | ES_NOHIDESEL | ( ( !hb_parl( 14 ) ) ? WS_VSCROLL : ES_AUTOVSCROLL );

   if( hb_parl( 10 ) )
   {
      Style |= ES_READONLY;
   }

   if( !hb_parl( 11 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 12 ) )
   {
      Style |= WS_TABSTOP;
   }

   if( !hb_parl( 13 ) )
   {
      Style |= WS_HSCROLL;
   }

   if( !hRELib )
   {
      hRELib = LoadLibrary( TEXT( "RichEd20.dll" ) );
   }

   if( hRELib )
   {
      hWndControl = CreateWindowEx
         (
            WS_EX_CLIENTEDGE,
            ( LPCTSTR ) RICHEDIT_CLASS,
            TEXT( "" ),
            Style,
            hb_parni( 3 ),
            hb_parni( 4 ),
            hb_parni( 5 ),
            hb_parni( 6 ),
            hWnd,
            hMenu,
            GetInstance(),
            NULL
         );

      SendMessage( hWndControl, EM_LIMITTEXT, ( WPARAM ) hb_parni( 9 ), 0 );
      SendMessage
      (
         hWndControl,
         EM_SETEVENTMASK,
         ( WPARAM ) 0,
         ( LPARAM ) ENM_CHANGE | ENM_SELCHANGE | ENM_PROTECTED | ENM_SCROLL | ENM_LINK | ENM_KEYEVENTS | ENM_REQUESTRESIZE | ENM_MOUSEEVENTS
      );
      SendMessage( hWndControl, EM_SETTYPOGRAPHYOPTIONS, TO_ADVANCEDTYPOGRAPHY, TO_ADVANCEDTYPOGRAPHY );
      RegisterClipboardFormat( CF_RTF );
      RegisterClipboardFormat( CF_RETEXTOBJ );
   }

   hmg_ret_raw_HWND( hWndControl );
}

/*
 * HB_FUNC( UNLOADRICHEDITEXLIB )
 *
 * Unloads the Rich Edit library.
 *
 * Parameters:
 *   None.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function unloads the Rich Edit library.
 */
HB_FUNC( UNLOADRICHEDITEXLIB )
{
   if( hRELib )
   {
      FreeLibrary( hRELib );
      hRELib = NULL;
   }
}

/*
 * DWORD CALLBACK EditStreamCallbackRead(DWORD_PTR dwCookie, LPBYTE lpBuff, LONG cb, LONG *pcb)
 *
 * Callback function for reading data from a file.
 *
 * Parameters:
 *   DWORD_PTR dwCookie: Handle to the file.
 *   LPBYTE lpBuff: Buffer to read data into.
 *   LONG cb: Number of bytes to read.
 *   LONG *pcb: Pointer to the number of bytes actually read.
 *
 * Return Value:
 *   DWORD: Returns 0 if successful, otherwise returns -1.
 *
 * Purpose:
 *   This function reads data from a file into a buffer.
 */
DWORD CALLBACK EditStreamCallbackRead( DWORD_PTR dwCookie, LPBYTE lpBuff, LONG cb, LONG *pcb )
{
   HANDLE   hFile = ( HANDLE ) dwCookie;
   if( ReadFile( hFile, ( LPVOID ) lpBuff, ( DWORD ) cb, ( LPDWORD ) pcb, NULL ) )
   {
      return 0;
   }
   else
   {
      return( DWORD ) - 1;
   }
}

/*
 * HB_FUNC( RICHEDITBOX_STREAMIN )
 *
 * Streams text into a Rich Edit control from a file.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. LPCSTR cFileName: Name of the file to stream in.
 *   3. BOOL lSelection: Flag to indicate if the selection should be replaced.
 *   4. LONG nDataFormat: Format of the data.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function streams text into a Rich Edit control from a file.
 */
HB_FUNC( RICHEDITBOX_STREAMIN )
{
   HWND        hWndControl = hmg_par_raw_HWND( 1 );
#ifndef UNICODE
   LPCSTR      cFileName = ( char * ) hb_parc( 2 );
#else
   LPCWSTR     cFileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   BOOL        lSelection = ( BOOL ) hb_parl( 3 );
   LONG        nDataFormat = hmg_par_LONG( 4 );
   HANDLE      hFile;
   EDITSTREAM  es;
   LONG        Format;

   switch( nDataFormat )
   {
      case 1:
         Format = SF_TEXT;
         break;                     // ANSI and UTF-8 with BOM

      case 2:
         Format = ( CP_UTF8 << 16 ) | SF_USECODEPAGE | SF_TEXT;
         break;                     // ANSI and UTF-8 without BOM

      case 3:
         Format = SF_TEXT | SF_UNICODE;
         break;                     // UTF-16 LE

      case 4:
         Format = SF_RTF;
         break;

      case 5:
         Format = ( CP_UTF8 << 16 ) | SF_USECODEPAGE | SF_RTF;
         break;

      default:
         Format = SF_RTF;
         break;
   }

   if( lSelection )
   {
      Format |= SFF_SELECTION;
   }

   if( ( hFile = CreateFile( cFileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL ) ) == INVALID_HANDLE_VALUE )
   {
      hb_retl( FALSE );
      return;
   }

#ifdef UNICODE
   else
   {
      hb_xfree( ( TCHAR * ) cFileName );
   }
#endif
   es.pfnCallback = EditStreamCallbackRead;
   es.dwCookie = ( DWORD_PTR ) hFile;
   es.dwError = 0;
   SendMessage( hWndControl, EM_STREAMIN, ( WPARAM ) Format, ( LPARAM ) & es );
   CloseHandle( hFile );

   if( es.dwError )
   {
      hb_retl( FALSE );
   }
   else
   {
      hb_retl( TRUE );
   }
}

/*
 * DWORD CALLBACK EditStreamCallbackWrite(DWORD_PTR dwCookie, LPBYTE lpBuff, LONG cb, LONG *pcb)
 *
 * Callback function for writing data to a file.
 *
 * Parameters:
 *   DWORD_PTR dwCookie: Handle to the file.
 *   LPBYTE lpBuff: Buffer containing data to write.
 *   LONG cb: Number of bytes to write.
 *   LONG *pcb: Pointer to the number of bytes actually written.
 *
 * Return Value:
 *   DWORD: Returns 0 if successful, otherwise returns -1.
 *
 * Purpose:
 *   This function writes data from a buffer to a file.
 */
DWORD CALLBACK EditStreamCallbackWrite( DWORD_PTR dwCookie, LPBYTE lpBuff, LONG cb, LONG *pcb )
{
   HANDLE   hFile = ( HANDLE ) dwCookie;
   if( WriteFile( hFile, ( LPVOID ) lpBuff, ( DWORD ) cb, ( LPDWORD ) pcb, NULL ) )
   {
      return 0;
   }
   else
   {
      return( DWORD ) - 1;
   }
}

/*
 * HB_FUNC( RICHEDITBOX_STREAMOUT )
 *
 * Streams text from a Rich Edit control to a file.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. LPCSTR cFileName: Name of the file to stream out to.
 *   3. BOOL lSelection: Flag to indicate if the selection should be streamed out.
 *   4. LONG nDataFormat: Format of the data.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function streams text from a Rich Edit control to a file.
 */
HB_FUNC( RICHEDITBOX_STREAMOUT )
{
   HWND        hWndControl = hmg_par_raw_HWND( 1 );
#ifndef UNICODE
   LPCSTR      cFileName = ( char * ) hb_parc( 2 );
#else
   LPCWSTR     cFileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   BOOL        lSelection = ( BOOL ) hb_parl( 3 );
   LONG        nDataFormat = hmg_par_LONG( 4 );
   HANDLE      hFile;
   EDITSTREAM  es;
   LONG        Format;

   switch( nDataFormat )
   {
      case 1:
         Format = SF_TEXT;
         break;                     // ANSI and UTF-8 with BOM

      case 2:
         Format = ( CP_UTF8 << 16 ) | SF_USECODEPAGE | SF_TEXT;
         break;                     // ANSI and UTF-8 without BOM

      case 3:
         Format = SF_TEXT | SF_UNICODE;
         break;                     // UTF-16 LE

      case 4:
         Format = SF_RTF;
         break;

      case 5:
         Format = ( CP_UTF8 << 16 ) | SF_USECODEPAGE | SF_RTF;
         break;

      default:
         Format = SF_RTF;
         break;
   }

   if( lSelection )
   {
      Format |= SFF_SELECTION;
   }

   if( ( hFile = CreateFile( cFileName, GENERIC_WRITE, FILE_SHARE_WRITE, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL ) ) == INVALID_HANDLE_VALUE )
   {
      hb_retl( FALSE );
      return;
   }

#ifdef UNICODE
   else
   {
      hb_xfree( ( TCHAR * ) cFileName );
   }
#endif
   es.pfnCallback = EditStreamCallbackWrite;
   es.dwCookie = ( DWORD_PTR ) hFile;
   es.dwError = 0;
   SendMessage( hWndControl, EM_STREAMOUT, ( WPARAM ) Format, ( LPARAM ) & es );
   CloseHandle( hFile );

   if( es.dwError )
   {
      hb_retl( FALSE );
   }
   else
   {
      hb_retl( TRUE );
   }
}

/*
 * HB_FUNC( RICHEDITBOX_RTFLOADRESOURCEFILE )
 *
 * Loads an RTF resource file into a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. LPCSTR cFileName: Name of the resource file.
 *   3. BOOL lSelect: Flag to indicate if the selection should be replaced.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function loads an RTF resource file into a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_RTFLOADRESOURCEFILE )
{
   HWND     hWndControl = hmg_par_raw_HWND( 1 );
#ifndef UNICODE
   LPCSTR   cFileName = ( char * ) hb_parc( 2 );
#else
   LPCWSTR  cFileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   BOOL     lSelect = ( BOOL ) hb_parl( 3 );
   HRSRC    hResourceData;
   HGLOBAL  hGlobalResource;
   TCHAR    *lpGlobalResource = NULL;

   hResourceData = FindResource( NULL, cFileName, TEXT( "RTF" ) );
   if( hResourceData != NULL )
   {
      hGlobalResource = LoadResource( NULL, hResourceData );
      if( hGlobalResource != NULL )
      {
         lpGlobalResource = LockResource( hGlobalResource );
         if( lpGlobalResource != NULL )
         {
            SETTEXTEX   ST;
            ST.flags = ( lSelect ? ST_SELECTION : ST_DEFAULT );
#ifdef UNICODE
            ST.codepage = CP_UNICODE;
#else
            ST.codepage = CP_ACP;
#endif
            SendMessage( hWndControl, EM_SETTEXTEX, ( WPARAM ) & ST, ( LPARAM ) lpGlobalResource );
         }

         FreeResource( hGlobalResource );
      }
   }

   hmg_ret_L( !( lpGlobalResource == NULL ) );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) cFileName );
#endif
}

/*
 * HB_FUNC( RICHEDITBOX_SETRTFTEXTMODE )
 *
 * Sets the text mode of a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. BOOL lRTF: Flag to indicate if the text mode should be RTF.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function sets the text mode of a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SETRTFTEXTMODE )
{
   HWND  hWndControl = hmg_par_raw_HWND( 1 );
   BOOL  lRTF = ( HB_ISLOG( 2 ) ? ( BOOL ) hb_parl( 2 ) : TRUE );
   LONG  Mode = ( lRTF ? TM_RICHTEXT : TM_PLAINTEXT ) | TM_MULTILEVELUNDO | TM_MULTICODEPAGE;
   SendMessage( hWndControl, EM_SETTEXTMODE, ( WPARAM ) Mode, 0 );
}

/*
 * HB_FUNC( RICHEDITBOX_ISRTFTEXTMODE )
 *
 * Checks if the text mode of a Rich Edit control is RTF.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   BOOL: Returns TRUE if the text mode is RTF, otherwise FALSE.
 *
 * Purpose:
 *   This function checks if the text mode of a Rich Edit control is RTF.
 */
HB_FUNC( RICHEDITBOX_ISRTFTEXTMODE )
{
   LRESULT  lResult = SendMessage( hmg_par_raw_HWND( 1 ), EM_GETTEXTMODE, 0, 0 );
   hb_retl( ( BOOL ) ( lResult & TM_RICHTEXT ) );
}

/*
 * HB_FUNC( RICHEDITBOX_SETAUTOURLDETECT )
 *
 * Sets the auto URL detection of a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. BOOL lLink: Flag to indicate if auto URL detection should be enabled.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function sets the auto URL detection of a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SETAUTOURLDETECT )
{
   BOOL  lLink = ( HB_ISLOG( 2 ) ? ( BOOL ) hb_parl( 2 ) : TRUE );
   SendMessage( hmg_par_raw_HWND( 1 ), EM_AUTOURLDETECT, ( WPARAM ) lLink, 0 );
}

/*
 * HB_FUNC( RICHEDITBOX_GETAUTOURLDETECT )
 *
 * Gets the auto URL detection setting of a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   BOOL: Returns TRUE if auto URL detection is enabled, otherwise FALSE.
 *
 * Purpose:
 *   This function gets the auto URL detection setting of a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_GETAUTOURLDETECT )
{
   hb_retl( ( BOOL ) SendMessage( hmg_par_raw_HWND( 1 ), EM_GETAUTOURLDETECT, 0, 0 ) );
}

/*
 * HB_FUNC( RICHEDITBOX_SETBKGNDCOLOR )
 *
 * Sets the background color of a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. Array aBkgndColor: Array containing the RGB values for the background color.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function sets the background color of a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SETBKGNDCOLOR )
{
   HWND  hWndControl = hmg_par_raw_HWND( 1 );
   if( HB_ISARRAY( 2 ) )
   {
      SendMessage( hWndControl, EM_SETBKGNDCOLOR, 0, ( LPARAM ) RGB( HB_PARNI( 2, 1 ), HB_PARNI( 2, 2 ), HB_PARNI( 2, 3 ) ) );
   }
   else
   {
      SendMessage( hWndControl, EM_SETBKGNDCOLOR, ( WPARAM ) 1, 0 ); // Set to the window background system color
   }
}

/*
 * HB_FUNC( RICHEDITBOX_SETZOOM )
 *
 * Sets the zoom level of a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. int nNumerator: Numerator for the zoom ratio.
 *   3. int nDenominator: Denominator for the zoom ratio.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function sets the zoom level of a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SETZOOM )   // ZoomRatio = nNumerator / nDenominator
{
   SendMessage( hmg_par_raw_HWND( 1 ), EM_SETZOOM, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) hb_parni( 3 ) ); // 1/64 < ZoomRatio < 64
}

/*
 * HB_FUNC( RICHEDITBOX_GETZOOM )
 *
 * Gets the zoom level of a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. int @nNumerator: Reference to store the numerator for the zoom ratio.
 *   3. int @nDenominator: Reference to store the denominator for the zoom ratio.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function gets the zoom level of a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_GETZOOM )
{
   int   nNumerator, nDenominator;
   SendMessage( hmg_par_raw_HWND( 1 ), EM_GETZOOM, ( WPARAM ) & nNumerator, ( LPARAM ) & nDenominator );
   if( HB_ISBYREF( 2 ) )
   {
      hb_storni( nNumerator, 2 );
   }

   if( HB_ISBYREF( 3 ) )
   {
      hb_storni( nDenominator, 3 );
   }
}

/*
 * HB_FUNC( RICHEDITBOX_SETFONT )
 *
 * Sets the font of a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. LPCSTR cFontName: Name of the font.
 *   3. int nFontSize: Size of the font.
 *   4. BOOL lBold: Flag to indicate if the font should be bold.
 *   5. BOOL lItalic: Flag to indicate if the font should be italic.
 *   6. BOOL lUnderline: Flag to indicate if the font should be underlined.
 *   7. BOOL lStrikeout: Flag to indicate if the font should be strikeout.
 *   8. Array aTextColor: Array containing the RGB values for the text color.
 *   9. Array aBackColor: Array containing the RGB values for the background color.
 *   10. int nScript: Script type.
 *   11. BOOL lLink: Flag to indicate if the font should be a link.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function sets the font of a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SETFONT )
{
   HWND        hWndControl = hmg_par_raw_HWND( 1 );
   CHARFORMAT2 CharFormat2;
   DWORD       Mask = 0;
   DWORD       Effects = 0;

   ZeroMemory( &CharFormat2, sizeof( CHARFORMAT2 ) );
   CharFormat2.cbSize = sizeof( CHARFORMAT2 );

   if( HB_ISCHAR( 2 ) )
   {
#ifndef UNICODE
      TCHAR *szFaceName = ( TCHAR * ) hb_parc( 2 );
#else
      TCHAR *szFaceName = ( TCHAR * ) hb_osStrU16Encode( ( char * ) hb_parc( 2 ) );
#endif
      Mask = Mask | CFM_FACE;
      lstrcpy( CharFormat2.szFaceName, szFaceName );
   }

   if( HB_ISNUM( 3 ) && hb_parnl( 3 ) )
   {
      Mask = Mask | CFM_SIZE;
      CharFormat2.yHeight = hb_parnl( 3 ) * 20 / 1;               // yHeight (character height) is in twips (1/1440 of an inch or 1/20 of a printer point)
   }

   if( HB_ISLOG( 4 ) )
   {
      Mask = Mask | CFM_BOLD;
      if( hb_parl( 4 ) )
      {
         Effects = Effects | CFE_BOLD;
      }
   }

   if( HB_ISLOG( 5 ) )
   {
      Mask = Mask | CFM_ITALIC;
      if( hb_parl( 5 ) )
      {
         Effects = Effects | CFE_ITALIC;
      }
   }

   if( HB_ISLOG( 6 ) )
   {
      Mask = Mask | CFM_UNDERLINE;
      if( hb_parl( 6 ) )
      {
         Effects = Effects | CFE_UNDERLINE;
      }
   }

   if( HB_ISLOG( 7 ) )
   {
      Mask = Mask | CFM_STRIKEOUT;
      if( hb_parl( 7 ) )
      {
         Effects = Effects | CFE_STRIKEOUT;
      }
   }

   if( HB_ISARRAY( 8 ) )
   {
      Mask = Mask | CFM_COLOR;
      CharFormat2.crTextColor = RGB( HB_PARNI( 8, 1 ), HB_PARNI( 8, 2 ), HB_PARNI( 8, 3 ) );
   }
   else if( HB_ISNUM( 8 ) && hb_parnl( 8 ) == -1 )
   {
      Mask = Mask | CFM_COLOR;
      Effects = Effects | CFE_AUTOCOLOR;                          // equivalent to GetSysColor(COLOR_WINDOWTEXT)
   }

   if( HB_ISARRAY( 9 ) )
   {
      Mask = Mask | CFM_BACKCOLOR;
      CharFormat2.crBackColor = RGB( HB_PARNI( 9, 1 ), HB_PARNI( 9, 2 ), HB_PARNI( 9, 3 ) );
   }
   else if( HB_ISNUM( 9 ) && hb_parnl( 9 ) == -1 )
   {
      Mask = Mask | CFM_BACKCOLOR;
      Effects = Effects | CFE_AUTOBACKCOLOR;                      // equivalent to GetSysColor(COLOR_WINDOW)
   }

   if( HB_ISNUM( 10 ) )
   {
      Mask = Mask | CFM_SUBSCRIPT | CFM_SUPERSCRIPT;              // The CFE_SUPERSCRIPT and CFE_SUBSCRIPT values are mutually exclusive
      if( hb_parnl( 10 ) == 1 )
      {
         Effects = Effects | CFE_SUBSCRIPT;
      }

      if( hb_parnl( 10 ) == 2 )
      {
         Effects = Effects | CFE_SUPERSCRIPT;
      }
   }

   if( HB_ISLOG( 11 ) )
   {
      Mask = Mask | CFM_LINK;
      if( hb_parl( 11 ) )
      {
         Effects = Effects | CFE_LINK;
      }
   }

   CharFormat2.dwMask = Mask;
   CharFormat2.dwEffects = Effects;
   hmg_ret_L( SendMessage( hWndControl, EM_SETCHARFORMAT, SCF_SELECTION, ( LPARAM ) & CharFormat2 ) );
}

/*
 * HB_FUNC( RICHEDITBOX_GETFONT )
 *
 * Gets the font properties of a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. LPCSTR @cFontName: Reference to store the font name.
 *   3. int @nFontSize: Reference to store the font size.
 *   4. BOOL @lBold: Reference to store the bold flag.
 *   5. BOOL @lItalic: Reference to store the italic flag.
 *   6. BOOL @lUnderline: Reference to store the underline flag.
 *   7. BOOL @lStrikeout: Reference to store the strikeout flag.
 *   8. Array @aTextColor: Reference to store the RGB values for the text color.
 *   9. Array @aBackColor: Reference to store the RGB values for the background color.
 *   10. int @nScript: Reference to store the script type.
 *   11. BOOL @lLink: Reference to store the link flag.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function gets the font properties of a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_GETFONT )
{
   HWND        hWndControl = hmg_par_raw_HWND( 1 );
   CHARFORMAT2 CharFormat2;
   DWORD       Mask;
   DWORD       Effects;
#ifdef UNICODE
   LPSTR       pStr;
#endif
   ZeroMemory( &CharFormat2, sizeof( CHARFORMAT2 ) );
   CharFormat2.cbSize = sizeof( CHARFORMAT2 );
   Mask = CFM_FACE | CFM_SIZE | CFM_BOLD | CFM_ITALIC | CFM_UNDERLINE | CFM_STRIKEOUT | CFM_COLOR | CFM_BACKCOLOR | CFM_SUBSCRIPT | CFM_SUPERSCRIPT | CFM_LINK;
   CharFormat2.dwMask = Mask;
   SendMessage( hWndControl, EM_GETCHARFORMAT, SCF_SELECTION, ( LPARAM ) & CharFormat2 );
   Effects = CharFormat2.dwEffects;

   if( HB_ISBYREF( 2 ) )
   {
#ifndef UNICODE
      hb_storc( CharFormat2.szFaceName, 2 );
#else
      pStr = WideToAnsi( CharFormat2.szFaceName );
      hb_storc( pStr, 2 );
      hb_xfree( pStr );
#endif
   }

   if( HB_ISBYREF( 3 ) )
   {
      hb_stornl( ( LONG ) ( CharFormat2.yHeight * 1 / 20 ), 3 );  // yHeight (character height) is in twips (1/1440 of an inch or 1/20 of a printer point)
   }

   if( HB_ISBYREF( 4 ) )
   {
      hb_storl( ( BOOL ) ( Effects & CFE_BOLD ), 4 );
   }

   if( HB_ISBYREF( 5 ) )
   {
      hb_storl( ( BOOL ) ( Effects & CFE_ITALIC ), 5 );
   }

   if( HB_ISBYREF( 6 ) )
   {
      hb_storl( ( BOOL ) ( Effects & CFE_UNDERLINE ), 6 );
   }

   if( HB_ISBYREF( 7 ) )
   {
      hb_storl( ( BOOL ) ( Effects & CFE_STRIKEOUT ), 7 );
   }

   if( HB_ISBYREF( 8 ) )
   {
      PHB_ITEM pArray = hb_param( 8, HB_IT_ANY );
      hb_arrayNew( pArray, 3 );
      hb_arraySetNL( pArray, 1, GetRValue( CharFormat2.crTextColor ) );
      hb_arraySetNL( pArray, 2, GetGValue( CharFormat2.crTextColor ) );
      hb_arraySetNL( pArray, 3, GetBValue( CharFormat2.crTextColor ) );
   }

   if( HB_ISBYREF( 9 ) )
   {
      PHB_ITEM pArray = hb_param( 9, HB_IT_ANY );
      hb_arrayNew( pArray, 3 );
      hb_arraySetNL( pArray, 1, GetRValue( CharFormat2.crBackColor ) );
      hb_arraySetNL( pArray, 2, GetGValue( CharFormat2.crBackColor ) );
      hb_arraySetNL( pArray, 3, GetBValue( CharFormat2.crBackColor ) );
   }

   if( HB_ISBYREF( 10 ) )
   {
      if( Effects & CFE_SUPERSCRIPT )
      {
         hb_stornl( ( LONG ) 2, 10 );
      }
      else if( Effects & CFE_SUBSCRIPT )
      {
         hb_stornl( ( LONG ) 1, 10 );
      }
      else
      {
         hb_stornl( ( LONG ) 0, 10 );
      }
   }

   if( HB_ISBYREF( 11 ) )
   {
      hb_storl( ( BOOL ) ( Effects & CFE_LINK ), 11 );
   }
}

/*
 * HB_FUNC( RICHEDITBOX_SETSELRANGE )
 *
 * Sets the selection range in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. Array { nMin, nMax }: Array containing the minimum and maximum positions of the selection.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function sets the selection range in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SETSELRANGE )
{
   CHARRANGE   CharRange;
   CharRange.cpMin = hmg_parv_LONG( 2, 1 );
   CharRange.cpMax = hmg_parv_LONG( 2, 2 );
   SendMessage( hmg_par_raw_HWND( 1 ), EM_EXSETSEL, 0, ( LPARAM ) & CharRange );
}

/*
 * HB_FUNC( RICHEDITBOX_GETSELRANGE )
 *
 * Gets the selection range in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   Array: Returns an array containing the minimum and maximum positions of the selection.
 *
 * Purpose:
 *   This function gets the selection range in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_GETSELRANGE )
{
   CHARRANGE   CharRange;
   SendMessage( hmg_par_raw_HWND( 1 ), EM_EXGETSEL, 0, ( LPARAM ) & CharRange );
   hb_reta( 2 );
   HB_STORVNL( ( LONG ) CharRange.cpMin, -1, 1 );
   HB_STORVNL( ( LONG ) CharRange.cpMax, -1, 2 );
}

/*
 * HB_FUNC( RICHEDITBOX_REPLACESEL )
 *
 * Replaces the selected text in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. LPCSTR cText: Text to replace the selection with.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function replaces the selected text in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_REPLACESEL )
{
#ifndef UNICODE
   LPCTSTR  cBuffer = ( LPCTSTR ) hb_parc( 2 );
#else
   LPCTSTR  cBuffer = ( LPCTSTR ) AnsiToWide( hb_parc( 2 ) );
#endif
   SendMessage( hmg_par_raw_HWND( 1 ), EM_REPLACESEL, ( WPARAM ) TRUE, ( LPARAM ) cBuffer );
}

/*
 * HB_FUNC( RICHEDITBOX_SETTEXT )
 *
 * Sets the text in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. BOOL lSelect: Flag to indicate if the selection should be replaced.
 *   3. LPCSTR cText: Text to set in the control.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function sets the text in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SETTEXT )
{
   HWND        hWndControl = hmg_par_raw_HWND( 1 );
   BOOL        lSelect = ( BOOL ) hb_parl( 2 );
#ifndef UNICODE
   LPCTSTR     cBuffer = ( LPCTSTR ) hb_parc( 3 );
#else
   LPCTSTR     cBuffer = ( LPCTSTR ) AnsiToWide( hb_parc( 3 ) );
#endif
   SETTEXTEX   ST;
   ST.flags = ( lSelect ? ST_SELECTION : ST_DEFAULT );
#ifdef UNICODE
   ST.codepage = CP_UNICODE;
#else
   ST.codepage = CP_ACP;
#endif
   SendMessage( hWndControl, EM_SETTEXTEX, ( WPARAM ) & ST, ( LPARAM ) cBuffer );
}

/*
 * HB_FUNC( RICHEDITBOX_GETTEXT )
 *
 * Gets the text from a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. BOOL lSelect: Flag to indicate if the selection should be retrieved.
 *
 * Return Value:
 *   LPCSTR: Returns the text from the Rich Edit control.
 *
 * Purpose:
 *   This function gets the text from a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_GETTEXT )
{
#ifdef UNICODE
   LPSTR             pStr;
#endif
   HWND              hWndControl = hmg_par_raw_HWND( 1 );
   BOOL              lSelect = ( BOOL ) hb_parl( 2 );
   LRESULT           nLen;
   GETTEXTLENGTHEX   GTL;
   GTL.flags = GTL_PRECISE;
#ifdef UNICODE
   GTL.codepage = CP_UNICODE;
#else
   GTL.codepage = CP_ACP;
#endif
   nLen = SendMessage( hWndControl, EM_GETTEXTLENGTHEX, ( WPARAM ) & GTL, 0 );
   if( nLen > 0 )
   {
      TCHAR       *cBuffer = ( TCHAR * ) hb_xgrab( ( nLen + 1 ) * sizeof( TCHAR ) );
      GETTEXTEX   GT;
      GT.cb = ( DWORD ) ( ( nLen + 1 ) * sizeof( TCHAR ) );
      GT.flags = ( lSelect ? GT_SELECTION : GT_DEFAULT );
#ifdef UNICODE
      GT.codepage = CP_UNICODE;
#else
      GT.codepage = CP_ACP;
#endif
      GT.lpDefaultChar = NULL;
      GT.lpUsedDefChar = NULL;
      SendMessage( hWndControl, EM_GETTEXTEX, ( WPARAM ) & GT, ( LPARAM ) cBuffer );
#ifndef UNICODE
      hb_retc( cBuffer );
#else
      pStr = WideToAnsi( cBuffer );
      hb_retc( pStr );
      hb_xfree( pStr );
#endif
   }
}

/*
 * HB_FUNC( RICHEDITBOX_GETTEXTLENGTH )
 *
 * Gets the length of the text in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   LONG: Returns the length of the text in the Rich Edit control.
 *
 * Purpose:
 *   This function gets the length of the text in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_GETTEXTLENGTH )
{
   LONG              nLength;
   GETTEXTLENGTHEX   GTL;
   GTL.flags = GTL_NUMCHARS;
#ifdef UNICODE
   GTL.codepage = CP_UNICODE;
#else
   GTL.codepage = CP_ACP;
#endif
   nLength = ( LONG ) SendMessage( hmg_par_raw_HWND( 1 ), EM_GETTEXTLENGTHEX, ( WPARAM ) & GTL, 0 );
   hmg_ret_LONG( nLength );
}

/*
 * HB_FUNC( RICHEDITBOX_GETTEXTRANGE )
 *
 * Gets a range of text from a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. Array { nMin, nMax }: Array containing the minimum and maximum positions of the text range.
 *
 * Return Value:
 *   LPCSTR: Returns the text from the specified range in the Rich Edit control.
 *
 * Purpose:
 *   This function gets a range of text from a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_GETTEXTRANGE )
{
#ifdef UNICODE
   LPSTR             pStr;
#endif
   HWND              hWndControl = hmg_par_raw_HWND( 1 );
   LRESULT           nLength;
   GETTEXTLENGTHEX   GTL;
   GTL.flags = GTL_PRECISE;
#ifdef UNICODE
   GTL.codepage = CP_UNICODE;
#else
   GTL.codepage = CP_ACP;
#endif
   nLength = SendMessage( hWndControl, EM_GETTEXTLENGTHEX, ( WPARAM ) & GTL, 0 );
   if( nLength > 0 )
   {
      TCHAR       *cBuffer = ( TCHAR * ) hb_xgrab( ( nLength + 1 ) * sizeof( TCHAR ) );
      TEXTRANGE   TextRange;
      TextRange.lpstrText = cBuffer;
      TextRange.chrg.cpMin = hmg_parv_LONG( 2, 1 );
      TextRange.chrg.cpMax = hmg_parv_LONG( 2, 2 );
      SendMessage( hWndControl, EM_GETTEXTRANGE, 0, ( LPARAM ) & TextRange );
#ifndef UNICODE
      hb_retc( TextRange.lpstrText );
#else
      pStr = WideToAnsi( TextRange.lpstrText );
      hb_retc( pStr );
      hb_xfree( pStr );
#endif
   }
}

/*
 * HB_FUNC( RICHEDITBOX_FINDTEXT )
 *
 * Finds text in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. LPCSTR cFind: Text to find.
 *   3. BOOL lDown: Flag to indicate if the search should be downwards.
 *   4. BOOL lMatchCase: Flag to indicate if the search should be case-sensitive.
 *   5. BOOL lWholeWord: Flag to indicate if the search should match whole words.
 *   6. BOOL lSelectFindText: Flag to indicate if the found text should be selected.
 *
 * Return Value:
 *   Array: Returns an array containing the minimum and maximum positions of the found text.
 *
 * Purpose:
 *   This function finds text in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_FINDTEXT )
{
   HWND        hWndControl = hmg_par_raw_HWND( 1 );
#ifndef UNICODE
   LPSTR       cFind = ( LPSTR ) hb_parc( 2 );
#else
   LPWSTR      cFind = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   BOOL        Down = ( BOOL ) ( HB_ISNIL( 3 ) ? TRUE : hb_parl( 3 ) );
   BOOL        MatchCase = ( BOOL ) ( HB_ISNIL( 4 ) ? FALSE : hb_parl( 4 ) );
   BOOL        WholeWord = ( BOOL ) ( HB_ISNIL( 5 ) ? FALSE : hb_parl( 5 ) );
   BOOL        SelectFindText = ( BOOL ) ( HB_ISNIL( 6 ) ? TRUE : hb_parl( 6 ) );
   CHARRANGE   CharRange;
   int         Options = 0;
#ifdef UNICODE
   FINDTEXTEXW FindText;
#else
   FINDTEXTEX  FindText;
#endif
   if( Down )
   {
      Options = Options | FR_DOWN;
   }

   if( MatchCase )
   {
      Options = Options | FR_MATCHCASE;
   }

   if( WholeWord )
   {
      Options = Options | FR_WHOLEWORD;
   }

   SendMessage( hWndControl, EM_EXGETSEL, 0, ( LPARAM ) & CharRange );
   if( Down )
   {
      CharRange.cpMin = CharRange.cpMax;
      CharRange.cpMax = -1;
   }
   else
   {
      CharRange.cpMin = CharRange.cpMin;
      CharRange.cpMax = 0;
   }

   FindText.chrg = CharRange;
   FindText.lpstrText = cFind;
#ifdef UNICODE
   SendMessage( hWndControl, EM_FINDTEXTEXW, ( WPARAM ) Options, ( LPARAM ) & FindText );
#else
   SendMessage( hWndControl, EM_FINDTEXTEX, ( WPARAM ) Options, ( LPARAM ) & FindText );
#endif
   if( SelectFindText == FALSE )
   {
      FindText.chrgText.cpMin = FindText.chrgText.cpMax;
   }

   SendMessage( hWndControl, EM_EXSETSEL, 0, ( LPARAM ) & FindText.chrgText );
   hb_reta( 2 );
   HB_STORVNL( ( LONG ) FindText.chrgText.cpMin, -1, 1 );
   HB_STORVNL( ( LONG ) FindText.chrgText.cpMax, -1, 2 );
#ifdef UNICODE
   hb_xfree( cFind );
#endif
}

/*
 * HB_FUNC( RICHEDITBOX_SETPARAFORMAT )
 *
 * Sets the paragraph format in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. WORD nAlignment: Alignment of the paragraph.
 *   3. WORD nNumbering: Numbering style of the paragraph.
 *   4. WORD nNumberingStyle: Numbering style of the paragraph.
 *   5. WORD nNumberingStart: Starting number for the numbering.
 *   6. double ndOffset: Offset of the paragraph.
 *   7. double ndLineSpacing: Line spacing of the paragraph.
 *   8. double ndStartIndent: Start indent of the paragraph.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function sets the paragraph format in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SETPARAFORMAT )
{
   HWND        hWndControl = hmg_par_raw_HWND( 1 );
   WORD        Alignment = ( WORD ) ( HB_ISNIL( 2 ) ? 0 : hb_parni( 2 ) );
   WORD        Numbering = ( WORD ) ( HB_ISNIL( 3 ) ? 0 : hb_parni( 3 ) );
   WORD        NumberingStyle = ( WORD ) ( HB_ISNIL( 4 ) ? 0 : hb_parni( 4 ) );
   WORD        NumberingStart = ( WORD ) ( HB_ISNIL( 5 ) ? 0 : hb_parni( 5 ) );
   double      Offset = HB_ISNIL( 6 ) ? 0.0 : ( double ) hb_parnd( 6 );
   double      LineSpacing = HB_ISNIL( 7 ) ? 0.0 : ( double ) hb_parnd( 7 );
   double      StartIndent = HB_ISNIL( 8 ) ? 0.0 : ( double ) hb_parnd( 8 );
   PARAFORMAT2 ParaFormat2;
   DWORD       Mask = 0;

   ZeroMemory( &ParaFormat2, sizeof( PARAFORMAT2 ) );
   ParaFormat2.cbSize = sizeof( PARAFORMAT2 );

   if( Alignment > 0 )
   {
      Mask = Mask | PFM_ALIGNMENT;
      switch( Alignment )
      {
         case 1:
            ParaFormat2.wAlignment = PFA_LEFT;
            break;

         case 2:
            ParaFormat2.wAlignment = PFA_RIGHT;
            break;

         case 3:
            ParaFormat2.wAlignment = PFA_CENTER;
            break;

         case 4:
            ParaFormat2.wAlignment = PFA_JUSTIFY;
            break;

         default:
            ParaFormat2.wAlignment = PFA_LEFT;
            break;
      }
   }

   if( Numbering > 0 )
   {
      Mask = Mask | PFM_NUMBERING;
      switch( Numbering )
      {
         case 1:
            ParaFormat2.wNumbering = 0;
            break;   // No paragraph numbering or bullets

         case 2:
            ParaFormat2.wNumbering = PFN_BULLET;
            break;   // Insert a bullet at the beginning of each selected paragraph

         case 3:
            ParaFormat2.wNumbering = PFN_ARABIC;
            break;   // Use Arabic numbers (0, 1, 2, ...)

         case 4:
            ParaFormat2.wNumbering = PFN_LCLETTER;
            break;   // Use lowercase letters (a, b, c, ...)

         case 5:
            ParaFormat2.wNumbering = PFN_LCROMAN;
            break;   // Use lowercase Roman letters (i, ii, iii, ...)

         case 6:
            ParaFormat2.wNumbering = PFN_UCLETTER;
            break;   // Use uppercase letters (A, B, C, ...)

         case 7:
            ParaFormat2.wNumbering = PFN_UCROMAN;
            break;   // Use uppercase Roman letters (I, II, III, ...)

         case 8:
            ParaFormat2.wNumbering = 7;
            break;   // Uses a sequence of characters beginning with the Unicode character specified by the nNumberingStart

         default:
            ParaFormat2.wNumbering = 0;
            break;
      }
   }

   if( NumberingStyle > 0 )
   {
      Mask = Mask | PFM_NUMBERINGSTYLE;
      switch( NumberingStyle )
      {
         case 1:
            ParaFormat2.wNumberingStyle = PFNS_PAREN;
            break;   // Follows the number with a right parenthesis.

         case 2:
            ParaFormat2.wNumberingStyle = PFNS_PARENS;
            break;   // Encloses the number in parentheses

         case 3:
            ParaFormat2.wNumberingStyle = PFNS_PERIOD;
            break;   // Follows the number with a period

         case 4:
            ParaFormat2.wNumberingStyle = PFNS_PLAIN;
            break;   // Displays only the number

         case 5:
            ParaFormat2.wNumberingStyle = PFNS_NONUMBER;
            break;   // Continues a numbered list without applying the next number or bullet

         case 6:
            ParaFormat2.wNumberingStyle = PFNS_NEWNUMBER;
            break;   // Starts a new number with nNumberingStart

         default:
            ParaFormat2.wNumberingStyle = 0;
            break;
      }
   }

   if( HB_ISNUM( 5 ) )
   {
      Mask = Mask | PFM_NUMBERINGSTART;
      ParaFormat2.wNumberingStart = NumberingStart;
   }

   if( HB_ISNUM( 6 ) )
   {
      Mask = Mask | PFM_OFFSET;
      ParaFormat2.dxOffset = ( LONG ) ( ( double ) ( Offset * 1440.0 / 25.4 ) ); // Offset is in millimeters (1 inch = 25.4 mm = 1440 twips)
   }

   if( LineSpacing > 0.0 )
   {
      Mask = Mask | PFM_LINESPACING;
      ParaFormat2.bLineSpacingRule = 5;   // Spacing from one line to the next, 20 twips = single-spaced text
      ParaFormat2.dyLineSpacing = ( LONG ) ( ( double ) ( LineSpacing * 20.0 / 1.0 ) );
   }

   if( HB_ISNUM( 8 ) )
   {
      Mask = Mask | PFM_STARTINDENT;
      ParaFormat2.dxStartIndent = ( LONG ) ( ( double ) ( StartIndent * 1440.0 / 25.4 ) );   // StartIndent is in millimeters (1 inch = 25.4 mm = 1440 twips)
   }

   ParaFormat2.dwMask = Mask;
   SendMessage( hWndControl, EM_SETPARAFORMAT, 0, ( LPARAM ) & ParaFormat2 );
}

/*
 * HB_FUNC( RICHEDITBOX_GETPARAFORMAT )
 *
 * Gets the paragraph format in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. WORD @nAlignment: Reference to store the alignment of the paragraph.
 *   3. WORD @nNumbering: Reference to store the numbering style of the paragraph.
 *   4. WORD @nNumberingStyle: Reference to store the numbering style of the paragraph.
 *   5. WORD @nNumberingStart: Reference to store the starting number for the numbering.
 *   6. double @ndOffset: Reference to store the offset of the paragraph.
 *   7. double @ndLineSpacing: Reference to store the line spacing of the paragraph.
 *   8. double @ndStartIndent: Reference to store the start indent of the paragraph.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function gets the paragraph format in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_GETPARAFORMAT )
{
   HWND        hWndControl = hmg_par_raw_HWND( 1 );
   WORD        Alignment = 0;
   WORD        Numbering = 0;
   WORD        NumberingStyle;
   WORD        NumberingStart;
   double      Offset;
   double      LineSpacing = 0.0;
   double      StartIndent;
   PARAFORMAT2 ParaFormat2;

   ZeroMemory( &ParaFormat2, sizeof( PARAFORMAT2 ) );
   ParaFormat2.cbSize = sizeof( PARAFORMAT2 );
   ParaFormat2.dwMask = PFM_ALIGNMENT | PFM_NUMBERING | PFM_NUMBERINGSTYLE | PFM_NUMBERINGSTART | PFM_LINESPACING | PFM_STARTINDENT | PFM_OFFSET;
   SendMessage( hWndControl, EM_GETPARAFORMAT, 0, ( LPARAM ) & ParaFormat2 );

   if( HB_ISBYREF( 2 ) )
   {
      if( ParaFormat2.wAlignment == PFA_LEFT )
      {
         Alignment = 1;
      }
      else if( ParaFormat2.wAlignment == PFA_RIGHT )
      {
         Alignment = 2;
      }
      else if( ParaFormat2.wAlignment == PFA_CENTER )
      {
         Alignment = 3;
      }
      else if( ParaFormat2.wAlignment == PFA_JUSTIFY )
      {
         Alignment = 4;
      }

      hb_stornl( ( LONG ) Alignment, 2 );
   }

   if( HB_ISBYREF( 3 ) )
   {
      if( ParaFormat2.wNumbering == 0 )
      {
         Numbering = 1;
      }
      else if( ParaFormat2.wNumbering == PFN_BULLET )
      {
         Numbering = 2;
      }
      else if( ParaFormat2.wNumbering == PFN_ARABIC )
      {
         Numbering = 3;
      }
      else if( ParaFormat2.wNumbering == PFN_LCLETTER )
      {
         Numbering = 4;
      }
      else if( ParaFormat2.wNumbering == PFN_LCROMAN )
      {
         Numbering = 5;
      }
      else if( ParaFormat2.wNumbering == PFN_UCLETTER )
      {
         Numbering = 6;
      }
      else if( ParaFormat2.wNumbering == PFN_UCROMAN )
      {
         Numbering = 7;
      }
      else if( ParaFormat2.wNumbering == 7 )
      {
         Numbering = 8;
      }

      hb_stornl( ( LONG ) Numbering, 3 );
   }

   if( HB_ISBYREF( 4 ) )
   {
      if( ParaFormat2.wNumberingStyle == PFNS_PAREN )
      {
         NumberingStyle = 1;
      }
      else if( ParaFormat2.wNumberingStyle == PFNS_PARENS )
      {
         NumberingStyle = 2;
      }
      else if( ParaFormat2.wNumberingStyle == PFNS_PERIOD )
      {
         NumberingStyle = 3;
      }
      else if( ParaFormat2.wNumberingStyle == PFNS_PLAIN )
      {
         NumberingStyle = 4;
      }
      else if( ParaFormat2.wNumberingStyle == PFNS_NONUMBER )
      {
         NumberingStyle = 5;
      }
      else if( ParaFormat2.wNumberingStyle == PFNS_NEWNUMBER )
      {
         NumberingStyle = 6;
      }
      else
      {
         NumberingStyle = 0;
      }

      hb_stornl( ( LONG ) NumberingStyle, 4 );
   }

   if( HB_ISBYREF( 5 ) )
   {
      NumberingStart = ParaFormat2.wNumberingStart;
      hb_stornl( ( LONG ) NumberingStart, 5 );
   }

   if( HB_ISBYREF( 6 ) )
   {
      Offset = ( double ) ParaFormat2.dxOffset;
      hb_stornd( ( double ) ( Offset * 25.4 / 1440.0 ), 6 );
   }

   if( HB_ISBYREF( 7 ) )
   {
      if( ParaFormat2.bLineSpacingRule == 0 )
      {
         LineSpacing = 1.0;
      }
      else if( ParaFormat2.bLineSpacingRule == 1 )
      {
         LineSpacing = 1.5;
      }
      else if( ParaFormat2.bLineSpacingRule == 2 )
      {
         LineSpacing = 2.0;
      }
      else if( ParaFormat2.bLineSpacingRule == 3 )
      {
         LineSpacing = ( ( double ) ParaFormat2.dyLineSpacing ) * -1.0; // if < 0 is in twips
      }
      else if( ParaFormat2.bLineSpacingRule == 4 )
      {
         LineSpacing = ( ( double ) ParaFormat2.dyLineSpacing ) * -1.0; // if < 0 is in twips
      }
      else if( ParaFormat2.bLineSpacingRule == 5 )
      {
         LineSpacing = ( ( double ) ParaFormat2.dyLineSpacing ) * 1.0 / 20.0;
      }

      hb_stornd( ( double ) LineSpacing, 7 );
   }

   if( HB_ISBYREF( 8 ) )
   {
      StartIndent = ( double ) ParaFormat2.dxStartIndent;
      hb_stornd( ( double ) ( StartIndent * 25.4 / 1440.0 ), 8 );
   }
}

/*
 * HB_FUNC( RICHEDITBOX_SELCOPY )
 *
 * Copies the selected text in a Rich Edit control to the clipboard.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function copies the selected text in a Rich Edit control to the clipboard.
 */
HB_FUNC( RICHEDITBOX_SELCOPY )
{
   SendMessage( hmg_par_raw_HWND( 1 ), WM_COPY, 0, 0 );                 // copy the current selection to the clipboard in CF_TEXT format
}

/*
 * HB_FUNC( RICHEDITBOX_SELPASTE )
 *
 * Pastes text from the clipboard into a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function pastes text from the clipboard into a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SELPASTE )
{
   SendMessage( hmg_par_raw_HWND( 1 ), WM_PASTE, 0, 0 );                // copy the current content of the clipboard at the current caret position, data is inserted only if the clipboard contains data in CF_TEXT format
}

/*
 * HB_FUNC( RICHEDITBOX_SELCUT )
 *
 * Cuts the selected text in a Rich Edit control to the clipboard.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function cuts the selected text in a Rich Edit control to the clipboard.
 */
HB_FUNC( RICHEDITBOX_SELCUT )
{
   SendMessage( hmg_par_raw_HWND( 1 ), WM_CUT, 0, 0 );                  // delete (cut) the current selection and place the deleted content on the clipboard
}

/*
 * HB_FUNC( RICHEDITBOX_SELCLEAR )
 *
 * Clears the selected text in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function clears the selected text in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SELCLEAR )
{
   SendMessage( hmg_par_raw_HWND( 1 ), WM_CLEAR, 0, 0 );                // delete (cut) the current selection
}

/*
 * HB_FUNC( RICHEDITBOX_CHANGEUNDO )
 *
 * Undoes the last action in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function undoes the last action in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_CHANGEUNDO )
{
   SendMessage( hmg_par_raw_HWND( 1 ), EM_UNDO, 0, 0 );
}

/*
 * HB_FUNC( RICHEDITBOX_CHANGEREDO )
 *
 * Redoes the last undone action in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function redoes the last undone action in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_CHANGEREDO )
{
   SendMessage( hmg_par_raw_HWND( 1 ), EM_REDO, 0, 0 );
}

/*
 * HB_FUNC( RICHEDITBOX_CLEARUNDOBUFFER )
 *
 * Clears the undo buffer in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function clears the undo buffer in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_CLEARUNDOBUFFER )
{
   SendMessage( hmg_par_raw_HWND( 1 ), EM_EMPTYUNDOBUFFER, 0, 0 );
}

/*
 * HB_FUNC( RICHEDITBOX_CANPASTE )
 *
 * Checks if text can be pasted into a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   BOOL: Returns TRUE if text can be pasted, otherwise FALSE.
 *
 * Purpose:
 *   This function checks if text can be pasted into a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_CANPASTE )
{
   hb_retl( ( BOOL ) SendMessage( hmg_par_raw_HWND( 1 ), EM_CANPASTE, 0, 0 ) );
}

/*
 * HB_FUNC( RICHEDITBOX_CANUNDO )
 *
 * Checks if the last action can be undone in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   BOOL: Returns TRUE if the last action can be undone, otherwise FALSE.
 *
 * Purpose:
 *   This function checks if the last action can be undone in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_CANUNDO )
{
   hb_retl( ( BOOL ) SendMessage( hmg_par_raw_HWND( 1 ), EM_CANUNDO, 0, 0 ) );
}

/*
 * HB_FUNC( RICHEDITBOX_CANREDO )
 *
 * Checks if the last undone action can be redone in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   BOOL: Returns TRUE if the last undone action can be redone, otherwise FALSE.
 *
 * Purpose:
 *   This function checks if the last undone action can be redone in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_CANREDO )
{
   hb_retl( ( BOOL ) SendMessage( hmg_par_raw_HWND( 1 ), EM_CANREDO, 0, 0 ) );
}

/*
 * HB_FUNC( RICHEDITBOX_GETRECT )
 *
 * Gets the rectangle of a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *
 * Return Value:
 *   Array: Returns an array containing the left, top, right, and bottom coordinates of the rectangle.
 *
 * Purpose:
 *   This function gets the rectangle of a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_GETRECT )
{
   RECT  rc;
   SendMessage( hmg_par_raw_HWND( 1 ), EM_GETRECT, ( WPARAM ) 0, ( LPARAM ) & rc );
   hb_reta( 4 );
   HB_STORNI( rc.left, -1, 1 );
   HB_STORNI( rc.top, -1, 2 );
   HB_STORNI( rc.right, -1, 3 );
   HB_STORNI( rc.bottom, -1, 4 );
}

/*
 * HB_FUNC( RICHEDITBOX_SETRECT )
 *
 * Sets the rectangle of a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. Array { nLeft, nTop, nRight, nBottom }: Array containing the left, top, right, and bottom coordinates of the rectangle.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function sets the rectangle of a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SETRECT )
{
   RECT  rc;
   rc.left = HB_PARNI( 2, 1 );
   rc.top = HB_PARNI( 2, 2 );
   rc.right = HB_PARNI( 2, 3 );
   rc.bottom = HB_PARNI( 2, 4 );
   SendMessage( hmg_par_raw_HWND( 1 ), EM_SETRECT, ( WPARAM ) 1, ( LPARAM ) & rc );
}

/*
 * HB_FUNC( RICHEDITBOX_PASTESPECIAL )
 *
 * Pastes a specific clipboard format into a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. ClipboardFormat: The clipboard format to paste.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function pastes a specific clipboard format into a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_PASTESPECIAL )
{
   if( HB_ISCHAR( 2 ) )
   {
      CHAR  *ClipboardFormat = ( CHAR * ) hb_parc( 2 );
      SendMessage( hmg_par_raw_HWND( 1 ), EM_PASTESPECIAL, ( WPARAM ) ClipboardFormat, ( LPARAM ) NULL );
   }
   else
   {
      WPARAM   ClipboardFormat = ( WPARAM ) hb_parnl( 2 );
      SendMessage( hmg_par_raw_HWND( 1 ), EM_PASTESPECIAL, ( WPARAM ) ClipboardFormat, ( LPARAM ) NULL );
   }
}

/*
 * HB_FUNC( RICHEDITBOX_SETMARGINS )
 *
 * Sets the margins of a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. WORD LeftMargin: Left margin in pixels.
 *   3. WORD RightMargin: Right margin in pixels.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function sets the margins of a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_SETMARGINS )
{
   HWND  hWndControl = hmg_par_raw_HWND( 1 );
   WORD  LeftMargin = hmg_par_WORD( 2 );  // in pixels
   WORD  RightMargin = hmg_par_WORD( 3 ); // in pixels
   SendMessage( hWndControl, EM_SETMARGINS, EC_USEFONTINFO, MAKELPARAM( LeftMargin, RightMargin ) );
}

/*
 * HB_FUNC( RICHEDITBOX_FORMATRANGE )
 *
 * Formats a range of text in a Rich Edit control for printing.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. HDC hDCPrinter: Handle to the printer device context.
 *   3. int nLeft: Left coordinate in twips.
 *   4. int nTop: Top coordinate in twips.
 *   5. int nRight: Right coordinate in twips.
 *   6. int nBottom: Bottom coordinate in twips.
 *   7. Array { cpMin, cpMax }: Array containing the minimum and maximum character positions of the range.
 *
 * Return Value:
 *   LONG: Returns the minimum character position of the formatted range.
 *
 * Purpose:
 *   This function formats a range of text in a Rich Edit control for printing.
 */
HB_FUNC( RICHEDITBOX_FORMATRANGE )
{
   FORMATRANGE FormatRange;
   RECT        rc;
   LONG        cpMin;
   HWND        hWndControl = hmg_par_raw_HWND( 1 );
   HDC         hDCPrinter = hmg_par_raw_HDC( 2 );
   rc.left = hb_parni( 3 );               // in twips
   rc.top = hb_parni( 4 );                // in twips
   rc.right = hb_parni( 5 );              // in twips
   rc.bottom = hb_parni( 6 );             // in twips
   FormatRange.hdc = hDCPrinter;
   FormatRange.hdcTarget = hDCPrinter;
   FormatRange.rc = rc;
   FormatRange.rcPage = rc;
   FormatRange.chrg.cpMin = hmg_parv_LONG( 7, 1 );
   FormatRange.chrg.cpMax = hmg_parv_LONG( 7, 2 );
   cpMin = ( LONG ) SendMessage( hWndControl, EM_FORMATRANGE, ( WPARAM ) ( BOOL ) TRUE, ( LPARAM ) & FormatRange );
   SendMessage( hWndControl, EM_FORMATRANGE, ( WPARAM ) ( BOOL ) FALSE, ( LPARAM ) NULL );
   hmg_ret_LONG( cpMin );
}

/*
 * HB_FUNC( RICHEDITBOX_POSFROMCHAR )
 *
 * Retrieves the screen coordinates of a specified character in a Rich Edit control.
 *
 * Parameters:
 *   1. HWND hWndControl: Handle to the Rich Edit control.
 *   2. LONG nPosChar: Character position.
 *
 * Return Value:
 *   Array: Returns an array containing the row and column screen coordinates of the character, or {-1, -1} if the character is not displayed.
 *
 * Purpose:
 *   This function retrieves the screen coordinates of a specified character in a Rich Edit control.
 */
HB_FUNC( RICHEDITBOX_POSFROMCHAR )
{
   HWND     hWndControl = hmg_par_raw_HWND( 1 );
   LONG     nPosChar = hmg_par_LONG( 2 );
   POINTL   PointL;
   POINT    Point;
   SendMessage( hWndControl, EM_POSFROMCHAR, ( WPARAM ) & PointL, ( LPARAM ) nPosChar );  // Retrieves the client area coordinates of a specified character in an edit control
   hb_reta( 2 );

   // A returned coordinate can be a negative value if the specified character is not displayed in the edit control's client area
   if( PointL.y < 0 || PointL.x < 0 )
   {
      Point.y = -1;
      Point.x = -1;
   }
   else
   {
      Point.x = ( INT ) PointL.x;
      Point.y = ( INT ) PointL.y;
      ClientToScreen( hWndControl, &Point );
   }

   HB_STORNI( ( INT ) Point.y, -1, 1 );
   HB_STORNI( ( INT ) Point.x, -1, 2 );
}

//********************************************************************
// by Dr. Claudio Soto ( January 2014 )
//********************************************************************
static TCHAR         cFindWhat[1024];
static TCHAR         cReplaceWith[1024];
static FINDREPLACE   FindReplace;
static HWND          hDlgFindReplace = NULL;

/*
 * HB_FUNC( REGISTERFINDMSGSTRING )
 *
 * Registers the FindMsgString message.
 *
 * Parameters:
 *   None.
 *
 * Return Value:
 *   UINT: Returns the message identifier.
 *
 * Purpose:
 *   This function registers the FindMsgString message.
 */
HB_FUNC( REGISTERFINDMSGSTRING )
{
   UINT  MessageID = RegisterWindowMessage( FINDMSGSTRING );
   hmg_ret_UINT( MessageID );
}

/*
 * HB_FUNC( FINDREPLACEDLG )
 *
 * Displays the Find/Replace dialog.
 *
 * Parameters:
 *   1. HWND hWnd: Handle to the owner window.
 *   2. BOOL NoUpDown: Flag to hide the Up/Down controls.
 *   3. BOOL NoMatchCase: Flag to hide the Match Case control.
 *   4. BOOL NoWholeWord: Flag to hide the Whole Word control.
 *   5. BOOL CheckDown: Flag to check the Down option.
 *   6. BOOL CheckMatchCase: Flag to check the Match Case option.
 *   7. BOOL CheckWholeWord: Flag to check the Whole Word option.
 *   8. LPSTR FindWhat: Text to find.
 *   9. LPSTR ReplaceWith: Text to replace with.
 *   10. BOOL lReplace: Flag to indicate if the dialog is a Replace dialog.
 *   11. LPSTR cTitle: Title of the dialog.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function displays the Find/Replace dialog.
 */
HB_FUNC( FINDREPLACEDLG )
{
   HWND     hWnd = HB_ISNIL( 1 ) ? GetActiveWindow() : hmg_par_raw_HWND( 1 );
   BOOL     NoUpDown = ( BOOL ) ( HB_ISNIL( 2 ) ? FALSE : hb_parl( 2 ) );
   BOOL     NoMatchCase = ( BOOL ) ( HB_ISNIL( 3 ) ? FALSE : hb_parl( 3 ) );
   BOOL     NoWholeWord = ( BOOL ) ( HB_ISNIL( 4 ) ? FALSE : hb_parl( 4 ) );
   BOOL     CheckDown = ( BOOL ) ( HB_ISNIL( 5 ) ? TRUE : hb_parl( 5 ) );
   BOOL     CheckMatchCase = ( BOOL ) ( HB_ISNIL( 6 ) ? FALSE : hb_parl( 6 ) );
   BOOL     CheckWholeWord = ( BOOL ) ( HB_ISNIL( 7 ) ? FALSE : hb_parl( 7 ) );
   BOOL     lReplace = ( BOOL ) hb_parl( 10 );
#ifndef UNICODE
   LPSTR    FindWhat = ( LPSTR ) hb_parc( 8 );
   LPSTR    ReplaceWith = ( LPSTR ) hb_parc( 9 );
   LPSTR    cTitle = ( LPSTR ) hb_parc( 11 );
#else
   LPWSTR   FindWhat = AnsiToWide( ( char * ) hb_parc( 8 ) );
   LPWSTR   ReplaceWith = AnsiToWide( ( char * ) hb_parc( 9 ) );
   LPWSTR   cTitle = AnsiToWide( ( char * ) hb_parc( 11 ) );
#endif
   if( hDlgFindReplace == NULL )
   {
      ZeroMemory( &FindReplace, sizeof( FindReplace ) );
      lstrcpy( cFindWhat, FindWhat );
      lstrcpy( cReplaceWith, ReplaceWith );
      FindReplace.lStructSize = sizeof( FindReplace );
      FindReplace.Flags = ( NoUpDown ? FR_HIDEUPDOWN : 0 ) | ( NoMatchCase ? FR_HIDEMATCHCASE : 0 ) | ( NoWholeWord ? FR_HIDEWHOLEWORD : 0 ) | ( CheckDown ? FR_DOWN : 0 ) | ( CheckMatchCase ? FR_MATCHCASE : 0 ) | ( CheckWholeWord ? FR_WHOLEWORD : 0 );
      FindReplace.hwndOwner = hWnd;
      FindReplace.lpstrFindWhat = cFindWhat;
      FindReplace.wFindWhatLen = sizeof( cFindWhat ) / sizeof( TCHAR );
      FindReplace.lpstrReplaceWith = cReplaceWith;
      FindReplace.wReplaceWithLen = sizeof( cReplaceWith ) / sizeof( TCHAR );
      if( lReplace )
      {
         hDlgFindReplace = ReplaceText( &FindReplace );
      }
      else
      {
         hDlgFindReplace = FindText( &FindReplace );
      }

      if( HB_ISCHAR( 11 ) )
      {
         SetWindowText( hDlgFindReplace, cTitle );
      }

      ShowWindow( hDlgFindReplace, SW_SHOW );
   }

#ifdef UNICODE
   hb_xfree( FindWhat );
   hb_xfree( ReplaceWith );
   hb_xfree( cTitle );
#endif
}

/*
 * HB_FUNC( FINDREPLACEDLGSETTITLE )
 *
 * Sets the title of the Find/Replace dialog.
 *
 * Parameters:
 *   1. LPSTR cTitle: Title of the dialog.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function sets the title of the Find/Replace dialog.
 */
HB_FUNC( FINDREPLACEDLGSETTITLE )
{
#ifndef UNICODE
   LPCSTR   cTitle = ( LPCSTR ) hb_parc( 1 );
#else
   LPCWSTR  cTitle = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   if( hDlgFindReplace != NULL )
   {
      SetWindowText( hDlgFindReplace, cTitle );
   }

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) cTitle );
#endif
}

/*
 * HB_FUNC( FINDREPLACEDLGGETTITLE )
 *
 * Gets the title of the Find/Replace dialog.
 *
 * Parameters:
 *   None.
 *
 * Return Value:
 *   LPSTR: Returns the title of the dialog.
 *
 * Purpose:
 *   This function gets the title of the Find/Replace dialog.
 */
HB_FUNC( FINDREPLACEDLGGETTITLE )
{
#ifdef UNICODE
   LPSTR pStr;
#endif
   TCHAR cTitle[256];
   if( hDlgFindReplace != NULL )
   {
      GetWindowText( hDlgFindReplace, cTitle, sizeof( cTitle ) / sizeof( TCHAR ) );
#ifndef UNICODE
      hb_retc( cTitle );
#else
      pStr = WideToAnsi( cTitle );
      hb_retc( pStr );
      hb_xfree( pStr );
#endif
   }
   else
   {
#ifndef UNICODE
      hb_retc( "" );
#else
      pStr = WideToAnsi( TEXT( "" ) );
      hb_retc( pStr );
      hb_xfree( pStr );
#endif
   }
}

/*
 * HB_FUNC( FINDREPLACEDLGSHOW )
 *
 * Shows or hides the Find/Replace dialog.
 *
 * Parameters:
 *   1. BOOL lShow: Flag to indicate if the dialog should be shown or hidden.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function shows or hides the Find/Replace dialog.
 */
HB_FUNC( FINDREPLACEDLGSHOW )
{
   BOOL  lShow = HB_ISNIL( 1 ) ? TRUE : hb_parl( 1 );
   if( hDlgFindReplace != NULL )
   {
      if( lShow )
      {
         ShowWindow( hDlgFindReplace, SW_SHOW );
      }
      else
      {
         ShowWindow( hDlgFindReplace, SW_HIDE );
      }
   }
}

/*
 * HB_FUNC( FINDREPLACEDLGGETHANDLE )
 *
 * Gets the handle of the Find/Replace dialog.
 *
 * Parameters:
 *   None.
 *
 * Return Value:
 *   HWND: Returns the handle of the dialog.
 *
 * Purpose:
 *   This function gets the handle of the Find/Replace dialog.
 */
HB_FUNC( FINDREPLACEDLGGETHANDLE )
{
   hmg_ret_raw_HWND( hDlgFindReplace );
}

/*
 * HB_FUNC( FINDREPLACEDLGISRELEASE )
 *
 * Checks if the Find/Replace dialog is released.
 *
 * Parameters:
 *   None.
 *
 * Return Value:
 *   BOOL: Returns TRUE if the dialog is released, otherwise FALSE.
 *
 * Purpose:
 *   This function checks if the Find/Replace dialog is released.
 */
HB_FUNC( FINDREPLACEDLGISRELEASE )
{
   hb_retl( ( BOOL ) ( hDlgFindReplace == NULL ) );
}

/*
 * HB_FUNC( FINDREPLACEDLGRELEASE )
 *
 * Releases the Find/Replace dialog.
 *
 * Parameters:
 *   1. BOOL lDestroy: Flag to indicate if the dialog should be destroyed.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This function releases the Find/Replace dialog.
 */
HB_FUNC( FINDREPLACEDLGRELEASE )
{
   BOOL  lDestroy = HB_ISNIL( 1 ) ? TRUE : hb_parl( 1 );
   if( hDlgFindReplace != NULL && lDestroy )
   {
      DestroyWindow( hDlgFindReplace );
   }

   hDlgFindReplace = NULL;
}

/*
 * HB_FUNC( FINDREPLACEDLGGETOPTIONS )
 *
 * Gets the options of the Find/Replace dialog.
 *
 * Parameters:
 *   1. LPARAM lParam: Pointer to the FINDREPLACE structure.
 *
 * Return Value:
 *   Array: Returns an array containing the options of the dialog.
 *
 * Purpose:
 *   This function gets the options of the Find/Replace dialog.
 */
HB_FUNC( FINDREPLACEDLGGETOPTIONS )
{
#ifdef UNICODE
   LPSTR       pStr, pStr2;
#endif
   LPARAM      lParam = hmg_par_raw_LPARAM( 1 );
   FINDREPLACE *FR = ( FINDREPLACE * ) lParam;
   LONG        nRet = -1;
   if( FR->Flags & FR_DIALOGTERM )
   {
      nRet = 0;
   }

   if( FR->Flags & FR_FINDNEXT )
   {
      nRet = 1;
   }

   if( FR->Flags & FR_REPLACE )
   {
      nRet = 2;
   }

   if( FR->Flags & FR_REPLACEALL )
   {
      nRet = 3;
   }

   hb_reta( 6 );
   HB_STORVNL( ( LONG ) nRet, -1, 1 );
#ifndef UNICODE
   HB_STORC( FR->lpstrFindWhat, -1, 2 );
#else
   pStr = WideToAnsi( FR->lpstrFindWhat );
   HB_STORC( pStr, -1, 2 );
   hb_xfree( pStr );
#endif
#ifndef UNICODE
   HB_STORC( FR->lpstrReplaceWith, -1, 3 );
#else
   pStr2 = WideToAnsi( FR->lpstrReplaceWith );
   HB_STORC( pStr2, -1, 3 );
   hb_xfree( pStr2 );
#endif
   HB_STORL( ( BOOL ) ( FR->Flags & FR_DOWN ), -1, 4 );
   HB_STORL( ( BOOL ) ( FR->Flags & FR_MATCHCASE ), -1, 5 );
   HB_STORL( ( BOOL ) ( FR->Flags & FR_WHOLEWORD ), -1, 6 );
}
