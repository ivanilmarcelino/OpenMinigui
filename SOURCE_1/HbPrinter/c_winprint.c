/*
 * HBPRINTER - Harbour Win32 Printing library source code
 *
 * Copyright 2002-2005 Richard Rylko <rrylko@poczta.onet.pl>
 */

#define NO_LEAN_AND_MEAN

#include <mgdefs.h>

#if defined( _MSC_VER )
#pragma warning( disable : 4996 )
#endif
#include "hbapiitm.h"

#if defined( _MSC_VER )
#pragma warning( push )
#pragma warning( disable : 4201 )  /* warning C4201: nonstandard extension used: nameless struct/union */
#endif
#include <olectl.h>
#if defined( _MSC_VER )
#pragma warning( pop )
#endif
#include <commctrl.h>

#ifdef __XHARBOUR__
#define HB_PARC    hb_parc
#define HB_PARNL3  hb_parnl
#else
#define HB_PARC    hb_parvc
#if defined( _WIN64 )
#define HB_PARNL3  hb_parvnll
#else
#define HB_PARNL3  hb_parvnl
#endif
#endif
#if defined( _MSC_VER ) && ! defined( __POCC__ )
#define itoa( __value, __string, __radix )  _itoa( __value, __string, __radix )
#define ltoa( __value, __string, __radix )  _ltoa( __value, __string, __radix )
#endif
static HDC              hDC    = NULL;
static HDC              hDCRef = NULL;
static HDC              hDCtemp;
static DEVMODE *        pDevMode  = NULL;
static DEVMODE *        pDevMode2 = NULL;
static DEVNAMES *       pDevNames = NULL;
static HANDLE           hPrinter  = NULL;
static PRINTER_INFO_2 * pi2       = NULL;
static PRINTER_INFO_2 * pi22      = NULL;  // to restore printer dev mode after print.
static PRINTER_DEFAULTS pd;
static PRINTDLG         pdlg;
static DOCINFO          di;
static int              nFromPage = 0;
static int              nToPage   = 0;
static TCHAR            PrinterName[ 128 ];
static TCHAR            PrinterDefault[ 128 ];
static DWORD            charset = DEFAULT_CHARSET;
static HFONT            hfont;
static HPEN             hpen;
static HBRUSH           hbrush;
static int              textjust     = 0;
static int              devcaps[]    = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 };
static int              preview      = 0;
static int              polyfillmode = 1;
static HRGN             hrgn         = NULL;
static HBITMAP          himgbmp;
static HBITMAP          hbmp[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
static OSVERSIONINFO    osvi;

#ifdef UNICODE
LPWSTR                  AnsiToWide( LPCSTR );
LPSTR                   WideToAnsi( LPWSTR );
#endif
HINSTANCE               GetResources( void );
void                    rr_getdevmode( void );

HB_FUNC( RR_FINISH )
{
   pDevMode  = NULL;
   pDevMode2 = NULL;
   pDevNames = NULL;
   ClosePrinter( hPrinter );
   hPrinter = NULL;
   pi2      = NULL;
   memset( &pd, 0, sizeof( pd ) );
   memset( &pdlg, 0, sizeof( pdlg ) );
   memset( &di, 0, sizeof( di ) );
   nFromPage = 0;
   nToPage   = 0;
   hfont     = NULL;
   hpen      = NULL;
   hbrush    = NULL;
   textjust  = 0;
   memset( &devcaps, 0, sizeof( devcaps ) );
   devcaps[ 15 ] = 1;
   preview       = 0;
   polyfillmode  = 1;
   hrgn    = NULL;
   himgbmp = NULL;
   memset( &hbmp, 0, sizeof( hbmp ) );
}

HB_FUNC( RR_PRINTERNAME )
{
   #ifndef UNICODE
   hb_retc( PrinterName );
   #else
   LPSTR pStr = WideToAnsi( PrinterName );
   hb_retc( pStr );
   hb_xfree( pStr );
   #endif
}

HB_FUNC( RR_PRINTDIALOG )
{
   TCHAR * pDevice;

   memset( &pdlg, 0, sizeof( pdlg ) );
   pdlg.lStructSize = sizeof( pdlg );
   pdlg.hDevMode    = ( HANDLE ) NULL;
   pdlg.hDevNames   = ( HANDLE ) NULL;
   pdlg.Flags       = PD_RETURNDC | PD_ALLPAGES;
   pdlg.hwndOwner   = GetActiveWindow(); // Identifies the window that owns the dialog box.
   pdlg.hDC         = NULL;
   pdlg.nCopies     = 1;
   pdlg.nFromPage   = 1;
   pdlg.nToPage     = ( unsigned short int ) -1;
   pdlg.nMinPage    = 1;
   pdlg.nMaxPage    = 0xFFFF;

   if( PrintDlg( &pdlg ) )
   {
      hDC = pdlg.hDC;

      if( hDC == NULL )
      {
         lstrcpy( PrinterName, TEXT( "" ) );
      }
      else
      {
         pDevMode  = ( LPDEVMODE ) GlobalLock( pdlg.hDevMode );
         pDevNames = ( LPDEVNAMES ) GlobalLock( pdlg.hDevNames );

         // Note: pDevMode->dmDeviceName is limited to 32 characters.
         // if the printer name is greater than 32, like network printers,
         // the rr_getdc() function return a null handle. So, I'm using
         // pDevNames instead pDevMode. (E.F.)
         //strcpy(PrinterName,pDevMode->dmDeviceName);

         pDevice = ( TCHAR * ) pDevNames + pDevNames->wDeviceOffset;
         lstrcpy( PrinterName, ( TCHAR * ) pDevice );

         HB_STORNI( pdlg.nFromPage, 1, 1 );
         HB_STORNI( pdlg.nToPage, 1, 2 );
         HB_STORNI( pDevMode->dmCopies > 1 ? pDevMode->dmCopies : pdlg.nCopies, 1, 3 );
         if( ( pdlg.Flags & PD_PAGENUMS ) == PD_PAGENUMS )
         {
            HB_STORNI( 2, 1, 4 );
         }
         else if( ( pdlg.Flags & PD_SELECTION ) == PD_SELECTION )
         {
            HB_STORNI( 1, 1, 4 );
         }
         else
         {
            HB_STORNI( 0, 1, 4 );
         }

         rr_getdevmode();

         GlobalUnlock( pdlg.hDevMode );
         GlobalUnlock( pdlg.hDevNames );
      }
   }
   else
   {
      hDC = 0;
   }

   hDCRef = hDC;

   hmg_ret_raw_HDC( hDC );
}

HB_FUNC( RR_GETDC )
{
   #ifdef UNICODE
   LPWSTR pwszDevice = AnsiToWide( ( char * ) hb_parc( 1 ) );

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {
      hDC = CreateDC( TEXT( "WINSPOOL" ), pwszDevice, NULL, NULL );
   }
   else
   {
      hDC = CreateDC( NULL, pwszDevice, NULL, NULL );
   }
   #else
   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {
      hDC = CreateDC( "WINSPOOL", hb_parc( 1 ), NULL, NULL );
   }
   else
   {
      hDC = CreateDC( NULL, hb_parc( 1 ), NULL, NULL );
   }
   #endif
   if( hDC )
   {
      #ifdef UNICODE
      lstrcpy( PrinterName, pwszDevice );
      #else
      strcpy( PrinterName, hb_parc( 1 ) );
      #endif
      rr_getdevmode();
   }

   hDCRef = hDC;
   hmg_ret_raw_HDC( hDC );

   #ifdef UNICODE
   hb_xfree( pwszDevice );
   #endif
}

void rr_getdevmode( void )
{
   DWORD dwNeeded = 0;

   memset( &pd, 0, sizeof( pd ) );
   pd.DesiredAccess = PRINTER_ALL_ACCESS;
   OpenPrinter( PrinterName, &hPrinter, NULL );
   GetPrinter( hPrinter, 2, 0, 0, &dwNeeded );
   pi2 = ( PRINTER_INFO_2 * ) GlobalAlloc( GPTR, dwNeeded );
   GetPrinter( hPrinter, 2, ( LPBYTE ) pi2, dwNeeded, &dwNeeded );
   pi22 = ( PRINTER_INFO_2 * ) GlobalAlloc( GPTR, dwNeeded );
   GetPrinter( hPrinter, 2, ( LPBYTE ) pi22, dwNeeded, &dwNeeded );
   if( pDevMode )
   {
      pi2->pDevMode = pDevMode;
   }
   else if( pi2->pDevMode == NULL )
   {
      dwNeeded  = DocumentProperties( NULL, hPrinter, PrinterName, NULL, NULL, 0 );
      pDevMode2 = ( DEVMODE * ) GlobalAlloc( GPTR, dwNeeded );
      DocumentProperties( NULL, hPrinter, PrinterName, pDevMode2, NULL, DM_OUT_BUFFER );
      pi2->pDevMode = pDevMode2;
   }

   hfont  = ( HFONT ) GetCurrentObject( hDC, OBJ_FONT );
   hbrush = ( HBRUSH ) GetCurrentObject( hDC, OBJ_BRUSH );
   hpen   = ( HPEN ) GetCurrentObject( hDC, OBJ_PEN );
}

HB_FUNC( EF_RESETPRINTER )
{
   if( pi22 )
   {
      SetPrinter( hPrinter, 2, ( LPBYTE ) pi22, 0 );
   }

   GlobalFree( pi22 );
   pi22 = NULL;
}

HB_FUNC( RR_DELETEDC )
{
   if( pDevMode )
   {
      GlobalFree( pDevMode );
   }

   if( pDevMode2 )
   {
      GlobalFree( pDevMode2 );
   }

   if( pDevNames )
   {
      GlobalFree( pDevNames );
   }

   if( pi2 )
   {
      GlobalFree( pi2 );
   }

   DeleteDC( hmg_par_raw_HDC( 1 ) );
}

HB_FUNC( RR_GETDEVICECAPS )
{
   TEXTMETRIC tm;
   UINT       i;
   HFONT      xfont = hmg_par_raw_HFONT( 2 );

   if( xfont != 0 )
   {
      SelectObject( hDCRef, xfont );
   }

   GetTextMetrics( hDCRef, &tm );
   devcaps[ 1 ]  = GetDeviceCaps( hDCRef, VERTSIZE );
   devcaps[ 2 ]  = GetDeviceCaps( hDCRef, HORZSIZE );
   devcaps[ 3 ]  = GetDeviceCaps( hDCRef, VERTRES );
   devcaps[ 4 ]  = GetDeviceCaps( hDCRef, HORZRES );
   devcaps[ 5 ]  = GetDeviceCaps( hDCRef, LOGPIXELSY );
   devcaps[ 6 ]  = GetDeviceCaps( hDCRef, LOGPIXELSX );
   devcaps[ 7 ]  = GetDeviceCaps( hDCRef, PHYSICALHEIGHT );
   devcaps[ 8 ]  = GetDeviceCaps( hDCRef, PHYSICALWIDTH );
   devcaps[ 9 ]  = GetDeviceCaps( hDCRef, PHYSICALOFFSETY );
   devcaps[ 10 ] = GetDeviceCaps( hDCRef, PHYSICALOFFSETX );

   devcaps[ 11 ] = tm.tmHeight;
   devcaps[ 12 ] = tm.tmAveCharWidth;
   devcaps[ 13 ] = ( int ) ( ( devcaps[ 3 ] - tm.tmAscent ) / tm.tmHeight );
   devcaps[ 14 ] = ( int ) ( devcaps[ 4 ] / tm.tmAveCharWidth );
   devcaps[ 15 ] = ( int ) pi2->pDevMode->dmOrientation;
   devcaps[ 16 ] = ( int ) tm.tmAscent;
   devcaps[ 17 ] = ( int ) pi2->pDevMode->dmPaperSize;
   for( i = 1; i <= hb_parinfa( 1, 0 ); i++ )
   {
      HB_STORNI( devcaps[ i ], 1, i );
   }

   if( xfont != 0 )
   {
      SelectObject( hDCRef, hfont );
   }
}

HB_FUNC( RR_SETDEVMODE )
{
   DWORD what = hb_parnl( 1 );

   if( what == ( pi2->pDevMode->dmFields & what ) )
   {
      pi2->pDevMode->dmFields = pi2->pDevMode->dmFields | what;

      if( what == DM_ORIENTATION )
      {
         pi2->pDevMode->dmOrientation = hmg_par_short( 2 );
      }

      if( what == DM_PAPERSIZE )
      {
         pi2->pDevMode->dmPaperSize = hmg_par_short( 2 );
      }

      if( what == DM_SCALE )
      {
         pi2->pDevMode->dmScale = hmg_par_short( 2 );
      }

      if( what == DM_COPIES )
      {
         pi2->pDevMode->dmCopies = hmg_par_short( 2 );
      }

      if( what == DM_DEFAULTSOURCE )
      {
         pi2->pDevMode->dmDefaultSource = hmg_par_short( 2 );
      }

      if( what == DM_PRINTQUALITY )
      {
         pi2->pDevMode->dmPrintQuality = hmg_par_short( 2 );
      }

      if( what == DM_COLOR )
      {
         pi2->pDevMode->dmColor = hmg_par_short( 2 );
      }

      if( what == DM_DUPLEX )
      {
         pi2->pDevMode->dmDuplex = hmg_par_short( 2 );
      }
   }

   DocumentProperties( NULL, hPrinter, PrinterName, pi2->pDevMode, pi2->pDevMode, DM_IN_BUFFER | DM_OUT_BUFFER );
   SetPrinter( hPrinter, 2, ( LPBYTE ) pi2, 0 );
   ResetDC( hDCRef, pi2->pDevMode );
   hmg_ret_raw_HDC( hDCRef );
}

HB_FUNC( RR_SETUSERMODE )
{
   DWORD what = hb_parnl( 1 );

   if( what == ( pi2->pDevMode->dmFields & what ) )
   {
      pi2->pDevMode->dmFields      = pi2->pDevMode->dmFields | DM_PAPERSIZE | DM_PAPERWIDTH | DM_PAPERLENGTH;
      pi2->pDevMode->dmPaperSize   = DMPAPER_USER;
      pi2->pDevMode->dmPaperWidth  = hmg_par_short( 2 );
      pi2->pDevMode->dmPaperLength = hmg_par_short( 3 );
   }

   DocumentProperties( NULL, hPrinter, PrinterName, pi2->pDevMode, pi2->pDevMode, DM_IN_BUFFER | DM_OUT_BUFFER );
   SetPrinter( hPrinter, 2, ( LPBYTE ) pi2, 0 );
   ResetDC( hDCRef, pi2->pDevMode );
   hmg_ret_raw_HDC( hDCRef );
}

#ifdef UNICODE
typedef BOOL ( WINAPI * _GETDEFAULTPRINTER )( LPWSTR, LPDWORD );
#define GETDEFAULTPRINTER  "GetDefaultPrinterW"
#else
typedef BOOL ( WINAPI * _GETDEFAULTPRINTER )( LPSTR, LPDWORD );
#define GETDEFAULTPRINTER  "GetDefaultPrinterA"
#endif
#define MAX_BUFFER_SIZE    254

HB_FUNC( RR_GETDEFAULTPRINTER )
{
   DWORD Needed, Returned;
   DWORD BuffSize = MAX_BUFFER_SIZE;
   LPPRINTER_INFO_5 PrinterInfo;

   #ifdef UNICODE
   LPSTR pStr;
   #endif
   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS ) /* Windows 95 or 98 */
   {
      EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 5, NULL, 0, &Needed, &Returned );
      PrinterInfo = ( LPPRINTER_INFO_5 ) LocalAlloc( LPTR, Needed );
      EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 5, ( LPBYTE ) PrinterInfo, Needed, &Needed, &Returned );
      lstrcpy( PrinterDefault, PrinterInfo->pPrinterName );
      LocalFree( PrinterInfo );
   }
   else if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {
      if( osvi.dwMajorVersion == 5 )                     /* Windows 2000 or XP */
      {
         BOOL  bFlag;
         TCHAR lpPrinterName[ MAX_BUFFER_SIZE ];
         _GETDEFAULTPRINTER fnGetDefaultPrinter;

         HMODULE hWinSpool = LoadLibrary( TEXT( "winspool.drv" ) );
         if( ! hWinSpool )
         {
            hb_retc( "" );
            return;
         }

         fnGetDefaultPrinter = ( _GETDEFAULTPRINTER ) GetProcAddress( hWinSpool, GETDEFAULTPRINTER );
         if( ! fnGetDefaultPrinter )
         {
            FreeLibrary( hWinSpool );
            hb_retc( "" );
            return;
         }

         bFlag = ( *fnGetDefaultPrinter )( lpPrinterName, &BuffSize );
         lstrcpy( PrinterDefault, lpPrinterName );
         FreeLibrary( hWinSpool );
         if( ! bFlag )
         {
            hb_retc( "" );
            return;
         }
      }
      else                 /* Windows NT 4.0 or earlier */
      {
         GetProfileString( TEXT( "windows" ), TEXT( "device" ), TEXT( "" ), PrinterDefault, BuffSize );
         _tcstok( PrinterDefault, TEXT( "," ) );
      }
   }

   #ifndef UNICODE
   hb_retc( PrinterDefault );
   #else
   pStr = WideToAnsi( PrinterDefault );
   hb_retc( pStr );
   hb_xfree( pStr );
   #endif
}

#undef MAX_BUFFER_SIZE
#undef GETDEFAULTPRINTER
HB_FUNC( RR_GETPRINTERS )
{
   DWORD   dwSize     = 0;
   DWORD   dwPrinters = 0;
   DWORD   i;
   HGLOBAL pBuffer;
   HGLOBAL cBuffer;
   PRINTER_INFO_4 * pInfo4 = NULL;
   PRINTER_INFO_5 * pInfo5 = NULL;
   DWORD level;
   DWORD flags;

   #ifdef UNICODE
   LPSTR pStr;
   #endif
   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( &osvi );
   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {
      level = 4;
      flags = PRINTER_ENUM_CONNECTIONS | PRINTER_ENUM_LOCAL;
   }
   else
   {
      level = 5;
      flags = PRINTER_ENUM_LOCAL;
   }

   EnumPrinters( flags, NULL, level, NULL, 0, &dwSize, &dwPrinters );

   pBuffer = ( char * ) GlobalAlloc( GPTR, dwSize );
   if( pBuffer == NULL )
   {
      hb_retc( ",," );
      return;
   }

   EnumPrinters( flags, NULL, level, ( LPBYTE ) pBuffer, dwSize, &dwSize, &dwPrinters );

   if( dwPrinters == 0 )
   {
      hb_retc( ",," );
      GlobalFree( pBuffer );
      return;
   }

   cBuffer = ( char * ) GlobalAlloc( GPTR, dwPrinters * 256 );

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {
      pInfo4 = ( PRINTER_INFO_4 * ) pBuffer;
   }
   else
   {
      pInfo5 = ( PRINTER_INFO_5 * ) pBuffer;
   }

   for( i = 0; i < dwPrinters; i++ )
   {
      if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
      {
         lstrcat( cBuffer, pInfo4->pPrinterName );
         lstrcat( cBuffer, TEXT( "," ) );
         if( pInfo4->Attributes == PRINTER_ATTRIBUTE_LOCAL )
         {
            lstrcat( cBuffer, TEXT( "local printer" ) );
         }
         else
         {
            lstrcat( cBuffer, TEXT( "network printer" ) );
         }

         pInfo4++;
      }
      else
      {
         lstrcat( cBuffer, pInfo5->pPrinterName );
         lstrcat( cBuffer, TEXT( "," ) );
         lstrcat( cBuffer, pInfo5->pPortName );
         pInfo5++;
      }

      if( i < dwPrinters - 1 )
      {
         lstrcat( cBuffer, TEXT( ",," ) );
      }
   }

   #ifndef UNICODE
   hb_retc( ( const char * ) cBuffer );
   #else
   pStr = WideToAnsi( cBuffer );
   hb_retc( pStr );
   hb_xfree( pStr );
   #endif
   GlobalFree( pBuffer );
   GlobalFree( cBuffer );
}

HB_FUNC( RR_STARTDOC )
{
   #ifndef UNICODE
   LPTSTR lpText = ( LPTSTR ) hb_parc( 1 );
   #else
   LPWSTR lpText = AnsiToWide( ( char * ) hb_parc( 1 ) );
   #endif
   memset( &di, 0, sizeof( di ) );
   di.cbSize      = sizeof( di );
   di.lpszDocName = lpText;
   StartDoc( hDC, &di );

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpText );
   #endif
}

HB_FUNC( RR_STARTPAGE )
{
   StartPage( hDC );
   SetTextAlign( hDC, TA_BASELINE );
}

HB_FUNC( RR_ENDPAGE )
{
   EndPage( hDC );
}

HB_FUNC( RR_ENDDOC )
{
   EndDoc( hDC );
}

HB_FUNC( RR_ABORTDOC )
{
   AbortDoc( hDC );
   DeleteDC( hDC );
}

HB_FUNC( RR_DEVICECAPABILITIES )
{
   HGLOBAL cGBuffer, pGBuffer, nGBuffer, sGBuffer, bnGBuffer, bwGBuffer, bcGBuffer;
   TCHAR * cBuffer, * pBuffer, * nBuffer, * sBuffer, * bnBuffer, * bwBuffer, * bcBuffer;
   DWORD   numpapers, numbins, i;
   LPPOINT lp;
   TCHAR   buffer[ sizeof( long ) * 8 + 1 ];

   #ifdef UNICODE
   LPSTR pStr;
   #endif
   #ifdef UNICODE
   numpapers = DeviceCapabilities( AnsiToWide( ( LPSTR ) pi2->pPrinterName ), AnsiToWide( ( LPSTR ) pi2->pPortName ), DC_PAPERNAMES, NULL, NULL );
   #else
   numpapers = DeviceCapabilities( pi2->pPrinterName, pi2->pPortName, DC_PAPERNAMES, NULL, NULL );
   #endif
   if( numpapers > 0 )
   {
      pGBuffer = GlobalAlloc( GPTR, numpapers * 64 );
      nGBuffer = GlobalAlloc( GPTR, numpapers * sizeof( WORD ) );
      sGBuffer = GlobalAlloc( GPTR, numpapers * sizeof( POINT ) );
      cGBuffer = GlobalAlloc( GPTR, numpapers * 128 );
      pBuffer  = ( TCHAR * ) pGBuffer;
      nBuffer  = ( TCHAR * ) nGBuffer;
      sBuffer  = ( TCHAR * ) sGBuffer;
      cBuffer  = ( TCHAR * ) cGBuffer;
      #ifdef UNICODE
      DeviceCapabilities( AnsiToWide( ( LPSTR ) pi2->pPrinterName ), AnsiToWide( ( LPSTR ) pi2->pPortName ), DC_PAPERNAMES, pBuffer, pi2->pDevMode );
      DeviceCapabilities( AnsiToWide( ( LPSTR ) pi2->pPrinterName ), AnsiToWide( ( LPSTR ) pi2->pPortName ), DC_PAPERS, nBuffer, pi2->pDevMode );
      DeviceCapabilities( AnsiToWide( ( LPSTR ) pi2->pPrinterName ), AnsiToWide( ( LPSTR ) pi2->pPortName ), DC_PAPERSIZE, sBuffer, pi2->pDevMode );
      #else
      DeviceCapabilities( pi2->pPrinterName, pi2->pPortName, DC_PAPERNAMES, pBuffer, pi2->pDevMode );
      DeviceCapabilities( pi2->pPrinterName, pi2->pPortName, DC_PAPERS, nBuffer, pi2->pDevMode );
      DeviceCapabilities( pi2->pPrinterName, pi2->pPortName, DC_PAPERSIZE, sBuffer, pi2->pDevMode );
      #endif
      cBuffer[ 0 ] = 0;
      for( i = 0; i < numpapers; i++ )
      {
         lstrcat( cBuffer, pBuffer );
         lstrcat( cBuffer, TEXT( "," ) );
         lstrcat( cBuffer, _itot( *nBuffer, buffer, 10 ) );
         lstrcat( cBuffer, TEXT( "," ) );

         lp = ( LPPOINT ) sBuffer;
         lstrcat( cBuffer, _ltot( lp->x, buffer, 10 ) );
         lstrcat( cBuffer, TEXT( "," ) );
         lstrcat( cBuffer, _ltot( lp->y, buffer, 10 ) );
         if( i < numpapers - 1 )
         {
            lstrcat( cBuffer, TEXT( ",," ) );
         }

         pBuffer += 64;
         nBuffer += sizeof( WORD );
         sBuffer += sizeof( POINT );
      }

      #ifndef UNICODE
      hb_storc( cBuffer, 1 );
      #else
      pStr = WideToAnsi( cBuffer );
      hb_storc( pStr, 1 );
      hb_xfree( pStr );
      #endif
      GlobalFree( cGBuffer );
      GlobalFree( pGBuffer );
      GlobalFree( nGBuffer );
      GlobalFree( sGBuffer );
   }
   else
   {
      hb_storc( "", 1 );
   }

   #ifdef UNICODE
   numbins = DeviceCapabilities( AnsiToWide( ( LPSTR ) pi2->pPrinterName ), AnsiToWide( ( LPSTR ) pi2->pPortName ), DC_BINNAMES, NULL, NULL );
   #else
   numbins = DeviceCapabilities( pi2->pPrinterName, pi2->pPortName, DC_BINNAMES, NULL, NULL );
   #endif
   if( numbins > 0 )
   {
      bnGBuffer = GlobalAlloc( GPTR, numbins * 24 );
      bwGBuffer = GlobalAlloc( GPTR, numbins * sizeof( WORD ) );
      bcGBuffer = GlobalAlloc( GPTR, numbins * 64 );
      bnBuffer  = ( TCHAR * ) bnGBuffer;
      bwBuffer  = ( TCHAR * ) bwGBuffer;
      bcBuffer  = ( TCHAR * ) bcGBuffer;
      #ifdef UNICODE
      DeviceCapabilities( AnsiToWide( ( LPSTR ) pi2->pPrinterName ), AnsiToWide( ( LPSTR ) pi2->pPortName ), DC_BINNAMES, bnBuffer, pi2->pDevMode );
      DeviceCapabilities( AnsiToWide( ( LPSTR ) pi2->pPrinterName ), AnsiToWide( ( LPSTR ) pi2->pPortName ), DC_BINS, bwBuffer, pi2->pDevMode );
      #else
      DeviceCapabilities( pi2->pPrinterName, pi2->pPortName, DC_BINNAMES, bnBuffer, pi2->pDevMode );
      DeviceCapabilities( pi2->pPrinterName, pi2->pPortName, DC_BINS, bwBuffer, pi2->pDevMode );
      #endif
      bcBuffer[ 0 ] = 0;
      for( i = 0; i < numbins; i++ )
      {
         lstrcat( bcBuffer, bnBuffer );
         lstrcat( bcBuffer, TEXT( "," ) );
         lstrcat( bcBuffer, _itot( *bwBuffer, buffer, 10 ) );

         if( i < numbins - 1 )
         {
            lstrcat( bcBuffer, TEXT( ",," ) );
         }

         bnBuffer += 24;
         bwBuffer += sizeof( WORD );
      }

      #ifndef UNICODE
      hb_storc( bcBuffer, 2 );
      #else
      pStr = WideToAnsi( bcBuffer );
      hb_storc( pStr, 2 );
      hb_xfree( pStr );
      #endif
      GlobalFree( bnGBuffer );
      GlobalFree( bwGBuffer );
      GlobalFree( bcGBuffer );
   }
   else
   {
      hb_storc( "", 2 );
   }
}

HB_FUNC( RR_SETPOLYFILLMODE )
{
   if( SetPolyFillMode( hDC, hb_parni( 1 ) ) != 0 )
   {
      hb_retni( hb_parni( 1 ) );
   }
   else
   {
      hb_retni( GetPolyFillMode( hDC ) );
   }
}

HB_FUNC( RR_SETTEXTCOLOR )
{
   if( SetTextColor( hDC, hmg_par_COLORREF( 1 ) ) != CLR_INVALID )
   {
      hmg_ret_COLORREF( hmg_par_COLORREF( 1 ) );
   }
   else
   {
      hmg_ret_COLORREF( GetTextColor( hDC ) );
   }
}

HB_FUNC( RR_SETBKCOLOR )
{
   if( SetBkColor( hDC, hmg_par_COLORREF( 1 ) ) != CLR_INVALID )
   {
      hmg_ret_COLORREF( hmg_par_COLORREF( 1 ) );
   }
   else
   {
      hmg_ret_COLORREF( GetBkColor( hDC ) );
   }
}

HB_FUNC( RR_SETBKMODE )
{
   if( hb_parni( 1 ) == 1 )
   {
      SetBkMode( hDC, TRANSPARENT );
   }
   else
   {
      SetBkMode( hDC, OPAQUE );
   }
}

HB_FUNC( RR_DELETEOBJECTS )
{
   UINT i;

   for( i = 2; i <= hb_parinfa( 1, 0 ); i++ )
   {
      DeleteObject( ( HGDIOBJ ) HB_PARVNL( 1, i ) );
   }
}

HB_FUNC( RR_DELETEIMAGELISTS )
{
   UINT i;

   for( i = 1; i <= hb_parinfa( 1, 0 ); i++ )
   {
      ImageList_Destroy( ( HIMAGELIST ) HB_PARNL3( 1, i, 1 ) );
   }
}

HB_FUNC( RR_SAVEMETAFILE )
{
   #ifndef UNICODE
   LPSTR FileName = ( LPSTR ) hb_parc( 2 );
   #else
   LPWSTR FileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
   #endif
   CopyEnhMetaFile( ( HENHMETAFILE ) HB_PARNL( 1 ), FileName );

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) FileName );
   #endif
}

HB_FUNC( RR_GETCURRENTOBJECT )
{
   int     what = hb_parni( 1 );
   HGDIOBJ hand;

   if( what == 1 )
   {
      hand = GetCurrentObject( hDC, OBJ_FONT );
   }
   else if( what == 2 )
   {
      hand = GetCurrentObject( hDC, OBJ_BRUSH );
   }
   else
   {
      hand = GetCurrentObject( hDC, OBJ_PEN );
   }

   hmg_ret_raw_HGDIOBJ( hand );
}

HB_FUNC( RR_GETSTOCKOBJECT )
{
   hmg_ret_raw_HGDIOBJ( GetStockObject( hb_parni( 1 ) ) );
}

HB_FUNC( RR_CREATEPEN )
{
   hmg_ret_raw_HANDLE( CreatePen( hb_parni( 1 ), hb_parni( 2 ), hmg_par_COLORREF( 3 ) ) );
}

HB_FUNC( RR_MODIFYPEN )
{
   LOGPEN ppn;
   int    i;
   HPEN   hp;

   memset( &ppn, 0, sizeof( LOGPEN ) );
   i = GetObject( hmg_par_raw_HPEN( 1 ), sizeof( LOGPEN ), &ppn );
   if( i > 0 )
   {
      if( hb_parni( 2 ) >= 0 )
      {
         ppn.lopnStyle = hmg_par_UINT( 2 );
      }

      if( hb_parnl( 3 ) >= 0 )
      {
         ppn.lopnWidth.x = hb_parnl( 3 );
      }

      if( hb_parnl( 4 ) >= 0 )
      {
         ppn.lopnColor = hmg_par_COLORREF( 4 );
      }

      hp = CreatePenIndirect( &ppn );
      if( hp != NULL )
      {
         DeleteObject( hmg_par_raw_HPEN( 1 ) );
         hmg_ret_raw_HANDLE( hp );
      }
      else
      {
         hmg_ret_raw_HANDLE( hmg_par_raw_HPEN( 1 ) );
      }
   }
   else
   {
      hmg_ret_raw_HANDLE( hmg_par_raw_HPEN( 1 ) );
   }
}

HB_FUNC( RR_SELECTPEN )
{
   SelectObject( hDC, hmg_par_raw_HPEN( 1 ) );
   hpen = hmg_par_raw_HPEN( 1 );
}

HB_FUNC( RR_CREATEBRUSH )
{
   LOGBRUSH pbr;

   pbr.lbStyle = hmg_par_UINT( 1 );
   pbr.lbColor = hmg_par_COLORREF( 2 );
   pbr.lbHatch = hmg_par_LONG( 3 );

   hmg_ret_raw_HBRUSH( CreateBrushIndirect( &pbr ) );
}

HB_FUNC( RR_MODIFYBRUSH )
{
   LOGBRUSH ppn;
   int      i;
   HBRUSH   hb;

   memset( &ppn, 0, sizeof( LOGBRUSH ) );
   i = GetObject( hmg_par_raw_HBRUSH( 1 ), sizeof( LOGBRUSH ), &ppn );
   if( i > 0 )
   {
      if( hb_parni( 2 ) >= 0 )
      {
         ppn.lbStyle = hmg_par_UINT( 2 );
      }

      if( hb_parnl( 3 ) >= 0 )
      {
         ppn.lbColor = hmg_par_COLORREF( 3 );
      }

      if( hb_parnl( 4 ) >= 0 )
      {
         ppn.lbHatch = hmg_par_LONG( 4 );
      }

      hb = CreateBrushIndirect( &ppn );
      if( hb != NULL )
      {
         DeleteObject( hmg_par_raw_HBRUSH( 1 ) );
         hmg_ret_raw_HBRUSH( hb );
      }
      else
      {
         hmg_ret_raw_HBRUSH( hmg_par_raw_HBRUSH( 1 ) );
      }
   }
   else
   {
      hmg_ret_raw_HBRUSH( hmg_par_raw_HBRUSH( 1 ) );
   }
}

HB_FUNC( RR_SELECTBRUSH )
{
   SelectObject( hDC, hmg_par_raw_HBRUSH( 1 ) );
   hbrush = hmg_par_raw_HBRUSH( 1 );
}

HB_FUNC( RR_CREATEFONT )
{
   #ifndef UNICODE
   TCHAR * FontName = ( TCHAR * ) hb_parc( 1 );
   #else
   TCHAR * FontName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   #endif
   int        FontSize = hb_parni( 2 );
   LONG       FontWidth = hb_parnl( 3 );
   LONG       Orient = hb_parnl( 4 );
   LONG       Weight = hb_parnl( 5 );
   int        Italic = hb_parni( 6 );
   int        Underline = hb_parni( 7 );
   int        Strikeout = hb_parni( 8 );
   HFONT      oldfont, hxfont;
   LONG       newWidth, FontHeight;
   TEXTMETRIC tm;
   BYTE       bItalic, bUnderline, bStrikeOut;

   newWidth = ( LONG ) FontWidth;
   if( FontSize <= 0 )
   {
      FontSize = 10;
   }

   if( FontWidth < 0 )
   {
      newWidth = 0;
   }

   if( Orient <= 0 )
   {
      Orient = 0;
   }

   if( Weight <= 0 )
   {
      Weight = FW_NORMAL;
   }
   else
   {
      Weight = FW_BOLD;
   }

   if( Italic <= 0 )
   {
      bItalic = 0;
   }
   else
   {
      bItalic = 1;
   }

   if( Underline <= 0 )
   {
      bUnderline = 0;
   }
   else
   {
      bUnderline = 1;
   }

   if( Strikeout <= 0 )
   {
      bStrikeOut = 0;
   }
   else
   {
      bStrikeOut = 1;
   }

   FontHeight = -MulDiv( FontSize, GetDeviceCaps( hDCRef, LOGPIXELSY ), 72 );
   hxfont     = CreateFont( FontHeight, newWidth, Orient, Orient, Weight, bItalic, bUnderline, bStrikeOut, charset, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_DONTCARE, FontName );
   if( FontWidth < 0 )
   {
      oldfont = ( HFONT ) SelectObject( hDC, hxfont );
      GetTextMetrics( hDC, &tm );
      SelectObject( hDC, oldfont );
      DeleteObject( hxfont );
      newWidth = ( int ) ( ( float ) -( tm.tmAveCharWidth + tm.tmOverhang ) * FontWidth / 100 );
      hxfont   = CreateFont( FontHeight, newWidth, Orient, Orient, Weight, bItalic, bUnderline, bStrikeOut, charset, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_DONTCARE, FontName );
   }

   hmg_ret_raw_HANDLE( hxfont );

   #ifdef UNICODE
   hb_xfree( FontName );
   #endif
}

HB_FUNC( RR_MODIFYFONT )
{
   LOGFONT ppn;
   int     i;
   HFONT   hf;
   LONG    nHeight;

   memset( &ppn, 0, sizeof( LOGFONT ) );
   i = GetObject( hmg_par_raw_HFONT( 1 ), sizeof( LOGFONT ), &ppn );
   if( i > 0 )
   {
      //     if (hb_parc(2)!="")
      //       ppn.lfFaceName = hb_parc(2);

      if( hb_parni( 3 ) > 0 )
      {
         nHeight      = -MulDiv( hb_parni( 3 ), GetDeviceCaps( hDC, LOGPIXELSY ), 72 );
         ppn.lfHeight = nHeight;
      }

      if( hb_parnl( 4 ) >= 0 )
      {
         ppn.lfWidth = ( LONG ) hb_parnl( 4 ) * ppn.lfWidth / 100;
      }

      if( hb_parnl( 5 ) >= 0 )
      {
         ppn.lfOrientation = hb_parnl( 5 );
         ppn.lfEscapement  = hb_parnl( 5 );
      }

      if( hb_parnl( 6 ) >= 0 )
      {
         if( hb_parnl( 6 ) == 0 )
         {
            ppn.lfWeight = FW_NORMAL;
         }
         else
         {
            ppn.lfWeight = FW_BOLD;
         }
      }

      if( hb_parni( 7 ) >= 0 )
      {
         ppn.lfItalic = hmg_par_BYTE( 7 );
      }

      if( hb_parni( 8 ) >= 0 )
      {
         ppn.lfUnderline = hmg_par_BYTE( 8 );
      }

      if( hb_parni( 9 ) >= 0 )
      {
         ppn.lfStrikeOut = hmg_par_BYTE( 9 );
      }

      hf = CreateFontIndirect( &ppn );
      if( hf != NULL )
      {
         DeleteObject( hmg_par_raw_HFONT( 1 ) );
         hmg_ret_raw_HANDLE( hf );
      }
      else
      {
         hmg_ret_raw_HANDLE( hmg_par_raw_HFONT( 1 ) );
      }
   }
   else
   {
      hmg_ret_raw_HANDLE( hmg_par_raw_HFONT( 1 ) );
   }
}

HB_FUNC( RR_SELECTFONT )
{
   SelectObject( hDC, hmg_par_raw_HFONT( 1 ) );
   hfont = hmg_par_raw_HFONT( 1 );
}

HB_FUNC( RR_SETCHARSET )
{
   charset = hmg_par_DWORD( 1 );
}

HB_FUNC( RR_TEXTOUT )
{
   #ifndef UNICODE
   LPTSTR lpText = ( LPTSTR ) hb_parc( 1 );
   #else
   LPWSTR lpText = AnsiToWide( ( char * ) hb_parc( 1 ) );
   #endif
   HGDIOBJ xfont    = hmg_par_raw_HFONT( 3 );
   HFONT   prevfont = NULL;
   SIZE    szMetric;
   int     lspace = hb_parni( 4 );

   if( xfont != 0 )
   {
      prevfont = ( HFONT ) SelectObject( hDC, xfont );
   }

   if( textjust > 0 )
   {
      GetTextExtentPoint32( hDC, lpText, ( int ) lstrlen( lpText ), &szMetric );
      if( szMetric.cx < textjust )
      {                    // or can be for better look (szMetric.cx>(int) textjust*2/3)
         if( lspace > 0 )
         {
            SetTextJustification( hDC, ( int ) textjust - szMetric.cx, lspace );
         }
      }
   }

   hb_retl( TextOut( hDC, HB_PARNI( 2, 2 ), HB_PARNI( 2, 1 ) + devcaps[ 16 ], lpText, ( int ) lstrlen( lpText ) ) );
   if( xfont != 0 )
   {
      SelectObject( hDC, prevfont );
   }

   if( textjust > 0 )
   {
      SetTextJustification( hDC, 0, 0 );
   }

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpText );
   #endif
}

HB_FUNC( RR_DRAWTEXT )
{
   #ifndef UNICODE
   LPCSTR pszData = hb_parc( 3 );
   #else
   LPCWSTR pszData = AnsiToWide( ( char * ) hb_parc( 3 ) );
   #endif
   int     iLen     = ( int ) lstrlen( pszData );
   HGDIOBJ xfont    = hmg_par_raw_HFONT( 5 );
   HFONT   prevfont = NULL;
   RECT    rect;
   UINT    uFormat;

   SIZE sSize;
   UINT iStyle = hmg_par_UINT( 4 );
   LONG w, h;

   SetRect( &rect, HB_PARNI( 1, 2 ), HB_PARNI( 1, 1 ), HB_PARNI( 2, 2 ), HB_PARNI( 2, 1 ) );

   if( xfont != 0 )
   {
      prevfont = ( HFONT ) SelectObject( hDC, xfont );
   }

   GetTextExtentPoint32( hDC, pszData, iLen, &sSize );
   w = ( LONG ) sSize.cx;  // text width
   h = ( LONG ) sSize.cy;  // text height

   // Center text vertically within rectangle

   if( w < rect.right - rect.left )
   {
      rect.top = rect.top + ( rect.bottom - rect.top + h / 2 ) / 2;
   }
   else
   {
      rect.top = rect.top + ( rect.bottom - rect.top - h / 2 ) / 2;
   }

   uFormat = DT_NOCLIP | DT_NOPREFIX | DT_WORDBREAK | DT_END_ELLIPSIS;

   if( iStyle == 0 )
   {
      uFormat = uFormat | DT_LEFT;
   }
   else if( iStyle == 2 )
   {
      uFormat = uFormat | DT_RIGHT;
   }
   else if( iStyle == 1 )
   {
      uFormat = uFormat | DT_CENTER;
   }

   hmg_ret_NINT( DrawText( hDC, pszData, -1, &rect, uFormat ) );
   if( xfont != 0 )
   {
      SelectObject( hDC, prevfont );
   }

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) pszData );
   #endif
}

HB_FUNC( RR_RECTANGLE )
{
   HPEN   xpen   = hmg_par_raw_HPEN( 3 );
   HBRUSH xbrush = hmg_par_raw_HBRUSH( 4 );

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   if( xbrush != 0 )
   {
      SelectObject( hDC, ( HBRUSH ) xbrush );
   }

   hb_retni( Rectangle( hDC, HB_PARNI( 1, 2 ), HB_PARNI( 1, 1 ), HB_PARNI( 2, 2 ), HB_PARNI( 2, 1 ) ) );
   if( xpen != 0 )
   {
      SelectObject( hDC, hpen );
   }

   if( xbrush != 0 )
   {
      SelectObject( hDC, hbrush );
   }
}

HB_FUNC( RR_CLOSEMFILE )
{
   DeleteEnhMetaFile( CloseEnhMetaFile( hDC ) );
}

HB_FUNC( RR_CREATEMFILE )
{
   #ifndef UNICODE
   LPSTR FileName = ( LPSTR ) hb_parc( 1 );
   #else
   LPWSTR FileName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   #endif
   RECT emfrect;

   SetRect( &emfrect, 0, 0, GetDeviceCaps( hDCRef, HORZSIZE ) * 100, GetDeviceCaps( hDCRef, VERTSIZE ) * 100 );
   hDC = CreateEnhMetaFile( hDCRef, FileName, &emfrect, TEXT( "hbprinter\0emf file\0\0" ) );
   SetTextAlign( hDC, TA_BASELINE );
   preview = 1;
   hmg_ret_raw_HDC( hDC );

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) FileName );
   #endif
}

HB_FUNC( RR_DELETECLIPRGN )
{
   SelectClipRgn( hDC, NULL );
}

HB_FUNC( RR_CREATERGN )
{
   POINT lpp;

   GetViewportOrgEx( hDC, &lpp );
   if( hb_parni( 3 ) == 2 )
   {
      hmg_ret_raw_HANDLE( CreateEllipticRgn( HB_PARNI( 1, 2 ) + lpp.x, HB_PARNI( 1, 1 ) + lpp.y, HB_PARNI( 2, 2 ) + lpp.x, HB_PARNI( 2, 1 ) + lpp.y ) );
   }
   else if( hb_parni( 3 ) == 3 )
   {
      hmg_ret_raw_HANDLE( CreateRoundRectRgn( HB_PARNI( 1, 2 ) + lpp.x, HB_PARNI( 1, 1 ) + lpp.y, HB_PARNI( 2, 2 ) + lpp.x, HB_PARNI( 2, 1 ) + lpp.y, HB_PARNI( 4, 2 ) + lpp.x, HB_PARNI( 4, 1 ) + lpp.y ) );
   }
   else
   {
      hmg_ret_raw_HANDLE( CreateRectRgn( HB_PARNI( 1, 2 ) + lpp.x, HB_PARNI( 1, 1 ) + lpp.y, HB_PARNI( 2, 2 ) + lpp.x, HB_PARNI( 2, 1 ) + lpp.y ) );
   }
}

HB_FUNC( RR_CREATEPOLYGONRGN )
{
   int   number = ( int ) hb_parinfa( 1, 0 );
   int   i;
   POINT apoints[ 1024 ];

   for( i = 0; i <= number - 1; i++ )
   {
      apoints[ i ].x = HB_PARNI( 1, i + 1 );
      apoints[ i ].y = HB_PARNI( 2, i + 1 );
   }

   hmg_ret_raw_HANDLE( CreatePolygonRgn( apoints, number, hb_parni( 3 ) ) );
}

HB_FUNC( RR_COMBINERGN )
{
   HRGN rgnnew = CreateRectRgn( 0, 0, 1, 1 );

   CombineRgn( rgnnew, hmg_par_raw_HRGN( 1 ), hmg_par_raw_HRGN( 2 ), hb_parni( 3 ) );
   hmg_ret_raw_HANDLE( rgnnew );
}

HB_FUNC( RR_SELECTCLIPRGN )
{
   SelectClipRgn( hDC, hmg_par_raw_HRGN( 1 ) );
   hrgn = hmg_par_raw_HRGN( 1 );
}

HB_FUNC( RR_SETVIEWPORTORG )
{
   hb_retl( SetViewportOrgEx( hDC, HB_PARNI( 1, 2 ), HB_PARNI( 1, 1 ), NULL ) );
}

HB_FUNC( RR_GETVIEWPORTORG )
{
   POINT lpp;

   hb_retl( GetViewportOrgEx( hDC, &lpp ) );
   HB_STORVNL( lpp.x, 1, 2 );
   HB_STORVNL( lpp.y, 1, 1 );
}

HB_FUNC( RR_SETRGB )
{
   hmg_ret_NINT( RGB( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

HB_FUNC( RR_SETTEXTCHAREXTRA )
{
   hmg_ret_NINT( SetTextCharacterExtra( hDC, hb_parni( 1 ) ) );
}

HB_FUNC( RR_GETTEXTCHAREXTRA )
{
   hmg_ret_NINT( GetTextCharacterExtra( hDC ) );
}

HB_FUNC( RR_SETTEXTJUSTIFICATION )
{
   textjust = hb_parni( 1 );
}

HB_FUNC( RR_GETTEXTJUSTIFICATION )
{
   hb_retni( textjust );
}

HB_FUNC( RR_GETTEXTALIGN )
{
   hmg_ret_UINT( GetTextAlign( hDC ) );
}

HB_FUNC( RR_SETTEXTALIGN )
{
   hmg_ret_UINT( SetTextAlign( hDC, TA_BASELINE | hb_parni( 1 ) ) );
}

HB_FUNC( RR_PICTURE )
{
   IStream *  iStream;
   IPicture * iPicture;
   HGLOBAL    hGlobal;
   void *     pGlobal;
   HANDLE     hFile;
   DWORD      nFileSize;
   DWORD      nReadByte;
   long       lWidth, lHeight;
   int        x, y, xe, ye;

   #ifndef UNICODE
   LPSTR cFileName = ( LPSTR ) hb_parc( 1 );
   #else
   LPWSTR cFileName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   #endif
   int   r   = HB_PARNI( 2, 1 );
   int   c   = HB_PARNI( 2, 2 );
   int   dr  = HB_PARNI( 3, 1 );
   int   dc  = HB_PARNI( 3, 2 );
   int   tor = HB_PARNI( 4, 1 );
   int   toc = HB_PARNI( 4, 2 );
   HRGN  hrgn1;
   POINT lpp;

   hFile = CreateFile( cFileName, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) cFileName );
   #endif
   if( hFile == INVALID_HANDLE_VALUE )
   {
      return;
   }

   nFileSize = GetFileSize( hFile, NULL );
   hGlobal   = GlobalAlloc( GMEM_MOVEABLE, nFileSize );
   pGlobal   = GlobalLock( hGlobal );
   ReadFile( hFile, pGlobal, nFileSize, &nReadByte, NULL );
   CloseHandle( hFile );
   GlobalUnlock( hGlobal );
   CreateStreamOnHGlobal( hGlobal, TRUE, &iStream );
   OleLoadPicture( iStream, nFileSize, TRUE, &IID_IPicture, ( LPVOID * ) &iPicture );
   GlobalFree( hGlobal );
   iStream->lpVtbl->Release( iStream );
   if( iPicture == NULL )
   {
      return;
   }

   iPicture->lpVtbl->get_Width( iPicture, &lWidth );
   iPicture->lpVtbl->get_Height( iPicture, &lHeight );
   if( dc == 0 )
   {
      dc = ( int ) ( ( float ) dr * lWidth / lHeight );
   }

   if( dr == 0 )
   {
      dr = ( int ) ( ( float ) dc * lHeight / lWidth );
   }

   if( tor <= 0 )
   {
      tor = dr;
   }

   if( toc <= 0 )
   {
      toc = dc;
   }

   x  = c;
   y  = r;
   xe = c + toc - 1;
   ye = r + tor - 1;
   GetViewportOrgEx( hDC, &lpp );
   hrgn1 = CreateRectRgn( c + lpp.x, r + lpp.y, xe + lpp.x, ye + lpp.y );
   if( hrgn == NULL )
   {
      SelectClipRgn( hDC, hrgn1 );
   }
   else
   {
      ExtSelectClipRgn( hDC, hrgn1, RGN_AND );
   }

   while( x < xe )
   {
      while( y < ye )
      {
         iPicture->lpVtbl->Render( iPicture, hDC, x, y, dc, dr, 0, lHeight, lWidth, -lHeight, NULL );
         y += dr;
      }

      y  = r;
      x += dc;
   }

   iPicture->lpVtbl->Release( iPicture );
   SelectClipRgn( hDC, hrgn );
   DeleteObject( hrgn1 );
   hb_retni( 0 );
}

LPVOID rr_loadpicturefromresource( TCHAR * resname, LONG * lwidth, LONG * lheight )
{
   HBITMAP    hbmpx;
   IPicture * iPicture = NULL;
   IStream *  iStream  = NULL;
   PICTDESC   picd;
   HGLOBAL    hGlobalres;
   HGLOBAL    hGlobal;
   HRSRC      hSource;
   LPVOID     lpVoid;
   int        nSize;

   hbmpx = ( HBITMAP ) LoadImage( GetResources(), resname, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
   if( hbmpx != NULL )
   {
      picd.cbSizeofstruct = sizeof( PICTDESC );
      picd.picType        = PICTYPE_BITMAP;
      picd.bmp.hbitmap    = hbmpx;
      OleCreatePictureIndirect( &picd, &IID_IPicture, TRUE, ( LPVOID * ) &iPicture );
   }
   else
   {
      hSource = FindResource( GetResources(), resname, TEXT( "HMGPICTURE" ) );
      if( hSource == NULL )
      {
         return NULL;
      }

      hGlobalres = LoadResource( GetResources(), hSource );
      if( hGlobalres == NULL )
      {
         return NULL;
      }

      lpVoid = LockResource( hGlobalres );
      if( lpVoid == NULL )
      {
         return NULL;
      }

      nSize   = SizeofResource( GetResources(), hSource );
      hGlobal = GlobalAlloc( GPTR, nSize );
      if( hGlobal == NULL )
      {
         return NULL;
      }

      memcpy( hGlobal, lpVoid, nSize );
      FreeResource( hGlobalres );
      CreateStreamOnHGlobal( hGlobal, TRUE, &iStream );
      if( iStream == NULL )
      {
         GlobalFree( hGlobal );
         return NULL;
      }

      OleLoadPicture( iStream, nSize, TRUE, &IID_IPicture, ( LPVOID * ) &iPicture );
      iStream->lpVtbl->Release( iStream );
      GlobalFree( hGlobal );
   }

   if( iPicture != NULL )
   {
      iPicture->lpVtbl->get_Width( iPicture, lwidth );
      iPicture->lpVtbl->get_Height( iPicture, lheight );
   }

   return iPicture;
}

LPVOID rr_loadpicture( TCHAR * filename, LONG * lwidth, LONG * lheight )
{
   IStream *  iStream  = NULL;
   IPicture * iPicture = NULL;
   HGLOBAL    hGlobal;
   void *     pGlobal;
   HANDLE     hFile;
   DWORD      nFileSize, nReadByte;

   hFile = CreateFile( filename, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );
   if( hFile == INVALID_HANDLE_VALUE )
   {
      return NULL;
   }

   nFileSize = GetFileSize( hFile, NULL );
   hGlobal   = GlobalAlloc( GMEM_MOVEABLE, nFileSize + 4096 );
   pGlobal   = GlobalLock( hGlobal );
   ReadFile( hFile, pGlobal, nFileSize, &nReadByte, NULL );
   CloseHandle( hFile );

   CreateStreamOnHGlobal( hGlobal, TRUE, &iStream );
   if( iStream == NULL )
   {
      GlobalUnlock( hGlobal );
      GlobalFree( hGlobal );
      return NULL;
   }

   OleLoadPicture( iStream, nFileSize, TRUE, &IID_IPicture, ( LPVOID * ) &iPicture );
   GlobalUnlock( hGlobal );
   GlobalFree( hGlobal );
   iStream->lpVtbl->Release( iStream );
   iStream = NULL;
   if( iPicture != NULL )
   {
      iPicture->lpVtbl->get_Width( iPicture, lwidth );
      iPicture->lpVtbl->get_Height( iPicture, lheight );
   }

   return iPicture;
}

LPVOID rr_loadfromhbitmap( HBITMAP hbmpx, LONG * lwidth, LONG * lheight )
{
   IPicture * iPicture = NULL;
   PICTDESC   picd;

   picd.cbSizeofstruct = sizeof( PICTDESC );
   picd.picType        = PICTYPE_BITMAP;
   picd.bmp.hbitmap    = hbmpx;
   picd.bmp.hpal       = ( HPALETTE ) NULL;

   OleCreatePictureIndirect( &picd, &IID_IPicture, TRUE, ( LPVOID * ) &iPicture );
   if( iPicture != NULL )
   {
      iPicture->lpVtbl->get_Width( iPicture, lwidth );
      iPicture->lpVtbl->get_Height( iPicture, lheight );
   }

   return iPicture;
}

HB_FUNC( RR_DRAWPICTURE )
{
   #ifndef UNICODE
   LPSTR cFileName = ( LPSTR ) hb_parc( 1 );
   #else
   LPWSTR cFileName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   #endif
   IPicture * ipic;
   int        x, y, xe, ye;
   int        r       = HB_PARNI( 2, 1 );
   int        c       = HB_PARNI( 2, 2 );
   int        dr      = HB_PARNI( 3, 1 );
   int        dc      = HB_PARNI( 3, 2 );
   int        tor     = HB_PARNI( 4, 1 );
   int        toc     = HB_PARNI( 4, 2 );
   long       lwidth  = 0;
   long       lheight = 0;
   RECT       lrect;
   HRGN       hrgn1;
   POINT      lpp;
   int        lw, lh;

   ipic = ( IPicture * ) rr_loadpicture( cFileName, &lwidth, &lheight );
   if( ipic == NULL )
   {
      ipic = ( IPicture * ) rr_loadpicturefromresource( cFileName, &lwidth, &lheight );
   }

   if( ipic == NULL && HB_ISNUM( 1 ) )
   {
      ipic = ( IPicture * ) rr_loadfromhbitmap( hmg_par_raw_HBITMAP( 1 ), &lwidth, &lheight );
   }

   if( ipic == NULL )
   {
      return;
   }

   lw = MulDiv( lwidth, devcaps[ 6 ], 2540 );
   lh = MulDiv( lheight, devcaps[ 5 ], 2540 );
   if( dc == 0 )
   {
      dc = ( int ) ( ( float ) dr * lw / lh );
   }

   if( dr == 0 )
   {
      dr = ( int ) ( ( float ) dc * lh / lw );
   }

   if( tor <= 0 )
   {
      tor = dr;
   }

   if( toc <= 0 )
   {
      toc = dc;
   }

   x  = c;
   y  = r;
   xe = c + toc - 1;
   ye = r + tor - 1;
   GetViewportOrgEx( hDC, &lpp );
   hrgn1 = CreateRectRgn( c + lpp.x, r + lpp.y, xe + lpp.x, ye + lpp.y );
   if( hrgn == NULL )
   {
      SelectClipRgn( hDC, hrgn1 );
   }
   else
   {
      ExtSelectClipRgn( hDC, hrgn1, RGN_AND );
   }

   while( x < xe )
   {
      while( y < ye )
      {
         SetRect( &lrect, x, y, dc + x, dr + y );
         ipic->lpVtbl->Render( ipic, hDC, x, y, dc, dr, 0, lheight, lwidth, -lheight, &lrect );
         y += dr;
      }

      y  = r;
      x += dc;
   }

   ipic->lpVtbl->Release( ipic );
   SelectClipRgn( hDC, hrgn );
   DeleteObject( hrgn1 );
   hb_retni( 0 );

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) cFileName );
   #endif
}

HB_FUNC( RR_CREATEIMAGELIST )
{
   #ifndef UNICODE
   LPSTR cFileName = ( LPSTR ) hb_parc( 1 );
   #else
   LPWSTR cFileName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   #endif
   HBITMAP    hbmpx;
   BITMAP     bm;
   HIMAGELIST himl;
   int        dx, number;

   hbmpx = ( HBITMAP ) LoadImage( 0, cFileName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION );
   if( hbmpx == NULL )
   {
      hbmpx = ( HBITMAP ) LoadImage( GetModuleHandle( NULL ), cFileName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
   }

   if( hbmpx == NULL )
   {
      return;
   }

   GetObject( hbmpx, sizeof( BITMAP ), &bm );
   number = HB_ISNIL( 2 ) ? 0 : hb_parni( 2 );
   if( number == 0 )
   {
      number = ( int ) bm.bmWidth / bm.bmHeight;
      dx     = bm.bmHeight;
   }
   else
   {
      dx = ( int ) bm.bmWidth / number;
   }

   himl = ImageList_Create( dx, bm.bmHeight, ILC_COLOR24 | ILC_MASK, number, 0 );
   ImageList_AddMasked( himl, hbmpx, CLR_DEFAULT );
   hb_storni( dx, 3 );
   hb_storni( bm.bmHeight, 4 );
   DeleteObject( hbmpx );
   hmg_ret_raw_HANDLE( himl );

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) cFileName );
   #endif
}

HB_FUNC( RR_DRAWIMAGELIST )
{
   HIMAGELIST himl = hmg_par_raw_HIMAGELIST( 1 );
   HDC        tempdc, temp2dc;
   HBITMAP    hbmpx;
   RECT       rect;
   HWND       hwnd = GetActiveWindow();

   rect.left   = HB_PARNI( 3, 2 );
   rect.top    = HB_PARNI( 3, 1 );
   rect.right  = HB_PARNI( 4, 2 );
   rect.bottom = HB_PARNI( 4, 1 );
   temp2dc     = GetWindowDC( hwnd );
   tempdc      = CreateCompatibleDC( temp2dc );
   hbmpx       = CreateCompatibleBitmap( temp2dc, hb_parni( 5 ), hb_parni( 6 ) );
   ReleaseDC( hwnd, temp2dc );
   SelectObject( tempdc, hbmpx );
   BitBlt( tempdc, 0, 0, hb_parni( 5 ), hb_parni( 6 ), tempdc, 0, 0, WHITENESS );
   if( hb_parnl( 8 ) >= 0 )
   {
      ImageList_SetBkColor( himl, hmg_par_COLORREF( 8 ) );
   }

   ImageList_Draw( himl, hb_parni( 2 ) - 1, tempdc, 0, 0, hb_parni( 7 ) );
   if( hb_parnl( 8 ) >= 0 )
   {
      ImageList_SetBkColor( himl, CLR_NONE );
   }

   hb_retl( StretchBlt( hDC, rect.left, rect.top, rect.right, rect.bottom, tempdc, 0, 0, hb_parni( 5 ), hb_parni( 6 ), SRCCOPY ) );
   DeleteDC( tempdc );
   DeleteObject( hbmpx );
}

HB_FUNC( RR_POLYGON )
{
   int    number = ( int ) hb_parinfa( 1, 0 );
   int    i;
   int    styl = GetPolyFillMode( hDC );
   POINT  apoints[ 1024 ];
   HPEN   xpen   = hmg_par_raw_HPEN( 3 );
   HBRUSH xbrush = hmg_par_raw_HBRUSH( 4 );

   for( i = 0; i <= number - 1; i++ )
   {
      apoints[ i ].x = HB_PARNI( 1, i + 1 );
      apoints[ i ].y = HB_PARNI( 2, i + 1 );
   }

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   if( xbrush != 0 )
   {
      SelectObject( hDC, ( HBRUSH ) xbrush );
   }

   SetPolyFillMode( hDC, hb_parni( 5 ) );

   hb_retnl( ( LONG ) Polygon( hDC, apoints, number ) );

   if( xpen != 0 )
   {
      SelectObject( hDC, hpen );
   }

   if( xbrush != 0 )
   {
      SelectObject( hDC, hbrush );
   }

   SetPolyFillMode( hDC, styl );
}

HB_FUNC( RR_POLYBEZIER )
{
   DWORD number = ( DWORD ) hb_parinfa( 1, 0 );
   DWORD i;
   POINT apoints[ 1024 ];
   HPEN  xpen = hmg_par_raw_HPEN( 3 );

   for( i = 0; i <= number - 1; i++ )
   {
      apoints[ i ].x = HB_PARNI( 1, i + 1 );
      apoints[ i ].y = HB_PARNI( 2, i + 1 );
   }

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   hb_retni( PolyBezier( hDC, apoints, number ) );

   if( xpen != 0 )
   {
      SelectObject( hDC, hpen );
   }
}

HB_FUNC( RR_POLYBEZIERTO )
{
   DWORD number = ( DWORD ) hb_parinfa( 1, 0 );
   DWORD i;
   POINT apoints[ 1024 ];
   HPEN  xpen = hmg_par_raw_HPEN( 3 );

   for( i = 0; i <= number - 1; i++ )
   {
      apoints[ i ].x = HB_PARNI( 1, i + 1 );
      apoints[ i ].y = HB_PARNI( 2, i + 1 );
   }

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   hb_retni( PolyBezierTo( hDC, apoints, number ) );

   if( xpen != 0 )
   {
      SelectObject( hDC, hpen );
   }
}

HB_FUNC( RR_GETTEXTEXTENT )
{
   #ifndef UNICODE
   LPTSTR lpText = ( LPTSTR ) hb_parc( 1 );
   #else
   LPWSTR lpText = AnsiToWide( ( char * ) hb_parc( 1 ) );
   #endif
   HFONT xfont = hmg_par_raw_HFONT( 3 );
   SIZE  szMetric;

   if( xfont != 0 )
   {
      SelectObject( hDC, ( HFONT ) xfont );
   }

   hb_retni( GetTextExtentPoint32( hDC, lpText, ( int ) lstrlen( lpText ), &szMetric ) );
   HB_STORNI( szMetric.cy, 2, 1 );
   HB_STORNI( szMetric.cx, 2, 2 );
   if( xfont != 0 )
   {
      SelectObject( hDC, hfont );
   }

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpText );
   #endif
}

HB_FUNC( RR_ROUNDRECT )
{
   HPEN   xpen   = hmg_par_raw_HPEN( 4 );
   HBRUSH xbrush = hmg_par_raw_HBRUSH( 5 );

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   if( xbrush != 0 )
   {
      SelectObject( hDC, ( HBRUSH ) xbrush );
   }

   hb_retni( RoundRect( hDC, HB_PARNI( 1, 2 ), HB_PARNI( 1, 1 ), HB_PARNI( 2, 2 ), HB_PARNI( 2, 1 ), HB_PARNI( 3, 2 ), HB_PARNI( 3, 1 ) ) );

   if( xbrush != 0 )
   {
      SelectObject( hDC, ( HBRUSH ) hbrush );
   }

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) hpen );
   }
}

HB_FUNC( RR_ELLIPSE )
{
   HPEN   xpen   = hmg_par_raw_HPEN( 3 );
   HBRUSH xbrush = hmg_par_raw_HBRUSH( 4 );

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   if( xbrush != 0 )
   {
      SelectObject( hDC, ( HBRUSH ) xbrush );
   }

   hb_retni( Ellipse( hDC, HB_PARNI( 1, 2 ), HB_PARNI( 1, 1 ), HB_PARNI( 2, 2 ), HB_PARNI( 2, 1 ) ) );

   if( xpen != 0 )
   {
      SelectObject( hDC, hpen );
   }

   if( xbrush != 0 )
   {
      SelectObject( hDC, hbrush );
   }
}

HB_FUNC( RR_CHORD )
{
   HPEN   xpen   = hmg_par_raw_HPEN( 5 );
   HBRUSH xbrush = hmg_par_raw_HBRUSH( 6 );

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   if( xbrush != 0 )
   {
      SelectObject( hDC, ( HBRUSH ) xbrush );
   }

   hb_retni( Chord( hDC, HB_PARNI( 1, 2 ), HB_PARNI( 1, 1 ), HB_PARNI( 2, 2 ), HB_PARNI( 2, 1 ), HB_PARNI( 3, 2 ), HB_PARNI( 3, 1 ), HB_PARNI( 4, 2 ), HB_PARNI( 4, 1 ) ) );

   if( xpen != 0 )
   {
      SelectObject( hDC, hpen );
   }

   if( xbrush != 0 )
   {
      SelectObject( hDC, hbrush );
   }
}

HB_FUNC( RR_ARCTO )
{
   HPEN xpen = hmg_par_raw_HPEN( 5 );

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   hb_retni( ArcTo( hDC, HB_PARNI( 1, 2 ), HB_PARNI( 1, 1 ), HB_PARNI( 2, 2 ), HB_PARNI( 2, 1 ), HB_PARNI( 3, 2 ), HB_PARNI( 3, 1 ), HB_PARNI( 4, 2 ), HB_PARNI( 4, 1 ) ) );

   if( xpen != 0 )
   {
      SelectObject( hDC, hpen );
   }
}

HB_FUNC( RR_ARC )
{
   HPEN xpen = hmg_par_raw_HPEN( 5 );

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   hb_retni( Arc( hDC, HB_PARNI( 1, 2 ), HB_PARNI( 1, 1 ), HB_PARNI( 2, 2 ), HB_PARNI( 2, 1 ), HB_PARNI( 3, 2 ), HB_PARNI( 3, 1 ), HB_PARNI( 4, 2 ), HB_PARNI( 4, 1 ) ) );

   if( xpen != 0 )
   {
      SelectObject( hDC, hpen );
   }
}

HB_FUNC( RR_PIE )
{
   HPEN   xpen   = hmg_par_raw_HPEN( 5 );
   HBRUSH xbrush = hmg_par_raw_HBRUSH( 6 );

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   if( xbrush != 0 )
   {
      SelectObject( hDC, ( HBRUSH ) xbrush );
   }

   hb_retni( Pie( hDC, HB_PARNI( 1, 2 ), HB_PARNI( 1, 1 ), HB_PARNI( 2, 2 ), HB_PARNI( 2, 1 ), HB_PARNI( 3, 2 ), HB_PARNI( 3, 1 ), HB_PARNI( 4, 2 ), HB_PARNI( 4, 1 ) ) );

   if( xpen != 0 )
   {
      SelectObject( hDC, hpen );
   }

   if( xbrush != 0 )
   {
      SelectObject( hDC, hbrush );
   }
}

HB_FUNC( RR_FILLRECT )
{
   RECT rect;

   rect.left   = HB_PARNI( 1, 2 );
   rect.top    = HB_PARNI( 1, 1 );
   rect.right  = HB_PARNI( 2, 2 );
   rect.bottom = HB_PARNI( 2, 1 );
   hb_retni( FillRect( hDC, &rect, hmg_par_raw_HBRUSH( 3 ) ) );
}

HB_FUNC( RR_FRAMERECT )
{
   RECT rect;

   rect.left   = HB_PARNI( 1, 2 );
   rect.top    = HB_PARNI( 1, 1 );
   rect.right  = HB_PARNI( 2, 2 );
   rect.bottom = HB_PARNI( 2, 1 );
   hb_retni( FrameRect( hDC, &rect, hmg_par_raw_HBRUSH( 3 ) ) );
}

HB_FUNC( RR_LINE )
{
   HPEN xpen = hmg_par_raw_HPEN( 3 );

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   MoveToEx( hDC, HB_PARNI( 1, 2 ), HB_PARNI( 1, 1 ), NULL );
   hb_retni( LineTo( hDC, HB_PARNI( 2, 2 ), HB_PARNI( 2, 1 ) ) );
   if( xpen != 0 )
   {
      SelectObject( hDC, hpen );
   }
}

HB_FUNC( RR_LINETO )
{
   HPEN xpen = hmg_par_raw_HPEN( 2 );

   if( xpen != 0 )
   {
      SelectObject( hDC, ( HPEN ) xpen );
   }

   hb_retni( LineTo( hDC, HB_PARNI( 1, 2 ), HB_PARNI( 1, 1 ) ) );
   if( xpen != 0 )
   {
      SelectObject( hDC, hpen );
   }
}

HB_FUNC( RR_INVERTRECT )
{
   RECT rect;

   rect.left   = HB_PARNI( 1, 2 );
   rect.top    = HB_PARNI( 1, 1 );
   rect.right  = HB_PARNI( 2, 2 );
   rect.bottom = HB_PARNI( 2, 1 );
   hb_retni( InvertRect( hDC, &rect ) );
}

HB_FUNC( RR_GETDESKTOPAREA )
{
   RECT rect;

   SystemParametersInfo( SPI_GETWORKAREA, 1, &rect, 0 );

   hb_reta( 4 );
   HB_STORNI( ( INT ) rect.top, -1, 1 );
   HB_STORNI( ( INT ) rect.left, -1, 2 );
   HB_STORNI( ( INT ) rect.bottom - rect.top, -1, 3 );
   HB_STORNI( ( INT ) rect.right - rect.left, -1, 4 );
}

HB_FUNC( RR_GETCLIENTRECT )
{
   RECT rect;

   GetClientRect( ( HWND ) HB_PARVNL( 1, 7 ), &rect );
   HB_STORNI( rect.top, 1, 1 );
   HB_STORNI( rect.left, 1, 2 );
   HB_STORNI( rect.bottom, 1, 3 );
   HB_STORNI( rect.right, 1, 4 );
   HB_STORNI( rect.bottom - rect.top + 1, 1, 5 );
   HB_STORNI( rect.right - rect.left + 1, 1, 6 );
}

HB_FUNC( RR_SCROLLWINDOW )
{
   ScrollWindow( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ), NULL, NULL );
}

HB_FUNC( RR_PREVIEWPLAY )
{
   RECT rect;
   HDC  imgDC = GetWindowDC( hmg_par_raw_HWND( 1 ) );
   HDC  tmpDC = CreateCompatibleDC( imgDC );

   #ifndef UNICODE
   LPSTR FileName = ( LPSTR ) HB_PARC( 2, 1 );
   #else
   LPWSTR FileName = AnsiToWide( ( char * ) HB_PARC( 2, 1 ) );
   #endif
   HENHMETAFILE hh = GetEnhMetaFile( FileName );

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) FileName );
   #endif
   if( tmpDC == NULL )
   {
      ReleaseDC( hmg_par_raw_HWND( 1 ), imgDC );
      hb_retl( 0 );
   }

   if( himgbmp != 0 )
   {
      DeleteObject( himgbmp );
   }

   SetRect( &rect, 0, 0, HB_PARNI( 3, 4 ), HB_PARNI( 3, 3 ) );
   himgbmp = CreateCompatibleBitmap( imgDC, rect.right, rect.bottom );
   DeleteObject( SelectObject( tmpDC, ( HBITMAP ) himgbmp ) );
   FillRect( tmpDC, &rect, ( HBRUSH ) GetStockObject( WHITE_BRUSH ) );
   PlayEnhMetaFile( tmpDC, hh, &rect );
   DeleteEnhMetaFile( hh );
   SendMessage( hmg_par_raw_HWND( 1 ), ( UINT ) STM_SETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) himgbmp );
   ReleaseDC( hmg_par_raw_HWND( 1 ), imgDC );
   DeleteDC( tmpDC );
   if( himgbmp == 0 )
   {
      hb_retl( 0 );
   }
   else
   {
      hb_retl( 1 );
   }
}

HB_FUNC( RR_PLAYTHUMB )
{
   RECT rect;
   HDC  tmpDC;
   HDC  imgDC = GetWindowDC( ( HWND ) HB_PARVNL( 1, 5 ) );

   #ifndef UNICODE
   LPSTR  FileName = ( LPSTR ) HB_PARC( 2, 1 );
   LPTSTR lpText   = ( LPTSTR ) hb_parc( 3 );
   #else
   LPWSTR FileName = AnsiToWide( ( char * ) HB_PARC( 2, 1 ) );
   LPWSTR lpText   = AnsiToWide( ( char * ) hb_parc( 3 ) );
   #endif
   HENHMETAFILE hh = GetEnhMetaFile( FileName );
   int          i;

   i     = hb_parni( 4 ) - 1;
   tmpDC = CreateCompatibleDC( imgDC );
   SetRect( &rect, 0, 0, HB_PARNI( 1, 4 ), HB_PARNI( 1, 3 ) );
   hbmp[ i ] = CreateCompatibleBitmap( imgDC, rect.right, rect.bottom );
   DeleteObject( SelectObject( tmpDC, hbmp[ i ] ) );
   FillRect( tmpDC, &rect, ( HBRUSH ) GetStockObject( WHITE_BRUSH ) );
   PlayEnhMetaFile( tmpDC, hh, &rect );
   DeleteEnhMetaFile( hh );
   TextOut( tmpDC, ( int ) rect.right / 2 - 5, ( int ) rect.bottom / 2 - 5, lpText, ( int ) lstrlen( lpText ) );
   SendMessage( ( HWND ) HB_PARVNL( 1, 5 ), ( UINT ) STM_SETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) hbmp[ i ] );
   ReleaseDC( ( HWND ) HB_PARVNL( 1, 5 ), imgDC );
   DeleteDC( tmpDC );

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) FileName );
   hb_xfree( ( TCHAR * ) lpText );
   #endif
}

HB_FUNC( RR_PLAYENHMETAFILE )
{
   RECT rect;

   #ifndef UNICODE
   LPSTR FileName = ( LPSTR ) HB_PARC( 1, 1 );
   #else
   LPWSTR FileName = AnsiToWide( ( char * ) HB_PARC( 1, 1 ) );
   #endif
   HENHMETAFILE hh = GetEnhMetaFile( FileName );

   SetRect( &rect, 0, 0, HB_PARNI( 1, 5 ), HB_PARNI( 1, 4 ) );
   PlayEnhMetaFile( hmg_par_raw_HDC( 2 ), hh, &rect );
   DeleteEnhMetaFile( hh );

   #ifdef UNICODE
   hb_xfree( ( TCHAR * ) FileName );
   #endif
}

HB_FUNC( RR_LALABYE )
{
   if( hb_parni( 1 ) == 1 )
   {
      hDCtemp = hDC;
      hDC     = hDCRef;
   }
   else
   {
      hDC = hDCtemp;
   }
}
