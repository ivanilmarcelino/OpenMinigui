/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2023 Artyom Verchenko <artyomskynet@gmail.com>
*/

#include "minigui.ch"

#pragma BEGINDUMP

#include <mgdefs.h>
#include <hbapi.h>
#include <shlobj.h>
#include <time.h>
#include <windows.h>
#include "hbwinuni.h"

#ifndef __XHARBOUR__
   #define ISBYREF( n )          HB_ISBYREF( n )
#endif

HB_FUNC( SENDMESSAGEDATA )
{
   HWND hwnd = ( HWND ) HB_PARNL( 1 );

   if( IsWindow( hwnd ) )
   {
      COPYDATASTRUCT cds;

      cds.dwData = ( ULONG_PTR ) hb_parni( 3 ) ;
      cds.cbData = hb_parclen( 2 );
      cds.lpData = ( char * ) hb_parc( 2 );

      SendMessage( hwnd, WM_COPYDATA, 0, ( LPARAM ) &cds );
   }
}

HB_FUNC( GETMESSAGEDATA )
{
   PCOPYDATASTRUCT pcds = ( PCOPYDATASTRUCT ) HB_PARNL( 1 );

   hb_retc_null();

   if( pcds )
   {
      if( pcds->lpData )
      {
         hb_retclen(  pcds->lpData, pcds->cbData );
      }

      if( HB_ISBYREF( 2 ) )
      {
         hb_stornl( pcds->dwData, 2 );
      }
   }
}

HB_FUNC ( FINDWINDOW )
{
   hb_retnl( ( LONG ) FindWindow( 0, hb_parc( 1 ) ) );
}
/*
HB_FUNC( TERMINATEPROCESS ) {
  hb_retni( (BOOL) TerminateProcess( (HANDLE) hb_parni(1),0) );
}
*/
HB_FUNC( UNIXTIME ) {
    hb_retnl(time(NULL));
}

HB_FUNC(TESTWINDOW)
{
   HWND hwnd = ( HWND ) HB_PARNL( 1 );
   hb_retnl(IsWindow(hwnd));
}

#pragma ENDDUMP


// -----------------------------------------------------------------
//   Возврат работы по учетной записью АДМИНИСТРАТОРА - .t. or .f.
//
#pragma BEGINDUMP

#include <windows.h>
#define HB_OS_WIN_USED
#include "hbapiitm.h"

HB_FUNC( OS_ISUSERANADMIN ) // 24/11/09 11:43
{
BOOL iResult = FALSE ;
typedef int (WINAPI *USERADMIN)( void );
HINSTANCE hLib;
USERADMIN ProcAdd;
hLib = LoadLibrary("shell32.dll");
if (hLib != NULL)
{
ProcAdd = ( USERADMIN ) GetProcAddress(hLib, "IsUserAnAdmin");
if (NULL != ProcAdd)
{
iResult = (ProcAdd)() ;
}
FreeLibrary( hLib );
}
hb_retl( iResult ) ;
}

#pragma ENDDUMP


*--------------------------------------------------------*
#pragma BEGINDUMP

#define SM_MEDIACENTER          87

#include <windows.h>
#include "hbapiitm.h"

static void getwinver(  OSVERSIONINFO * pOSvi )
{
  pOSvi->dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
  GetVersionEx ( pOSvi );
}

HB_FUNC( ISWINNT )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT );
}

HB_FUNC( ISWIN9X )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS );
}

HB_FUNC( ISWIN95 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( ISWIN98 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10 );
}

HB_FUNC( ISWINME )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90 );
}

HB_FUNC( ISWINNT351 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT
        && osvi.dwMajorVersion == 3 && osvi.dwMinorVersion == 51 );
}

HB_FUNC( ISWINNT4 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( ISWIN2K )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( ISWINXP )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 );
}

HB_FUNC( ISWIN2003 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2 );
}

HB_FUNC( ISWINVISTA )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( ISWIN2KORLATER )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion >= 5 );
}

HB_FUNC( ISWINXPORLATER )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT
        && ((osvi.dwMajorVersion == 5 && osvi.dwMinorVersion > 0) ||
      (osvi.dwMajorVersion > 5)) );
}

HB_FUNC( ISVISTAORLATER )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion >= 6 );
}
/*
HB_FUNC( ISWIN8ORLATER )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion >= 6 && osvi.dwMinorVersion > 1 );
}
*/
HB_FUNC( GETWINVERSIONINFO )
{
  OSVERSIONINFO osvi;
  PHB_ITEM pArray = hb_itemArrayNew( 5 );
  getwinver( &osvi );
  hb_itemPutNL( hb_arrayGetItemPtr( pArray, 1 ), osvi.dwMajorVersion );
  hb_itemPutNL( hb_arrayGetItemPtr( pArray, 2 ), osvi.dwMinorVersion );
  if ( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
  {
    osvi.dwBuildNumber = LOWORD( osvi.dwBuildNumber );
  }
  hb_itemPutNL( hb_arrayGetItemPtr( pArray, 3 ), osvi.dwBuildNumber  );
  hb_itemPutNL( hb_arrayGetItemPtr( pArray, 4 ), osvi.dwPlatformId   );
  hb_itemPutC(  hb_arrayGetItemPtr( pArray, 5 ), osvi.szCSDVersion   );
  hb_itemRelease( hb_itemReturn( pArray) );
}

HB_FUNC( ISMEDIACENTER )
{
  if (GetSystemMetrics(SM_MEDIACENTER))
    hb_retl( TRUE );
  else
    hb_retl( FALSE );
}

#pragma ENDDUMP
