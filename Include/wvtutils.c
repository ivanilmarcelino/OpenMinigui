/*
 * Harbour Project source code:
 * Video subsystem for Win32 using GUI windows instead of Console
 *
 *    Copyright 2007 Pritpal Bedi <pritpal@vouchcac.com>
 * based on:
 *
 *    Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

//-------------------------------------------------------------------//



//-------------------------------------------------------------------//

//-------------------------------------------------------------------//

#ifndef _WIN32_IE
   #define _WIN32_IE 0x0400
#endif

#ifndef CINTERFACE
   #define CINTERFACE 1
#endif

#define NONAMELESSUNION

//-------------------------------------------------------------------//

#if defined( _MSC_VER )
   #pragma warning ( disable:4201 )
#endif

#include <windows.h>
#include <winuser.h>
#include <commctrl.h>
#include <ole2.h>
#include <oleauto.h>
#include <olectl.h>
#include <commdlg.h>
#if defined(__MINGW__)
#include <comctl32.h>
#endif

#if ! defined(__WATCOMC__)
  #include <shlobj.h>
#endif


#include <time.h>
#include <ctype.h>

#include "hbset.h"
#include "hbgtcore.h"
#include "hbapigt.h"
#include "hbinit.h"
#include "hbapicdp.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "inkey.ch"
#include "error.ch"
#include "hbvm.h"
#include "hbgfxdef.ch"

//-------------------------------------------------------------------//

HB_EXTERN_BEGIN

extern BOOL     wvt_Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM wvt_Rect2Array( RECT *rc  );
extern BOOL     wvt_Array2Point(PHB_ITEM aPoint, POINT *pt );
extern PHB_ITEM wvt_Point2Array( POINT *pt  );
extern BOOL     wvt_Array2Size(PHB_ITEM aSize, SIZE *siz );
extern PHB_ITEM wvt_Size2Array( SIZE *siz  );
extern void     wvt_Rect2ArrayEx( RECT *rc ,PHB_ITEM aRect );
extern void     wvt_Point2ArrayEx( POINT *pt  , PHB_ITEM aPoint);
extern void     wvt_Size2ArrayEx( SIZE *siz ,PHB_ITEM aSize );

HB_EXTERN_END

//----------------------------------------------------------------------//

#define HB_PARTSTR( n )  ( ISCHAR( n ) ? HB_TCHAR_CONVTO( hb_parc(n) ) : NULL )
#define HB_PARTFREE( p ) do { if( p ) HB_TCHAR_FREE( p ); } while( 0 )

HB_FUNC( WVT__GETOPENFILENAME )
{
   OPENFILENAME ofn;
   LPTSTR lpFileName, lpstrTitle, lpstrFilter, lpstrInitialDir, lpstrDefExt;
   int size = hb_parclen( 2 );

   size += size ? 1 : 1024;
   lpFileName = ( LPTSTR ) hb_xgrab( size * sizeof( TCHAR ) );
   HB_TCHAR_CPTO( lpFileName, hb_parcx( 2 ), size - 1 );
   lpstrTitle      = HB_PARTSTR( 3 );
   lpstrFilter     = HB_PARTSTR( 4 );
   lpstrInitialDir = HB_PARTSTR( 6 );
   lpstrDefExt     = HB_PARTSTR( 7 );

   ZeroMemory( &ofn, sizeof( ofn ) );

   ofn.hInstance        = GetModuleHandle( NULL )  ;
   ofn.lStructSize      = sizeof( ofn );
   ofn.hwndOwner        = ISNIL(1) ? GetActiveWindow() : (HWND) hb_parnl( 1 ) ;
   ofn.lpstrTitle       = lpstrTitle;
   ofn.lpstrFilter      = lpstrFilter;
   ofn.Flags            = ISNIL(5) ? OFN_SHOWHELP|OFN_NOCHANGEDIR : hb_parnl( 5 ) ;
   ofn.lpstrInitialDir  = lpstrInitialDir;
   ofn.lpstrDefExt      = lpstrDefExt;
   ofn.nFilterIndex     = ISNIL(8) ? 0 : (int) hb_parni( 8 );
   ofn.lpstrFile        = lpFileName;
   ofn.nMaxFile         = size;

   if( GetOpenFileName( &ofn ) )
   {
      char * szFileName = HB_TCHAR_CONVFROM( lpFileName );
      hb_stornl( ofn.nFilterIndex, 8 );
      hb_storclen( szFileName, size, 2 ) ;
      hb_retc( szFileName );
      HB_TCHAR_FREE( szFileName );
   }
   else
   {
      hb_retc( NULL );
   }
   hb_xfree( lpFileName );
   HB_PARTFREE( lpstrTitle );
   HB_PARTFREE( lpstrFilter );
   HB_PARTFREE( lpstrInitialDir );
   HB_PARTFREE( lpstrDefExt );
}

//----------------------------------------------------------------------//

HB_FUNC( WVT__GETSAVEFILENAME )
{
   OPENFILENAME ofn;
   LPTSTR lpstrTitle, lpstrFilter, lpstrInitialDir, lpstrDefExt;
   TCHAR lpFileName[MAX_PATH + 1];

   HB_TCHAR_CPTO( lpFileName, hb_parcx( 2 ), MAX_PATH );
   lpstrTitle      = HB_PARTSTR( 3 );
   lpstrFilter     = HB_PARTSTR( 4 );
   lpstrInitialDir = HB_PARTSTR( 6 );
   lpstrDefExt     = HB_PARTSTR( 7 );

   ZeroMemory( &ofn, sizeof( ofn ) );

   ofn.hInstance       = GetModuleHandle( NULL );
   ofn.lStructSize     = sizeof( ofn );
   ofn.hwndOwner       = ISNIL   (1) ? GetActiveWindow() : (HWND) hb_parnl( 1 );
   ofn.lpstrTitle      = lpstrTitle;
   ofn.lpstrFilter     = lpstrFilter;
   ofn.Flags           = (ISNIL  (5) ? OFN_FILEMUSTEXIST|OFN_EXPLORER|OFN_NOCHANGEDIR : hb_parnl( 5 ) );
   ofn.lpstrInitialDir = lpstrInitialDir;
   ofn.lpstrDefExt     = lpstrDefExt;
   ofn.nFilterIndex    = hb_parni(8);
   ofn.lpstrFile       = lpFileName;
   ofn.nMaxFile        = MAX_PATH;

   if( GetSaveFileName( &ofn ) )
   {
      char * szFileName = HB_TCHAR_CONVFROM( lpFileName );
      hb_stornl( ofn.nFilterIndex, 8 );
      hb_retc( szFileName );
      HB_TCHAR_FREE( szFileName );
   }
   else
   {
      hb_retc( NULL );
   }

   HB_PARTFREE( lpstrTitle );
   HB_PARTFREE( lpstrFilter );
   HB_PARTFREE( lpstrInitialDir );
   HB_PARTFREE( lpstrDefExt );
}
//----------------------------------------------------------------------//

HB_FUNC( WIN_AND )
{
   hb_retnl( hb_parnl(1) & hb_parnl(2) ) ;
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_OR )
{
   hb_retnl( hb_parnl(1) | hb_parnl(2) ) ;
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_NOT )
{
   hb_retnl( ~( hb_parnl(1) ) ) ;
}

//----------------------------------------------------------------------//
