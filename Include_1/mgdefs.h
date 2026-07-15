/*
 * Harbour MiniGUI Project source code:
 * The definitions for minigui C-level code.
 *
 * Copyright 2015-2024 Grigory Filatov <gfilatov@gmail.com>
 * www - https://www.hmgextended.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 */

#ifndef _MG_SETUP_H_
#define _MG_SETUP_H_

#ifndef WINVER
#if defined( __WIN98__ )
  #define WINVER   0x0400        /* version 4.0 */
#else
  #define WINVER   0x0501        /* version 5.0 */
#endif
#endif /* !WINVER */

#ifndef _WIN32_WINNT
  #define _WIN32_WINNT   WINVER  /* XP = 0x0501, Vista = 0x0600 */
#endif /* !_WIN32_WINNT */

#ifndef _WIN32_IE
  #define _WIN32_IE 0x0501
#endif /* !_WIN32_IE */

#include "SET_COMPILE_HMG_UNICODE.ch"

#if defined( UNICODE ) && !defined( _UNICODE )
  #define _UNICODE
#endif /* UNICODE && !_UNICODE */

#include "hbapi.h"

#ifndef NO_LEAN_AND_MEAN
  #define WIN32_LEAN_AND_MEAN
#endif /* !NO_LEAN_AND_MEAN */

#include <windows.h>
#include <tchar.h>

#ifndef NO_LEAN_AND_MEAN
  #undef  WIN32_LEAN_AND_MEAN
#endif /* !NO_LEAN_AND_MEAN */

#ifndef HMG_LEGACY_ON
//#define HMG_LEGACY_OFF
#endif

#if defined( _WIN64 )
  #define HB_arraySetNL    hb_arraySetNLL
  #define HB_arrayGetNL    hb_arrayGetNLL
#ifdef __XHARBOUR__
  #define HB_PARNI         hb_parni
  #define HB_PARNL         hb_parnll
  #define HB_PARVNL        hb_parnll
  #define HB_RETNL         hb_retnll
  #define HB_STORC         hb_storc
  #define HB_STORNI        hb_storni
  #define HB_STORNL        hb_stornll
  #define HB_STORVNL       hb_stornll
  #define HB_STORL         hb_storl
  #define HB_STORDL        hb_stordl
#else
  #define HB_PARNI         hb_parvni
  #define HB_PARNL         hb_parnll
  #define HB_PARVNL        hb_parvnll
  #define HB_RETNL         hb_retnll
  #define HB_STORC         hb_storvc
  #define HB_STORNI        hb_storvni
  #define HB_STORNL        hb_stornll
  #define HB_STORVNL       hb_storvnll
  #define HB_STORL         hb_storvl
  #define HB_STORDL        hb_storvdl
#endif /* __XHARBOUR__ */
#else
  #define HB_arraySetNL    hb_arraySetNL
  #define HB_arrayGetNL    hb_arrayGetNL
  #define HB_PARNL         hb_parnl
  #define HB_RETNL         hb_retnl
  #define HB_STORNL        hb_stornl
#if !( defined( __XHARBOUR__ ) || defined( __XCC__ ) )
  #define HB_PARNI         hb_parvni
  #define HB_PARVNL        hb_parvnl
  #define HB_STORC         hb_storvc
  #define HB_STORNI        hb_storvni
  #define HB_STORVNL       hb_storvnl
  #define HB_STORL         hb_storvl
  #define HB_STORDL        hb_storvdl
#else
  #define HB_PARNI         hb_parni
  #define HB_PARVNL        hb_parnl
  #define HB_STORC         hb_storc
  #define HB_STORNI        hb_storni
  #define HB_STORVNL       hb_stornl
  #define HB_STORL         hb_storl
  #define HB_STORDL        hb_stordl
#endif /* !( __XHARBOUR__ || __XCC__ ) */
#endif /* _WIN64 */

/* Harbour macro\functions mapped to xHarbour ones */
#ifdef __XHARBOUR__
#include "hbverbld.h"

#if defined( HB_VER_CVSID ) && ( HB_VER_CVSID < 9639 )
  #define HB_ISCHAR        ISCHAR
  #define HB_ISNUM         ISNUM
  #define HB_ISBYREF       ISBYREF
#endif

#if defined( HB_VER_CVSID ) && ( HB_VER_CVSID < 9798 )
  #define HB_ISNIL         ISNIL
#endif

#if defined( HB_VER_CVSID ) && ( HB_VER_CVSID < 9820 )
  #define HB_ISLOG         ISLOG
  #define HB_ISARRAY       ISARRAY
#endif

#define HB_ISDATE          ISDATE

#define hb_parldef( l1, l2 )        ( ISLOG( l1 ) ? hb_parl( l1 )    : l2 )
#define hb_parnidef( n1, n2 )       ( ISNUM( n1 ) ? hb_parni( n1 )   : n2 )
#define hb_parnldef( n1, n2 )       ( ISNUM( n1 ) ? hb_parnl( n1 )   : n2 )
#define hb_parnintdef( n1, n2 )     ( ISNUM( n1 ) ? hb_parnint( n1 ) : n2 )
#endif /* __XHARBOUR__ */

#if defined( UNICODE )
  #define _isValidCtrlClass  _isValidCtrlClassW
#else
  #define _isValidCtrlClass  _isValidCtrlClassA
#endif /* UNICODE */

#if defined( __BORLANDC__ ) && ! defined( HB_ARCH_64BIT )
  #undef MAKELONG
  #define MAKELONG( a, b )  ( ( LONG ) ( ( ( WORD ) ( ( DWORD_PTR ) ( a ) & 0xffff ) ) | \
                                         ( ( ( DWORD ) ( ( WORD ) ( ( DWORD_PTR ) ( b ) & 0xffff ) ) ) << 16 ) ) )
#endif /* __BORLANDC__ && !HB_ARCH_64BIT */

/*****************************************************************************************
 *  MACRO DEFINITION FOR CALL DLL FUNCTION
 *****************************************************************************************/
extern HB_PTRUINT wapi_GetProcAddress( HMODULE hmodule, LPCSTR lpProcName );

#define HMG_DEFINE_DLL_FUNC( \
      _FUNC_NAME,             \
      _DLL_LIBNAME,           \
      _DLL_FUNC_RET,          \
      _DLL_FUNC_TYPE,         \
      _DLL_FUNC_NAMESTRINGAW, \
      _DLL_FUNC_PARAM,        \
      _DLL_FUNC_CALLPARAM,    \
      _DLL_FUNC_RETFAILCALL   \
      ) \
\
   _DLL_FUNC_RET _DLL_FUNC_TYPE _FUNC_NAME _DLL_FUNC_PARAM \
   { \
      typedef _DLL_FUNC_RET ( _DLL_FUNC_TYPE * PFUNC ) _DLL_FUNC_PARAM; \
      static PFUNC pfunc = NULL; \
      if( pfunc == NULL ) \
      { \
         HMODULE hLib = LoadLibrary( _DLL_LIBNAME ); \
         pfunc = ( PFUNC ) wapi_GetProcAddress( hLib, _DLL_FUNC_NAMESTRINGAW ); \
      } \
      if( pfunc == NULL ) \
         return ( ( _DLL_FUNC_RET ) _DLL_FUNC_RETFAILCALL ); \
      else \
         return pfunc _DLL_FUNC_CALLPARAM; \
   }

/****** USER Macro APIs ******************************************************/

#define SubclassWindow1( hwnd, lpfn )       \
   ( ( WNDPROC ) SetWindowLongPtr( ( hwnd ), GWLP_WNDPROC, ( LPARAM ) ( WNDPROC ) ( lpfn ) ) )

#define SubclassWindow2( hwnd, lpfn )       \
   SetWindowLongPtr( ( hwnd ), GWLP_WNDPROC, ( LPARAM ) ( WNDPROC ) ( lpfn ) )

// macros for parameters

#define hmg_par_WPARAM( n )              ( ( WPARAM     ) hb_parnint( n ) )
#define hmg_par_LPARAM( n )              ( ( LPARAM     ) hb_parnint( n ) )
#define hmg_par_LRESULT( n )             ( ( LRESULT    ) hb_parnint( n ) )

#define hmg_par_raw_LPARAM( n )          HB_PARNL( n )

#define hmg_par_raw_HWND( n )            ( ( HWND       ) HB_PARNL( n ) )
#define hmg_par_raw_HDC( n )             ( ( HDC        ) HB_PARNL( n ) )
#define hmg_par_raw_HANDLE( n )          ( ( HANDLE     ) HB_PARNL( n ) )
#define hmg_par_raw_HGDIOBJ( n )         ( ( HGDIOBJ    ) HB_PARNL( n ) )
#define hmg_par_raw_HBRUSH( n )          ( ( HBRUSH     ) HB_PARNL( n ) )
#define hmg_par_raw_HBITMAP( n )         ( ( HBITMAP    ) HB_PARNL( n ) )
#define hmg_par_raw_HCURSOR( n )         ( ( HCURSOR    ) HB_PARNL( n ) )
#define hmg_par_raw_HDROP( n )           ( ( HDROP      ) HB_PARNL( n ) )
#define hmg_par_raw_HICON( n )           ( ( HICON      ) HB_PARNL( n ) )
#define hmg_par_raw_HPEN( n )            ( ( HPEN       ) HB_PARNL( n ) )
#define hmg_par_raw_HRGN( n )            ( ( HRGN       ) HB_PARNL( n ) )
#define hmg_par_raw_HMENU( n )           ( ( HMENU      ) HB_PARNL( n ) )
#define hmg_par_raw_HACCEL( n )          ( ( HACCEL     ) hb_parptr( n ) )
#define hmg_par_raw_HIMAGELIST( n )      ( ( HIMAGELIST ) HB_PARNL( n ) )
#define hmg_par_raw_HFONT( n )           ( ( HFONT      ) HB_PARNL( n ) )
#define hmg_par_raw_HINSTANCE( n )       ( ( HINSTANCE  ) HB_PARNL( n ) )
#define hmg_par_raw_HMONITOR( n )        ( ( HMONITOR   ) HB_PARNL( n ) )
#define hmg_par_raw_HTHEME( n )          ( ( HTHEME     ) HB_PARNL( n ) )
#define hmg_par_raw_TREEITEM( n )        ( ( HTREEITEM  ) HB_PARNL( n ) )
#define hmg_parv_raw_TREEITEM( n, i )    ( ( HTREEITEM  ) HB_PARVNL( n, i ) )
#define hmg_par_raw_LONG_PTR( n )        ( ( LONG_PTR   ) HB_PARNL( n ) )

#define hmg_par_raw_DITEMSTRUCT( n )     ( ( DRAWITEMSTRUCT *    ) HB_PARNL( n ) )
#define hmg_par_raw_MITEMSTRUCT( n )     ( ( LPMEASUREITEMSTRUCT ) HB_PARNL( n ) )

#define hmg_par_COLORREF( n )            ( ( COLORREF ) hb_parnl( n ) )
#define hmg_parv_COLORREF( n, i )        ( ( COLORREF ) HB_PARVNL( n, i ) )

#define hmg_par_BOOL( n )                ( ( BOOL  ) ( hb_parl( n ) ? TRUE : FALSE ) )
#define hmg_par_BYTE( n )                ( ( BYTE  ) hb_parni( n ) )
#define hmg_par_INT( n )                 ( ( INT   ) hb_parni( n ) )
#define hmg_par_UINT( n )                ( ( UINT  ) hb_parni( n ) )
#define hmg_par_UINT_def( n, d )         ( ( UINT  ) hb_parnidef( n, d ) )
#define hmg_par_LONG( n )                ( ( LONG  ) hb_parnl( n ) )
#ifdef __XHARBOUR__
#define hmg_parv_LONG( n, i )            ( ( LONG  ) hb_parnl( n, i ) )
#else
#define hmg_parv_LONG( n, i )            ( ( LONG  ) hb_parvnl( n, i ) )
#endif /* __XHARBOUR__ */
#define hmg_par_WORD( n )                ( ( WORD  ) hb_parnl( n ) )
#define hmg_par_WORD_def( n, d )         ( ( WORD  ) hb_parnldef( n, d ) )
#define hmg_par_DWORD( n )               ( ( DWORD ) hb_parnl( n ) )
#define hmg_par_short( n )               ( ( short ) hb_parni( n ) )

// macros for return values

#define hmg_ret_NINT( i )                hb_retnint( i )
#define hmg_ret_NI( i )                  hb_retni( i )
#define hmg_ret_L( b )                   hb_retl( b ? HB_TRUE : HB_FALSE )
#define hmg_ret_UINT( n )                hb_retnint( n )
#define hmg_ret_WORD( n )                hb_retni( n )
#define hmg_ret_DWORD( n )               hb_retnint( n )
#define hmg_ret_LONG( n )                hb_retnl( n )

#define hmg_ret_raw_HANDLE( h )          HB_RETNL( ( HB_PTRUINT ) h )
#define hmg_ret_raw_HWND( h )            HB_RETNL( ( HB_PTRUINT ) h )
#define hmg_ret_raw_HDC( h )             HB_RETNL( ( HB_PTRUINT ) h )
#define hmg_ret_raw_HMENU( h )           HB_RETNL( ( HB_PTRUINT ) h )
#define hmg_ret_raw_HACCEL( h )          hb_retptr( ( void * ) ( h ) )
#define hmg_ret_raw_HBRUSH( h )          HB_RETNL( ( HB_PTRUINT ) h )
#define hmg_ret_raw_HGDIOBJ( h )         HB_RETNL( ( HB_PTRUINT ) h )
#define hmg_ret_LONG_PTR( h )            HB_RETNL( ( LONG_PTR ) h )

#define hmg_ret_HRESULT( hr )            hb_retnint( hr )
#define hmg_ret_LRESULT( hr )            hb_retnint( hr )
#define hmg_ret_COLORREF( cr )           hb_retnint( ( COLORREF ) cr )

#define hmg_storvnl_HANDLE( h, v, n )    HB_STORVNL( ( HB_PTRUINT ) h, v, n )

#endif /* _MG_SETUP_H_ */
