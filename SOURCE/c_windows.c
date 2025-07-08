/*
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This    program  is  free  software;  you can redistribute it and/or modify
   it under  the  terms  of the GNU General Public License as published by the
   Free  Software   Foundation;  either  version 2 of the License, or (at your
   option) any later version.

   This   program   is   distributed  in  the hope that it will be useful, but
   WITHOUT    ANY    WARRANTY;    without   even   the   implied  warranty  of
   MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You   should  have  received a copy of the GNU General Public License along
   with   this   software;   see  the  file COPYING. If not, write to the Free
   Software   Foundation,   Inc.,   59  Temple  Place,  Suite  330, Boston, MA
   02111-1307 USA (or visit the web site http://www.gnu.org/).

   As   a   special  exception, you have permission for additional uses of the
   text  contained  in  this  release  of  Harbour Minigui.

   The   exception   is that,   if   you  link  the  Harbour  Minigui  library
   with  other    files   to  produce   an   executable,   this  does  not  by
   itself   cause  the   resulting   executable    to   be  covered by the GNU
   General  Public  License.  Your    use  of that   executable   is   in   no
   way  restricted on account of linking the Harbour-Minigui library code into
   it.

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

   Parts  of  this  code  is contributed and used here under permission of his
   author: Copyright 2016-2017 (C) P.Chornyj <myorg63@mail.ru>
*/
#define _WIN32_IE 0x0501

#if defined( __MINGW32__ ) || defined( __XCC__ ) || defined( __POCC__ )
#define _WIN32_WINNT 0x0500
#endif /* MINGW | XCC | POCC */

#include <mgdefs.h>

#include <commctrl.h>

#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )

// Static Class Name
#define WC_STATIC "Static"
#endif
#include "hbapiitm.h"
#include "hbvm.h"

#ifdef __XHARBOUR__
#include "thread.h"
#else
#include "hbwinuni.h"
#include "hbthread.h"
#endif /* __XHARBOUR__ */

#include "hbatomic.h"

#define DEFAULT_LISTENER   "EVENTS"
#define MAX_EVENTS         64

#if ( defined( __POCC__ ) && __POCC__ >= 900 )
#undef UNALIGNED
#define UNALIGNED
#endif /* __POCC__ */

// local types
typedef struct tagAppEvent
{
   UINT     message;
   PHB_ITEM bAction;
   BOOL     active;
} APPEVENT, *APPEVENT_PTR;

typedef struct tagEventsHolder
{
   HWND        hwnd;
   BOOL        active;
   size_t      count;
   HB_COUNTER  used;
   APPEVENT    events[MAX_EVENTS];
} EVENTSHOLDER, *EVENTSHOLDER_PTR;

typedef struct tagMyParam
{
   PHB_DYNS Listener;
} MYPARAMS;

typedef struct tagMyUserData
{
   UINT     cbSize;
   MYPARAMS myParam;

#if defined( _WIN64 )
} MYUSERDATA, *PMYUSERDATA;
#else
} MYUSERDATA, UNALIGNED * PMYUSERDATA;
#endif /* _WIN64 */

typedef struct tagWinEvent
{
   UINT     message;
   PHB_ITEM bBefore;
   PHB_ITEM bAction;
   PHB_ITEM bAfter;
   BOOL     active;
} WINEVENT, *WINEVENT_PTR;

typedef struct tagWinEventsHolder
{
   HWND        hwnd;
   BOOL        active;
   size_t      count;
   HB_COUNTER  used;
   WINEVENT    events[MAX_EVENTS];
} WINEVENTSHOLDER, *WINEVENTSHOLDER_PTR;

// extern functions
#ifdef UNICODE
LPWSTR AnsiToWide( LPCSTR );
LPSTR WideToAnsi( LPWSTR );
#endif
HINSTANCE GetInstance( void );
HINSTANCE GetResources( void );
extern void hmg_ErrorExit( LPCTSTR lpMessage, DWORD dwError, BOOL bExit );
extern HBITMAP HMG_LoadImage( const char *FileName );

// local functions
static size_t AppEventScan( EVENTSHOLDER *events, UINT message );
static LRESULT AppEventDo( EVENTSHOLDER *events, HB_BOOL bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static LRESULT AppEventOn( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static HB_BOOL AppEventRemove( HWND hWnd, const char *pszName, UINT message );
static size_t WinEventScan( WINEVENTSHOLDER *events, UINT message );
static LRESULT WinEventDo( WINEVENTSHOLDER *events, HB_BOOL bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static LRESULT WinEventOn( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static HB_BOOL WinEventRemove( HWND hWnd, const char *pszName, UINT message );

LRESULT CALLBACK MsgOnlyWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
LRESULT CALLBACK WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

// extern variables
extern HWND g_hWndMain;
extern HACCEL g_hAccel;

// global variable
PHB_DYNS g_ListenerDyns = NULL;

#ifdef __XHARBOUR__
#ifdef HB_THREAD_SUPPORT
static HB_CRITICAL_T s_lst_mtx;
#define HMG_LISTENER_LOCK              HB_CRITICAL_LOCK( s_lst_mtx );
#define HMG_LISTENER_UNLOCK            HB_CRITICAL_UNLOCK( s_lst_mtx );
#define HMG_LISTENER_INIT( s_lst_mtx ) HB_CRITICAL_INIT( s_lst_mtx );
#else
#define HMG_LISTENER_LOCK
#define HMG_LISTENER_UNLOCK
#define HMG_LISTENER_INIT( s_lst_mtx )
#endif /* HB_THREAD_SUPPORT */
#else
static HB_CRITICAL_NEW( s_lst_mtx );
#define HMG_LISTENER_LOCK     hb_threadEnterCriticalSection( &s_lst_mtx )
#define HMG_LISTENER_UNLOCK   hb_threadLeaveCriticalSection( &s_lst_mtx )
#endif \
 \
   /* __XHARBOUR__ */
HB_FUNC( GETGLOBALLISTENER )
{
   if( NULL != g_ListenerDyns )
   {
      hb_retc( hb_dynsymName( g_ListenerDyns ) );
   }
   else
   {
      hb_retc_null();
   }
}

HB_FUNC( SETGLOBALLISTENER )
{
   const char  *pszNewName = hb_parc( 1 );

   if( pszNewName && hb_dynsymIsFunction( hb_dynsymGet( pszNewName ) ) )
   {
      HMG_LISTENER_LOCK;
      g_ListenerDyns = hb_dynsymGet( pszNewName );
      hb_retl( HB_TRUE );
      HMG_LISTENER_UNLOCK;
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

HB_FUNC( RESETGLOBALLISTENER )
{
   HMG_LISTENER_LOCK;
   g_ListenerDyns = hb_dynsymGet( DEFAULT_LISTENER );
   HMG_LISTENER_UNLOCK;
}

static size_t AppEventScan( EVENTSHOLDER *events, UINT message )
{
   size_t   i, nPos = 0;

   for( i = 0; i < events->count; i++ )
   {
      if( message == events->events[i].message )
      {
         nPos = ( i + 1 );
         break;
      }
   }

   return nPos;
}

static HB_BOOL AppEventRemove( HWND hWnd, const char *pszProp, UINT message )
{
   if( IsWindow( hWnd ) )
   {
#ifdef UNICODE
      LPWSTR         pW = AnsiToWide( pszProp );
      EVENTSHOLDER   *events = ( EVENTSHOLDER * ) GetProp( hWnd, pW );
#else
      EVENTSHOLDER   *events = ( EVENTSHOLDER * ) GetProp( hWnd, pszProp );
#endif
      if( events != NULL )
      {
         if( message != 0 )
         {
            size_t   nPos = AppEventScan( events, message );

            if( nPos > 0 ) // if found
            {
               hb_itemRelease( events->events[nPos - 1].bAction );   // delete old codeblock
               events->events[nPos - 1].message = 0;
               events->events[nPos - 1].bAction = NULL;
               events->events[nPos - 1].active = FALSE;

               HB_ATOM_DEC( &events->used );
            }
         }
         else
         {
            size_t   i;

            for( i = 0; i < events->count; i++ )
            {
               // delete all not empty items with codeblocks
               if( events->events[i].bAction != NULL && HB_IS_BLOCK( events->events[i].bAction ) )
               {
                  hb_itemRelease( events->events[i].bAction );
               }
            }

            HB_ATOM_SET( &events->used, 0 );
         }

         if( !HB_ATOM_GET( &events->used ) )
         {
#ifdef UNICODE
            events = ( EVENTSHOLDER * ) RemoveProp( hWnd, pW );
#else
            events = ( EVENTSHOLDER * ) RemoveProp( hWnd, pszProp );
#endif
            hb_xfree( events );                 // delete events holder
         }

#ifdef UNICODE
         hb_xfree( pW );
#endif
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

static LRESULT AppEventDo( EVENTSHOLDER *events, HB_BOOL bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   size_t   nPos = AppEventScan( events, message );

   if
   (
      ( nPos > 0 )
   && events->active
   && ( events->events[nPos - 1].active && ( ( events->events[nPos - 1].bAction != NULL ) && HB_IS_BLOCK( events->events[nPos - 1].bAction ) ) )
   )
   {
      PHB_ITEM phWnd = hb_itemPutNInt( NULL, ( HB_PTRUINT ) hWnd );
      PHB_ITEM pmessage = hb_itemPutNS( NULL, message );
      PHB_ITEM pwParam = hb_itemPutNInt( NULL, ( HB_PTRUINT ) wParam );
      PHB_ITEM plParam = hb_itemPutNInt( NULL, ( HB_PTRUINT ) lParam );

      hb_evalBlock( events->events[nPos - 1].bAction, phWnd, pmessage, pwParam, plParam, NULL );

      hb_itemRelease( phWnd );
      hb_itemRelease( pmessage );
      hb_itemRelease( pwParam );
      hb_itemRelease( plParam );

      if( HB_TRUE == bOnce )
      {
         AppEventRemove( hWnd, "ONCE", message );
      }

      return hmg_par_LRESULT( -1 );
   }

   return( LRESULT ) 0;
}

static LRESULT AppEventOn( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   LRESULT  r = 0;

   if( IsWindow( hWnd ) )
   {
      EVENTSHOLDER   *events = ( EVENTSHOLDER * ) GetProp( hWnd, TEXT( "ONCE" ) );

      if( NULL != events )
      {
         if( hWnd == events->hwnd )
         {
            r = AppEventDo( events, HB_TRUE, hWnd, message, wParam, lParam );
         }
      }

      events = ( EVENTSHOLDER * ) GetProp( hWnd, TEXT( "ON" ) );

      if( NULL != events )
      {
         if( hWnd == events->hwnd )
         {
            r = AppEventDo( events, HB_FALSE, hWnd, message, wParam, lParam );
         }
      }
   }

   return r;
}

HB_FUNC( APPEVENTS )
{
   BOOL  bRes = FALSE;
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   UINT  message = ( UINT ) hb_parns( 2 );

   if( IsWindow( hWnd ) && ( message >= WM_APP && message <= ( WM_APP + MAX_EVENTS ) ) )
   {
      BOOL           bInit = FALSE;
      const char     *pszProp = hb_parldef( 5, HB_TRUE ) ? "ONCE" : "ON";
#ifdef UNICODE
      LPWSTR         pW = AnsiToWide( pszProp );
      EVENTSHOLDER   *events = ( EVENTSHOLDER * ) GetProp( hWnd, pW );
#else
      EVENTSHOLDER   *events = ( EVENTSHOLDER * ) GetProp( hWnd, pszProp );
#endif
      size_t         nPos;

      if( events == NULL )
      {
         events = ( EVENTSHOLDER * ) hb_xgrabz( sizeof( EVENTSHOLDER ) );
         events->hwnd = hWnd;
         events->active = hb_parldef( 4, HB_TRUE );
         events->count = ( size_t ) sizeof( events->events ) / sizeof( APPEVENT );

         HB_ATOM_SET( &events->used, 0 );

         bInit = TRUE;
      }

      nPos = AppEventScan( events, message );   // arleady exists ?
      if( nPos > 0 )
      {
         hb_itemRelease( events->events[nPos - 1].bAction );
      }
      else
      {
         nPos = bInit ? 1 : AppEventScan( events, 0 );
         if( nPos > 0 )
         {
            HB_ATOM_INC( &events->used );
         }
      }

      if( nPos > 0 )
      {
         events->events[nPos - 1].message = message;
         events->events[nPos - 1].bAction = hb_itemNew( hb_param( 3, HB_IT_BLOCK ) );
         events->events[nPos - 1].active = hb_parldef( 4, HB_TRUE );

         bRes = TRUE;
      }

      if( bInit )
      {
#ifdef UNICODE
         bRes = SetProp( hWnd, pW, ( HANDLE ) events ) ? HB_TRUE : HB_FALSE;
#else
         bRes = SetProp( hWnd, pszProp, ( HANDLE ) events ) ? HB_TRUE : HB_FALSE;
#endif
      }

#ifdef UNICODE
      hb_xfree( pW );
#endif
   }

   hmg_ret_L( bRes );
}

HB_FUNC( APPEVENTSREMOVE )
{
   HB_BOOL  bDel = HB_FALSE;
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   UINT     message = ( UINT ) hb_parns( 2 );

   if( IsWindow( hWnd ) )
   {
      const char  *pszProp = hb_parldef( 3, HB_TRUE ) ? "ONCE" : "ON";

      bDel = AppEventRemove( hWnd, pszProp, message );
   }

   hb_retl( bDel );
}

HB_FUNC( APPEVENTSUPDATE )
{
   HB_BOOL  bUpd = HB_FALSE;
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   UINT     message = ( UINT ) hb_parns( 2 );

   if( IsWindow( hWnd ) )
   {
      const char     *pszProp = hb_parldef( 5, HB_TRUE ) ? "ONCE" : "ON";
#ifdef UNICODE
      LPWSTR         pW = AnsiToWide( pszProp );
      EVENTSHOLDER   *events = ( EVENTSHOLDER * ) GetProp( hWnd, pW );
#else
      EVENTSHOLDER   *events = ( EVENTSHOLDER * ) GetProp( hWnd, pszProp );
#endif
      if( events != NULL )
      {
         if( message >= WM_APP && message <= ( WM_APP + MAX_EVENTS ) )
         {
            size_t   nPos = AppEventScan( events, message );   // arleady exists ?
            if( nPos > 0 )
            {
               if( HB_IS_BLOCK( hb_param( 3, HB_IT_ANY ) ) )
               {
                  hb_itemRelease( events->events[nPos - 1].bAction );
                  events->events[nPos - 1].bAction = hb_itemNew( hb_param( 3, HB_IT_BLOCK ) );
               }

               events->events[nPos - 1].active = hb_parldef( 4, HB_TRUE );

               bUpd = HB_TRUE;
            }
         }
         else if( message == 0 )
         {
            events->active = hb_parldef( 4, events->active );

            bUpd = HB_TRUE;
         }
      }

#ifdef UNICODE
      hb_xfree( pW );
#endif
   }

   hb_retl( bUpd );
}

HB_FUNC( ENUMAPPEVENTS )
{
   HWND        hWnd = hmg_par_raw_HWND( 1 );
   const char  *pszProp = hb_parldef( 2, HB_TRUE ) ? "ONCE" : "ON";
   PHB_ITEM    aEvents = hb_itemArrayNew( 0 );

   if( IsWindow( hWnd ) )
   {
#ifdef UNICODE
      LPWSTR         pW = AnsiToWide( pszProp );
      EVENTSHOLDER   *events = ( EVENTSHOLDER * ) GetProp( hWnd, pW );
#else
      EVENTSHOLDER   *events = ( EVENTSHOLDER * ) GetProp( hWnd, pszProp );
#endif
      if( events != NULL )
      {
         size_t   i;

         for( i = 0; i < events->count; i++ )
         {
            PHB_ITEM aEvent = hb_itemArrayNew( 3 );

            hb_arraySetNInt( aEvent, 1, events->events[i].message );
            hb_arraySetL( aEvent, 2, events->events[i].active );

            if( events->events[i].bAction != NULL && HB_IS_BLOCK( events->events[i].bAction ) )
            {
               hb_arraySet( aEvent, 3, hb_itemClone( events->events[i].bAction ) );
            }
            else
            {
               hb_arraySet( aEvent, 3, NULL );
            }

            hb_arrayAddForward( aEvents, aEvent );

            hb_itemRelease( aEvent );
         }
      }

#ifdef UNICODE
      hb_xfree( pW );
#endif
   }

   hb_itemReturnRelease( aEvents );
}

HB_FUNC( GETAPPEVENTSINFO )
{
   HWND        hWnd = hmg_par_raw_HWND( 1 );
   const char  *pszProp = hb_parldef( 2, HB_TRUE ) ? "ONCE" : "ON";
   PHB_ITEM    aInfo;

   if( IsWindow( hWnd ) )
   {
#ifdef UNICODE
      LPWSTR         pW = AnsiToWide( pszProp );
      EVENTSHOLDER   *events = ( EVENTSHOLDER * ) GetProp( hWnd, pW );
#else
      EVENTSHOLDER   *events = ( EVENTSHOLDER * ) GetProp( hWnd, pszProp );
#endif
      aInfo = hb_itemArrayNew( ( events != NULL ) ? 4 : 0 );

      if( events != NULL )
      {
         hb_arraySetNInt( aInfo, 1, ( HB_PTRUINT ) events->hwnd );
         hb_arraySetNS( aInfo, 2, events->count );
         hb_arraySetNS( aInfo, 3, ( HB_ISIZ ) HB_ATOM_GET( &events->used ) );
         hb_arraySetL( aInfo, 4, events->active );
      }

#ifdef UNICODE
      hb_xfree( pW );
#endif
   }
   else
   {
      aInfo = hb_itemArrayNew( 0 );
   }

   hb_itemReturnRelease( aInfo );
}

static size_t WinEventScan( WINEVENTSHOLDER *events, UINT message )
{
   size_t   i, nPos = 0;

   for( i = 0; i < events->count; i++ )
   {
      if( message == events->events[i].message )
      {
         nPos = ( i + 1 );
         break;
      }
   }

   return nPos;
}

static HB_BOOL WinEventRemove( HWND hWnd, const char *pszProp, UINT message )
{
   if( IsWindow( hWnd ) )
   {
#ifdef UNICODE
      LPWSTR            pW = AnsiToWide( pszProp );
      WINEVENTSHOLDER   *events = ( WINEVENTSHOLDER * ) GetProp( hWnd, pW );
#else
      WINEVENTSHOLDER   *events = ( WINEVENTSHOLDER * ) GetProp( hWnd, pszProp );
#endif
      if( events != NULL )
      {
         if( message != 0 )
         {
            size_t   nPos = WinEventScan( events, message );

            if( nPos > 0 ) // if found
            {
               hb_itemRelease( events->events[nPos - 1].bAction );   // delete old codeblock
               events->events[nPos - 1].message = 0;
               events->events[nPos - 1].bAction = NULL;
               events->events[nPos - 1].active = FALSE;

               HB_ATOM_DEC( &events->used );
            }
         }
         else
         {
            size_t   i;

            for( i = 0; i < events->count; i++ )
            {
               // delete all not empty items with codeblocks
               if( events->events[i].bAction != NULL && HB_IS_BLOCK( events->events[i].bAction ) )
               {
                  hb_itemRelease( events->events[i].bAction );
               }
            }

            HB_ATOM_SET( &events->used, 0 );
         }

         if( !HB_ATOM_GET( &events->used ) )
         {
#ifdef UNICODE
            events = ( WINEVENTSHOLDER * ) RemoveProp( hWnd, pW );
#else
            events = ( WINEVENTSHOLDER * ) RemoveProp( hWnd, pszProp );
#endif
            hb_xfree( events );                 // delete events holder
         }

#ifdef UNICODE
         hb_xfree( pW );
#endif
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

static LRESULT WinEventDo( WINEVENTSHOLDER *events, HB_BOOL bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   size_t   nPos = WinEventScan( events, message );

   if
   (
      ( nPos > 0 )
   && events->active
   && ( events->events[nPos - 1].active && ( ( events->events[nPos - 1].bAction != NULL ) && HB_IS_BLOCK( events->events[nPos - 1].bAction ) ) )
   )
   {
      PHB_ITEM phWnd = hb_itemPutNInt( NULL, ( HB_PTRUINT ) hWnd );
      PHB_ITEM pmessage = hb_itemPutNS( NULL, message );
      PHB_ITEM pwParam = hb_itemPutNInt( NULL, ( HB_PTRUINT ) wParam );
      PHB_ITEM plParam = hb_itemPutNInt( NULL, ( HB_PTRUINT ) lParam );

      hb_evalBlock( events->events[nPos - 1].bAction, phWnd, pmessage, pwParam, plParam, NULL );

      hb_itemRelease( phWnd );
      hb_itemRelease( pmessage );
      hb_itemRelease( pwParam );
      hb_itemRelease( plParam );

      if( HB_TRUE == bOnce )
      {
         WinEventRemove( hWnd, "ONCE", message );
      }

      return hmg_par_LRESULT( -1 );
   }

   return( LRESULT ) 0;
}

static LRESULT WinEventOn( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   LRESULT  r = 0;

   if( IsWindow( hWnd ) )
   {
      WINEVENTSHOLDER   *events = ( WINEVENTSHOLDER * ) GetProp( hWnd, TEXT( "ONCE" ) );

      if( NULL != events )
      {
         if( hWnd == events->hwnd )
         {
            r = WinEventDo( events, HB_TRUE, hWnd, message, wParam, lParam );
         }
      }

      events = ( WINEVENTSHOLDER * ) GetProp( hWnd, TEXT( "ON" ) );

      if( NULL != events )
      {
         if( hWnd == events->hwnd )
         {
            r = WinEventDo( events, HB_FALSE, hWnd, message, wParam, lParam );
         }
      }
   }

   return r;
}

HB_FUNC( WINEVENTS )
{
   BOOL  bRes = FALSE;
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   UINT  message = ( UINT ) hb_parns( 2 );

   if( IsWindow( hWnd ) && ( message <= ( WM_APP + MAX_EVENTS ) ) )
   {
      BOOL              bInit = FALSE;
      const char        *pszProp = hb_parldef( 5, HB_TRUE ) ? "ONCE" : "ON";
#ifdef UNICODE
      LPWSTR            pW = AnsiToWide( pszProp );
      WINEVENTSHOLDER   *events = ( WINEVENTSHOLDER * ) GetProp( hWnd, pW );
#else
      WINEVENTSHOLDER   *events = ( WINEVENTSHOLDER * ) GetProp( hWnd, pszProp );
#endif
      size_t            nPos;

      if( events == NULL )
      {
         events = ( WINEVENTSHOLDER * ) hb_xgrabz( sizeof( WINEVENTSHOLDER ) );
         events->hwnd = hWnd;
         events->active = hb_parldef( 4, HB_TRUE );
         events->count = ( size_t ) sizeof( events->events ) / sizeof( WINEVENT );

         HB_ATOM_SET( &events->used, 0 );

         bInit = TRUE;
      }

      nPos = WinEventScan( events, message );   // arleady exists ?
      if( nPos > 0 )
      {
         hb_itemRelease( events->events[nPos - 1].bAction );
      }
      else
      {
         nPos = bInit ? 1 : WinEventScan( events, 0 );
         if( nPos > 0 )
         {
            HB_ATOM_INC( &events->used );
         }
      }

      if( nPos > 0 )
      {
         events->events[nPos - 1].message = message;
         events->events[nPos - 1].bAction = hb_itemNew( hb_param( 3, HB_IT_BLOCK ) );
         events->events[nPos - 1].active = hb_parldef( 4, HB_TRUE );

         bRes = TRUE;
      }

      if( bInit )
      {
#ifdef UNICODE
         bRes = SetProp( hWnd, pW, ( HANDLE ) events ) ? HB_TRUE : HB_FALSE;
#else
         bRes = SetProp( hWnd, pszProp, ( HANDLE ) events ) ? HB_TRUE : HB_FALSE;
#endif
      }

#ifdef UNICODE
      hb_xfree( pW );
#endif
   }

   hmg_ret_L( bRes );
}

HB_FUNC( WINEVENTSREMOVE )
{
   HB_BOOL  bDel = HB_FALSE;
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   UINT     message = ( UINT ) hb_parns( 2 );

   if( IsWindow( hWnd ) )
   {
      const char  *pszProp = hb_parldef( 3, HB_TRUE ) ? "ONCE" : "ON";

      bDel = WinEventRemove( hWnd, pszProp, message );
   }

   hb_retl( bDel );
}

HB_FUNC( WINEVENTSUPDATE )
{
   HB_BOOL  bUpd = HB_FALSE;
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   UINT     message = ( UINT ) hb_parns( 2 );

   if( IsWindow( hWnd ) )
   {
      const char        *pszProp = hb_parldef( 5, HB_TRUE ) ? "ONCE" : "ON";
#ifdef UNICODE
      LPWSTR            pW = AnsiToWide( pszProp );
      WINEVENTSHOLDER   *events = ( WINEVENTSHOLDER * ) GetProp( hWnd, pW );
#else
      WINEVENTSHOLDER   *events = ( WINEVENTSHOLDER * ) GetProp( hWnd, pszProp );
#endif
      if( events != NULL )
      {
         if( message <= ( WM_APP + MAX_EVENTS ) )
         {
            size_t   nPos = WinEventScan( events, message );   // arleady exists ?
            if( nPos > 0 )
            {
               if( HB_IS_BLOCK( hb_param( 3, HB_IT_ANY ) ) )
               {
                  hb_itemRelease( events->events[nPos - 1].bAction );
                  events->events[nPos - 1].bAction = hb_itemNew( hb_param( 3, HB_IT_BLOCK ) );
               }

               events->events[nPos - 1].active = hb_parldef( 4, HB_TRUE );

               bUpd = HB_TRUE;
            }
         }
         else if( message == 0 )
         {
            events->active = hb_parldef( 4, events->active );

            bUpd = HB_TRUE;
         }
      }

#ifdef UNICODE
      hb_xfree( pW );
#endif
   }

   hb_retl( bUpd );
}

HB_FUNC( ENUMWINEVENTS )
{
   HWND        hWnd = hmg_par_raw_HWND( 1 );
   const char  *pszProp = hb_parldef( 2, HB_TRUE ) ? "ONCE" : "ON";
   PHB_ITEM    aEvents = hb_itemArrayNew( 0 );

   if( IsWindow( hWnd ) )
   {
#ifdef UNICODE
      LPWSTR            pW = AnsiToWide( pszProp );
      WINEVENTSHOLDER   *events = ( WINEVENTSHOLDER * ) GetProp( hWnd, pW );
#else
      WINEVENTSHOLDER   *events = ( WINEVENTSHOLDER * ) GetProp( hWnd, pszProp );
#endif
      if( events != NULL )
      {
         size_t   i;

         for( i = 0; i < events->count; i++ )
         {
            PHB_ITEM aEvent = hb_itemArrayNew( 3 );

            hb_arraySetNInt( aEvent, 1, events->events[i].message );
            hb_arraySetL( aEvent, 2, events->events[i].active );

            if( events->events[i].bAction != NULL && HB_IS_BLOCK( events->events[i].bAction ) )
            {
               hb_arraySet( aEvent, 3, hb_itemClone( events->events[i].bAction ) );
            }
            else
            {
               hb_arraySet( aEvent, 3, NULL );
            }

            hb_arrayAddForward( aEvents, aEvent );

            hb_itemRelease( aEvent );
         }
      }

#ifdef UNICODE
      hb_xfree( pW );
#endif
   }

   hb_itemReturnRelease( aEvents );
}

HB_FUNC( GETWINEVENTSINFO )
{
   HWND        hWnd = hmg_par_raw_HWND( 1 );
   const char  *pszProp = hb_parldef( 2, HB_TRUE ) ? "ONCE" : "ON";
   PHB_ITEM    aInfo;

   if( IsWindow( hWnd ) )
   {
#ifdef UNICODE
      LPWSTR            pW = AnsiToWide( pszProp );
      WINEVENTSHOLDER   *events = ( WINEVENTSHOLDER * ) GetProp( hWnd, pW );
#else
      WINEVENTSHOLDER   *events = ( WINEVENTSHOLDER * ) GetProp( hWnd, pszProp );
#endif
      aInfo = hb_itemArrayNew( ( events != NULL ) ? 4 : 0 );

      if( events != NULL )
      {
         hb_arraySetNInt( aInfo, 1, ( HB_PTRUINT ) events->hwnd );
         hb_arraySetNS( aInfo, 2, events->count );
         hb_arraySetNS( aInfo, 3, events->used );
         hb_arraySetL( aInfo, 4, events->active );
      }

#ifdef UNICODE
      hb_xfree( pW );
#endif
   }
   else
   {
      aInfo = hb_itemArrayNew( 0 );
   }

   hb_itemReturnRelease( aInfo );
}

LRESULT CALLBACK MsgOnlyWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   LONG_PTR lpUserData;
   LRESULT  result;

   if( message == WM_CREATE )
   {
      PMYUSERDATA pUserData = ( PMYUSERDATA ) ( ( ( LPCREATESTRUCT ) lParam )->lpCreateParams );

      if( pUserData )
      {
         SetLastError( 0 );

         SetWindowLongPtr( hWnd, GWLP_USERDATA, ( LONG_PTR ) pUserData );

         if( GetLastError() != 0 )
         {
            return -1;
         }
         else
         {
            SetWindowPos( hWnd, 0, 0, 0, 0, 0, SWP_NOZORDER | SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE );
         }
      }
   }
   else if( message == WM_NCDESTROY )
   {
      lpUserData = SetWindowLongPtr( hWnd, GWLP_USERDATA, ( LONG_PTR ) 0 );

      if( lpUserData )
      {
         PMYUSERDATA pUserData = ( PMYUSERDATA ) lpUserData;

         if( pUserData->cbSize == sizeof( MYUSERDATA ) )
         {
            hb_xfree( pUserData );
         }
      }
   }
   else if( message == WM_DESTROY )
   {
      WinEventRemove( hWnd, "ONCE", 0 );
      WinEventRemove( hWnd, "ON", 0 );
   }

   result = WinEventOn( hWnd, message, wParam, lParam );
   lpUserData = GetWindowLongPtr( hWnd, GWLP_USERDATA );

   if( lpUserData )
   {
      PMYUSERDATA pUserData = ( PMYUSERDATA ) lpUserData;
      PHB_DYNS    pListenerDyns = pUserData->myParam.Listener;
      PHB_SYMB    pListenerSymb = hb_dynsymSymbol( pListenerDyns );

      if( pListenerSymb )
      {
         if( hb_vmRequestReenter() )
         {
            hb_vmPushSymbol( pListenerSymb );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hWnd );
            hb_vmPushLong( message );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );

            result = hmg_par_LRESULT( -1 );

            hb_vmRequestRestore();
         }
      }
   }

   return( result != 0 ) ? result : DefWindowProc( hWnd, message, wParam, lParam );
}

HB_FUNC( INITMESSAGEONLYWINDOW )
{
   HWND        hwnd = NULL;

#ifndef __XHARBOUR__
   void        *hClassName;
   LPCTSTR     lpClassName = HB_PARSTR( 1, &hClassName, NULL );
#else
   const char  *lpClassName = hb_parc( 1 );
#endif
   if( lpClassName )
   {
      WNDCLASSEX  wcx = { 0 };

      wcx.cbSize = sizeof( wcx );
      wcx.lpfnWndProc = MsgOnlyWndProc;
      wcx.cbClsExtra = 0;              // no extra class memory
      wcx.cbWndExtra = 0;              // no extra window memory
      wcx.hInstance = GetInstance();
      wcx.lpszClassName = lpClassName;

      if( RegisterClassEx( &wcx ) )
      {
         const char  *pszFuncName = hb_parc( 2 );

         if( pszFuncName && hb_dynsymIsFunction( hb_dynsymGet( pszFuncName ) ) )
         {
            PMYUSERDATA pUserData = ( PMYUSERDATA ) hb_xgrabz( sizeof( MYUSERDATA ) );

            pUserData->cbSize = sizeof( MYUSERDATA );
            pUserData->myParam.Listener = hb_dynsymGet( pszFuncName );

            hwnd = CreateWindowEx( 0, lpClassName, 0, 0, 0, 0, 0, 0, HWND_MESSAGE, 0, GetInstance(), ( LPVOID ) pUserData );
         }
         else
         {
            hwnd = CreateWindowEx( 0, lpClassName, 0, 0, 0, 0, 0, 0, HWND_MESSAGE, 0, GetInstance(), 0 );
         }
      }
      else
      {
         hmg_ErrorExit( TEXT( "Window Registration Failed!" ), 0, TRUE );
      }
   }

#ifndef __XHARBOUR__
   hb_strfree( hClassName );
#endif
   hmg_ret_raw_HWND( hwnd );
}

/* Modified by P.Ch. 17.06. */
HB_FUNC( INITDUMMY )
{
   hmg_ret_raw_HWND( CreateWindowEx( 0, WC_STATIC, TEXT( "" ), WS_CHILD, 0, 0, 0, 0, hmg_par_raw_HWND( 1 ), ( HMENU ) NULL, GetInstance(), NULL ) );
}

/* Modified by P.Ch. 17.06. */
LRESULT CALLBACK WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   LRESULT  r = 0;
   PHB_SYMB g_ListenerSymb = hb_dynsymSymbol( g_ListenerDyns );

   if( message == WM_DESTROY )
   {
      AppEventRemove( hWnd, "ONCE", 0 );
      AppEventRemove( hWnd, "ON", 0 );

      if( IsWindow( g_hWndMain ) && hWnd == g_hWndMain && g_hAccel != NULL )
      {
         if( DestroyAcceleratorTable( g_hAccel ) )
         {
            g_hAccel = NULL;
         }
      }
   }

   if( message >= WM_APP && message <= ( WM_APP + MAX_EVENTS ) )
   {
      r = AppEventOn( hWnd, message, wParam, lParam );
   }
   else if( g_ListenerSymb )
   {
#ifndef __XHARBOUR__
      if( hb_vmRequestReenter() )
      {
#endif
         hb_vmPushSymbol( g_ListenerSymb );
         hb_vmPushNil();
         hb_vmPushNumInt( ( HB_PTRUINT ) hWnd );
         hb_vmPushLong( message );
         hb_vmPushNumInt( wParam );
         hb_vmPushNumInt( lParam );
         hb_vmDo( 4 );

         r = hmg_par_LRESULT( -1 );
#ifndef __XHARBOUR__
         hb_vmRequestRestore();
      }
#endif
   }

   return( r != 0 ) ? r : DefWindowProc( hWnd, message, wParam, lParam );
}

HB_FUNC( INITWINDOW )
{
   HWND     hwnd;
   DWORD    Style = WS_POPUP;
   DWORD    ExStyle = hb_parl( 16 ) ? WS_EX_CONTEXTHELP : 0;

#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 1 );
   LPCSTR   lpClassName = hb_parc( 12 );
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPWSTR   lpClassName = AnsiToWide( ( char * ) hb_parc( 12 ) );
#endif
   if( !ExStyle )
   {
      if( !hb_parl( 6 ) )
      {
         Style |= WS_MINIMIZEBOX;
      }

      if( !hb_parl( 7 ) )
      {
         Style |= WS_MAXIMIZEBOX;
      }
   }

   if( !hb_parl( 8 ) )
   {
      Style |= WS_SIZEBOX;
   }

   if( !hb_parl( 9 ) )
   {
      Style |= WS_SYSMENU;
   }

   if( !hb_parl( 10 ) )
   {
      Style |= WS_CAPTION;
   }

   if( hb_parl( 11 ) )
   {
      ExStyle |= WS_EX_TOPMOST;
   }

   if( hb_parl( 14 ) )
   {
      Style |= WS_VSCROLL;
   }

   if( hb_parl( 15 ) )
   {
      Style |= WS_HSCROLL;
   }

   if( hb_parl( 17 ) )
   {
      ExStyle |= WS_EX_PALETTEWINDOW;
   }

   if( hb_parl( 18 ) )                 // Panel
   {
      Style = WS_CHILD;
      ExStyle |= WS_EX_CONTROLPARENT | WS_EX_STATICEDGE;
   }

   hwnd = CreateWindowEx
      (
         ExStyle,
         lpClassName,
         lpWindowName,
         Style,
         hb_parni( 2 ),
         hb_parni( 3 ),
         hb_parni( 4 ),
         hb_parni( 5 ),
         hmg_par_raw_HWND( 13 ),
         ( HMENU ) NULL,
         GetInstance(),
         NULL
      );

   if( NULL != hwnd )
   {
      hmg_ret_raw_HWND( hwnd );
   }
   else
   {
      MessageBox( 0, TEXT( "Window Creation Failed!" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
   }

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpWindowName );
   hb_xfree( ( TCHAR * ) lpClassName );
#endif
}

HB_FUNC( INITMODALWINDOW )
{
   HWND     hwnd;
   DWORD    Style = WS_POPUP;
   DWORD    ExStyle = hb_parl( 13 ) ? WS_EX_CONTEXTHELP : 0;

#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 1 );
   LPCSTR   lpClassName = hb_parc( 10 );
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPWSTR   lpClassName = AnsiToWide( ( char * ) hb_parc( 10 ) );
#endif
   if( !hb_parl( 7 ) )
   {
      Style |= WS_SIZEBOX;
   }

   if( !hb_parl( 8 ) )
   {
      Style |= WS_SYSMENU;
   }

   if( !hb_parl( 9 ) )
   {
      Style |= WS_CAPTION;
   }

   if( hb_parl( 11 ) )
   {
      Style |= WS_VSCROLL;
   }

   if( hb_parl( 12 ) )
   {
      Style |= WS_HSCROLL;
   }

   hwnd = CreateWindowEx
      (
         ExStyle,
         lpClassName,
         lpWindowName,
         Style,
         hb_parni( 2 ),
         hb_parni( 3 ),
         hb_parni( 4 ),
         hb_parni( 5 ),
         hmg_par_raw_HWND( 6 ),
         ( HMENU ) NULL,
         GetInstance(),
         NULL
      );

   if( NULL != hwnd )
   {
      hmg_ret_raw_HWND( hwnd );
   }
   else
   {
      MessageBox( 0, TEXT( "Window Creation Failed!" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
   }

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpWindowName );
   hb_xfree( ( TCHAR * ) lpClassName );
#endif
}

HB_FUNC( INITSPLITCHILDWINDOW )
{
   HWND     hwnd;
   DWORD    Style = WS_POPUP;

#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 5 );
   LPCSTR   lpClassName = hb_parc( 3 );
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 5 ) );
   LPWSTR   lpClassName = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
   if( !hb_parl( 4 ) )
   {
      Style |= WS_CAPTION;
   }

   if( hb_parl( 7 ) )
   {
      Style |= WS_VSCROLL;
   }

   if( hb_parl( 8 ) )
   {
      Style |= WS_HSCROLL;
   }

   hwnd = CreateWindowEx
      (
         WS_EX_STATICEDGE | WS_EX_TOOLWINDOW,
         lpClassName,
         lpWindowName,
         Style,
         0,
         0,
         hb_parni( 1 ),
         hb_parni( 2 ),
         0,
         ( HMENU ) NULL,
         GetInstance(),
         NULL
      );

   if( NULL != hwnd )
   {
      hmg_ret_raw_HWND( hwnd );
   }
   else
   {
      MessageBox( 0, TEXT( "Window Creation Failed!" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
   }

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpWindowName );
   hb_xfree( ( TCHAR * ) lpClassName );
#endif
}

HB_FUNC( INITSPLITBOX )
{
   REBARINFO            rbi;
   HWND                 hwndRB;
   INITCOMMONCONTROLSEX icex;

   DWORD                Style = WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | RBS_BANDBORDERS | RBS_VARHEIGHT | RBS_FIXEDORDER;

   if( hb_parl( 2 ) )
   {
      Style |= CCS_BOTTOM;
   }

   if( hb_parl( 3 ) )
   {
      Style |= CCS_VERT;
   }

   icex.dwSize = sizeof( INITCOMMONCONTROLSEX );
   icex.dwICC = ICC_COOL_CLASSES | ICC_BAR_CLASSES;
   InitCommonControlsEx( &icex );

   hwndRB = CreateWindowEx( WS_EX_TOOLWINDOW | WS_EX_DLGMODALFRAME, REBARCLASSNAME, NULL, Style, 0, 0, 0, 0, hmg_par_raw_HWND( 1 ), NULL, GetInstance(), NULL );

   // Initialize and send the REBARINFO structure.
   rbi.cbSize = sizeof( REBARINFO );   // Required when using this struct.
   rbi.fMask = 0;
   rbi.himl = ( HIMAGELIST ) NULL;
   SendMessage( hwndRB, RB_SETBARINFO, 0, ( LPARAM ) &rbi );

   hmg_ret_raw_HWND( hwndRB );
}

/* Modified by P.Ch. 16.10.-16.12.,17.06. */
HB_FUNC( REGISTERWINDOW )
{
   WNDCLASS    WndClass;
   HBRUSH      hBrush = 0;
   HICON       hIcon;
   HCURSOR     hCursor;

#ifndef UNICODE
   LPCTSTR     lpIconName = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : ( HB_ISNUM( 1 ) ? MAKEINTRESOURCE( hmg_par_WORD( 1 ) ) : NULL );
#else
   LPWSTR      lpIconName = HB_ISCHAR( 1 ) ? AnsiToWide( ( char * ) hb_parc( 1 ) ) : ( HB_ISNUM( 1 ) ? ( LPWSTR ) MAKEINTRESOURCE( hmg_par_WORD( 1 ) ) : NULL );
#endif
#ifndef __XHARBOUR__
   void        *hClassName;
   LPCTSTR     lpClassName = HB_PARSTR( 2, &hClassName, NULL );
#else
   const char  *lpClassName = hb_parc( 2 );
#endif
#ifndef UNICODE
   LPCSTR      lpCursorName = HB_ISCHAR( 4 ) ? hb_parc( 4 ) : ( HB_ISNUM( 4 ) ? MAKEINTRESOURCE( hmg_par_WORD( 4 ) ) : NULL );
#else
   LPWSTR      lpCursorName = HB_ISCHAR( 4 ) ? AnsiToWide( ( char * ) hb_parc( 4 ) ) :
      ( HB_ISNUM( 4 ) ? ( LPWSTR ) MAKEINTRESOURCE( hmg_par_WORD( 4 ) ) : NULL );
#endif
   WndClass.style = CS_DBLCLKS | /*CS_HREDRAW | CS_VREDRAW |*/ CS_OWNDC;
   WndClass.lpfnWndProc = WndProc;
   WndClass.cbClsExtra = 0;
   WndClass.cbWndExtra = 0;
   WndClass.hInstance = GetInstance();

   // icon from resource
   hIcon = LoadIcon( GetResources(), lpIconName );

   // from file
   if( NULL == hIcon && HB_ISCHAR( 1 ) )
   {
      hIcon = ( HICON ) LoadImage( NULL, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE );
   }

   WndClass.hIcon = ( NULL != hIcon ) ? hIcon : LoadIcon( NULL, IDI_APPLICATION );

   // cursor from resource
   hCursor = LoadCursor( GetResources(), lpCursorName );

   // from file
   if( ( NULL == hCursor ) && HB_ISCHAR( 4 ) )
   {
      hCursor = LoadCursorFromFile( lpCursorName );
   }

   WndClass.hCursor = ( NULL != hCursor ) ? hCursor : LoadCursor( NULL, IDC_ARROW );

   if( HB_ISARRAY( 3 ) )               // old behavior (before 16.10)
   {
      if( HB_PARNI( 3, 1 ) == -1 )
      {
         hBrush = ( HBRUSH ) ( COLOR_BTNFACE + 1 );
      }
      else
      {
         hBrush = CreateSolidBrush( RGB( HB_PARNI( 3, 1 ), HB_PARNI( 3, 2 ), HB_PARNI( 3, 3 ) ) );
      }
   }
   else if( HB_ISCHAR( 3 ) || HB_ISNUM( 3 ) )
   {
      HBITMAP  hImage;
#ifndef UNICODE
      LPCTSTR  lpImageName = HB_ISCHAR( 3 ) ? hb_parc( 3 ) : ( HB_ISNUM( 3 ) ? MAKEINTRESOURCE( hmg_par_WORD( 3 ) ) : NULL );
#else
      LPWSTR   lpImageName = HB_ISCHAR( 3 ) ? AnsiToWide( ( char * ) hb_parc( 3 ) ) :
         ( HB_ISNUM( 3 ) ? ( LPWSTR ) MAKEINTRESOURCE( hmg_par_WORD( 3 ) ) : NULL );
#endif
      hImage = ( HBITMAP ) LoadImage( GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );

      if( hImage == NULL && HB_ISCHAR( 3 ) )
      {
         hImage = ( HBITMAP ) LoadImage( NULL, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
      }

#ifdef UNICODE
      hb_xfree( ( TCHAR * ) lpImageName );
#endif
      if( hImage == NULL )
      {
         hImage = ( HBITMAP ) HMG_LoadImage( hb_parc( 3 ) );
      }

      if( hImage != NULL )
      {
         hBrush = CreatePatternBrush( hImage );
      }
   }

   WndClass.hbrBackground = ( NULL != hBrush ) ? hBrush : ( hBrush = ( HBRUSH ) ( COLOR_BTNFACE + 1 ) );
   WndClass.lpszMenuName = NULL;
   WndClass.lpszClassName = lpClassName;

   if( !RegisterClass( &WndClass ) )
   {
      hmg_ErrorExit( TEXT( "Window Registration Failed!" ), 0, TRUE );
   }

#ifndef __XHARBOUR__
   hb_strfree( hClassName );
#endif
#ifdef UNICODE
   if( HB_ISCHAR( 1 ) )
   {
      hb_xfree( ( TCHAR * ) lpIconName );
   }

   if( HB_ISCHAR( 4 ) )
   {
      hb_xfree( ( TCHAR * ) lpCursorName );
   }
#endif
   hmg_ret_raw_HBRUSH( hBrush );
}

/* Modified by P.Ch. 17.06. */
HB_FUNC( REGISTERSPLITCHILDWINDOW )
{
   WNDCLASS    WndClass;
   HBRUSH      hbrush = 0;

#ifndef UNICODE
   LPCTSTR     lpIcon = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : ( HB_ISNIL( 1 ) ? NULL : MAKEINTRESOURCE( hmg_par_WORD( 1 ) ) );
#else
   LPWSTR      lpIcon = HB_ISCHAR( 1 ) ? AnsiToWide( ( char * ) hb_parc( 1 ) ) : ( HB_ISNIL( 1 ) ? NULL : ( LPWSTR ) MAKEINTRESOURCE( hmg_par_WORD( 1 ) ) );
#endif
#ifndef __XHARBOUR__
   void        *hClassName;
   LPCTSTR     lpClassName = HB_PARSTR( 2, &hClassName, NULL );
#else
   const char  *lpClassName = hb_parc( 2 );
#endif
   WndClass.style = CS_OWNDC;
   WndClass.lpfnWndProc = WndProc;
   WndClass.cbClsExtra = 0;
   WndClass.cbWndExtra = 0;
   WndClass.hInstance = GetInstance();
   WndClass.hIcon = LoadIcon( GetInstance(), lpIcon );
   if( WndClass.hIcon == NULL )
   {
      WndClass.hIcon = ( HICON ) LoadImage( 0, lpIcon, IMAGE_ICON, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE );
   }

   if( WndClass.hIcon == NULL )
   {
      WndClass.hIcon = LoadIcon( NULL, IDI_APPLICATION );
   }

   WndClass.hCursor = LoadCursor( NULL, IDC_ARROW );

   if( HB_PARNI( 3, 1 ) == -1 )
   {
      WndClass.hbrBackground = ( HBRUSH ) ( COLOR_BTNFACE + 1 );
   }
   else
   {
      hbrush = CreateSolidBrush( RGB( HB_PARNI( 3, 1 ), HB_PARNI( 3, 2 ), HB_PARNI( 3, 3 ) ) );
      WndClass.hbrBackground = hbrush;
   }

   WndClass.lpszMenuName = NULL;
   WndClass.lpszClassName = lpClassName;

   if( !RegisterClass( &WndClass ) )
   {
      hmg_ErrorExit( TEXT( "Window Registration Failed!" ), 0, TRUE );
   }

#ifndef __XHARBOUR__
   hb_strfree( hClassName );
#endif
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpIcon );
#endif
   hmg_ret_raw_HBRUSH( hbrush );
}

/* Modified by P.Ch. 17.06. */
HB_FUNC( UNREGISTERWINDOW )
{
#ifndef __XHARBOUR__
   void        *hClassName;
   LPCTSTR     lpClassName = HB_PARSTR( 1, &hClassName, NULL );
#else
   const char  *lpClassName = hb_parc( 1 );
#endif
   UnregisterClass( lpClassName, GetInstance() );
#ifndef __XHARBOUR__
   hb_strfree( hClassName );
#endif
}

//*********************************************
//    by Dr. Claudio Soto (July 2014)
//*********************************************

HB_FUNC( REBAR_GETHEIGHT )
{
   HWND hWnd    = hmg_par_raw_HWND( 1 );
   UINT nHeight = ( UINT ) SendMessage( hWnd, RB_GETBARHEIGHT, 0, 0 );

   hmg_ret_UINT( nHeight );
}

HB_FUNC( REBAR_GETBANDCOUNT )
{
   HWND hWnd       = hmg_par_raw_HWND( 1 );
   UINT nBandCount = ( UINT ) SendMessage( hWnd, RB_GETBANDCOUNT, 0, 0 );

   hmg_ret_UINT( nBandCount );
}

HB_FUNC( REBAR_GETBARRECT )
{
   HWND hWnd  = hmg_par_raw_HWND( 1 );
   UINT nBand = hmg_par_UINT( 2 );
   RECT Rect;

   SendMessage( hWnd, RB_GETRECT, ( WPARAM ) nBand, ( LPARAM ) &Rect );

   hb_reta( 6 );
   HB_STORVNL( ( LONG ) Rect.left, -1, 1 );
   HB_STORVNL( ( LONG ) Rect.top, -1, 2 );
   HB_STORVNL( ( LONG ) Rect.right, -1, 3 );
   HB_STORVNL( ( LONG ) Rect.bottom, -1, 4 );
   HB_STORVNL( ( LONG ) ( Rect.right - Rect.left ), -1, 5 );   // nWidth
   HB_STORVNL( ( LONG ) ( Rect.bottom - Rect.top ), -1, 6 );   // nHeight
}

HB_FUNC( REBAR_GETBANDBORDERS )
{
   HWND hWnd  = hmg_par_raw_HWND( 1 );
   UINT nBand = hmg_par_UINT( 2 );
   RECT Rect;

   SendMessage( hWnd, RB_GETBANDBORDERS, ( WPARAM ) nBand, ( LPARAM ) &Rect );

   hb_reta( 4 );
   HB_STORVNL( ( LONG ) Rect.left, -1, 1 );
   HB_STORVNL( ( LONG ) Rect.top, -1, 2 );
   HB_STORVNL( ( LONG ) Rect.right, -1, 3 );
   HB_STORVNL( ( LONG ) Rect.bottom, -1, 4 );
}

HB_FUNC( REBAR_SETMINCHILDSIZE )
{
   HWND hWnd  = hmg_par_raw_HWND( 1 );
   UINT nBand = hmg_par_UINT( 2 );
   UINT yMin  = hmg_par_UINT( 3 );

   REBARBANDINFO rbbi;

   rbbi.cbSize     = sizeof( REBARBANDINFO );
   rbbi.fMask      = RBBIM_CHILDSIZE;
   rbbi.cxMinChild = 0;
   rbbi.cyMinChild = yMin;
   rbbi.cx         = 0;

   SendMessage( hWnd, RB_SETBANDINFO, ( WPARAM ) nBand, ( LPARAM ) &rbbi );
}

HB_FUNC( REBAR_GETBANDINFO )
{
   HWND hWnd  = hmg_par_raw_HWND( 1 );
   UINT uBand = hmg_par_UINT( 2 );
   REBARBANDINFO rbbi;

   rbbi.cbSize = sizeof( REBARBANDINFO );
   rbbi.fMask  = RBBIM_CHILDSIZE | RBBIM_SIZE;

   SendMessage( hWnd, RB_GETBANDINFO, ( WPARAM ) uBand, ( LPARAM ) &rbbi );

   hb_reta( 7 );
   HB_STORVNL( ( LONG ) rbbi.cxMinChild, -1, 1 );
   HB_STORVNL( ( LONG ) rbbi.cyMinChild, -1, 2 );
   HB_STORVNL( ( LONG ) rbbi.cx, -1, 3 );
   HB_STORVNL( ( LONG ) rbbi.cyChild, -1, 4 );
   HB_STORVNL( ( LONG ) rbbi.cyMaxChild, -1, 5 );
   HB_STORVNL( ( LONG ) rbbi.cyIntegral, -1, 6 );
   HB_STORVNL( ( LONG ) rbbi.cxIdeal, -1, 7 );
}

HB_FUNC( MSC_VER )
{
#if defined( _MSC_VER )
   hb_retnl( _MSC_VER );
#else
   hb_retnl( 0 );
#endif
}

#define COMPILER_BUF_SIZE  80

HB_FUNC( BORLANDC )
{
   char        *pszCompiler;

#ifdef __BORLANDC__
   const char  *pszName;
   char        szSub[64];

   int         iVerMajor;
   int         iVerMinor;
   int         iVerPatch;

   pszCompiler = ( char * ) hb_xgrab( COMPILER_BUF_SIZE );
   szSub[0] = '\0';

#if ( __BORLANDC__ >= 0x0590 )         /* Version 5.9 */
#if ( __BORLANDC__ >= 0x0620 )         /* Version 6.2 */
   pszName = "Embarcadero C++";
#else
   pszName = "CodeGear C++";
#endif
#else
   pszName = "Borland C++";
#endif
#if ( __BORLANDC__ >= 0x0500 )         /* Version 5.x */
   iVerMajor = __BORLANDC__ >> 8;
   iVerMinor = ( __BORLANDC__ & 0xFF ) >> 4;
   iVerPatch = __BORLANDC__ & 0xF;
#else /* Version 4.x */
   iVerMajor = __BORLANDC__ >> 8;
   iVerMinor = ( __BORLANDC__ - 1 & 0xFF ) >> 4;
   iVerPatch = 0;
#endif
   if( pszName )
   {
      if( iVerPatch != 0 )
      {
         hb_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d.%d", pszName, szSub, iVerMajor, iVerMinor, iVerPatch );
      }
      else if( iVerMajor != 0 || iVerMinor != 0 )
      {
         hb_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d0", pszName, szSub, iVerMajor, iVerMinor );
      }
      else
      {
         hb_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s", pszName, szSub );
      }
   }
   else
   {
      hb_strncpy( pszCompiler, "(unknown)", COMPILER_BUF_SIZE - 1 );
   }

#if defined( HB_ARCH_32BIT )
   hb_strncat( pszCompiler, " (32-bit)", COMPILER_BUF_SIZE - 1 );
#elif defined( HB_ARCH_64BIT )
   hb_strncat( pszCompiler, " (64-bit)", COMPILER_BUF_SIZE - 1 );
#endif
#else
   pszCompiler = ( char * ) hb_xgrab( COMPILER_BUF_SIZE );
   strcpy( pszCompiler, "" );
#endif /* __BORLANDC__ */

   hb_retc_buffer( pszCompiler );
}

#include "mgver.h"

HB_FUNC( HMG_VERSION )
{
   char  *pszVersion;

   pszVersion = ( char * ) hb_xgrab( 40 );
   hb_snprintf( pszVersion, 40, "Harbour MiniGUI %d.%d.%d (%s)", MG_VER_MAJOR, MG_VER_MINOR, MG_VER_RELEASE, MG_VER_STATUS );

   hb_retc_buffer( pszVersion );
}

HB_FUNC( HMG_ISALPHA )
{
#ifndef UNICODE
   LPSTR    ch = ( char * ) hb_parc( 1 );
#else
   LPWSTR   ch = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   hb_retl( IsCharAlpha( ch[0] ) );
}

HB_FUNC( HMG_ISDIGIT )
{
#ifndef UNICODE
   LPSTR    ch = ( char * ) hb_parc( 1 );
#else
   LPWSTR   ch = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   hb_retl( IsCharAlphaNumeric( ch[0] ) && !IsCharAlpha( ch[0] ) );
}

#ifdef UNICODE
HB_FUNC( HMG_LOWER )
{
   LPSTR pStr;
   TCHAR *Text = ( TCHAR * ) AnsiToWide( ( char * ) hb_parc( 1 ) );
   INT   nLen;
   TCHAR *Buffer;

   if( Text == NULL )
   {
      hb_retc_null();
      return;
   }

   nLen = ( INT ) lstrlen( Text ) + 1;
   Buffer = ( TCHAR * ) hb_xgrab( nLen * sizeof( TCHAR ) );

   if( Buffer != NULL )
   {
      lstrcpy( Buffer, Text );
      CharLower( Buffer );
      pStr = WideToAnsi( Buffer );
      hb_retc( pStr );
      hb_xfree( pStr );
   }
   else
   {
      hb_retc_null();
   }

   hb_xfree( Text );
   hb_xfree( Buffer );
}

HB_FUNC( HMG_UPPER )
{
   LPSTR pStr;
   TCHAR *Text = ( TCHAR * ) AnsiToWide( ( char * ) hb_parc( 1 ) );
   INT   nLen;
   TCHAR *Buffer;

   if( Text == NULL )
   {
      hb_retc_null();
      return;
   }

   nLen = ( INT ) lstrlen( Text ) + 1;
   Buffer = ( TCHAR * ) hb_xgrab( nLen * sizeof( TCHAR ) );

   if( Buffer != NULL )
   {
      lstrcpy( Buffer, Text );
      CharUpper( Buffer );
      pStr = WideToAnsi( Buffer );
      hb_retc( pStr );
      hb_xfree( pStr );
   }
   else
   {
      hb_retc_null();
   }

   hb_xfree( Text );
   hb_xfree( Buffer );
}

HB_FUNC( HMG_ISLOWER )
{
#ifndef UNICODE
   LPSTR    Text = ( LPSTR ) hb_parc( 1 );
#else
   LPWSTR   Text = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   hb_retl( IsCharLower( Text[0] ) );

#ifdef UNICODE
   hb_xfree( Text );
#endif
}

HB_FUNC( HMG_ISUPPER )
{
#ifndef UNICODE
   LPSTR    Text = ( LPSTR ) hb_parc( 1 );
#else
   LPWSTR   Text = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   hb_retl( IsCharUpper( Text[0] ) );

#ifdef UNICODE
   hb_xfree( Text );
#endif
}

#else
HB_FUNC_TRANSLATE( HMG_LOWER, LOWER )
HB_FUNC_TRANSLATE( HMG_UPPER, UPPER )
HB_FUNC_TRANSLATE( HMG_ISLOWER, ISLOWER )
HB_FUNC_TRANSLATE( HMG_ISUPPER, ISUPPER )
#endif /* UNICODE */
