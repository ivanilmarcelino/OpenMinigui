// Author: Kamil Kalus
// Email: kamilkalus0[at]gmail.com

#include <mgdefs.h>

#include <commctrl.h>

#include "hbvm.h"

#define HOVER_TIME 15 // 1/4th second hover time


#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );
#endif

HINSTANCE         GetInstance( void );

LRESULT CALLBACK OwnSplitterProc( HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam );


HB_FUNC( INITSPLITTER )
{

#ifndef UNICODE
    LPCSTR   lpWindowName = hb_parc( 2 );
#else
   LPCWSTR  lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
    DWORD    Style =  WS_CHILD | WS_VISIBLE;

    
    // Register SPLITTER class if it hasn't been registered yet
    WNDCLASS    splitterWc = {0};    

    splitterWc.lpfnWndProc = OwnSplitterProc;
    splitterWc.hInstance = GetInstance();
    splitterWc.lpszClassName = "SPLITTER";
    splitterWc.style = CS_DBLCLKS;
    splitterWc.hCursor = LoadCursor(NULL, IDC_SIZEWE);

    if( !RegisterClass( &splitterWc ) ) {
       // hmg_ErrorExit( TEXT( "SPLITTER Registration Failed!" ), 0, TRUE );
    }

    hmg_ret_raw_HWND
    (
    CreateWindowEx
        (
            0,
            "SPLITTER",
            lpWindowName,
            Style,
            hb_parni( 4 ),
            hb_parni( 5 ),
            hb_parni( 6 ),
            hb_parni( 7 ),
            hmg_par_raw_HWND( 1 ),
            hmg_par_raw_HMENU( 3 ),
            GetInstance(),
            NULL
        )
    );


#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpWindowName );
#endif

}



LRESULT CALLBACK OwnSplitterProc( HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam ) {
    static BOOL isTracking = FALSE;
    static BOOL dragging = FALSE;
    //static int dragStartX;
    static PHB_SYMB   pSymbol = NULL;
    POINT pt;

    if( !pSymbol ) {
        pSymbol = hb_dynsymSymbol( hb_dynsymGet( "SPLITTEREVENTS" ) );
    }

    switch (msg) {

        case WM_LBUTTONDBLCLK:
            GetCursorPos(&pt);
            ScreenToClient(GetParent(hwnd), &pt);

            if( pSymbol ) {
                hb_vmPushSymbol( pSymbol );
                hb_vmPushNil();
                hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
                hb_vmPushLong( msg );
                hb_vmPushNumInt( pt.x );
                hb_vmPushNumInt( pt.y );
                hb_vmDo( 4 );
            }
            break;

        case WM_LBUTTONDOWN:
            SetCapture(hwnd);
            //dragStartX = LOWORD(lParam);
            dragging = TRUE;

            if( pSymbol ){
                hb_vmPushSymbol( pSymbol );
                hb_vmPushNil();
                hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
                hb_vmPushLong( msg );
                hb_vmPushNumInt( wParam );
                hb_vmPushNumInt( lParam );
                hb_vmDo( 4 );
            }
            break;

        case WM_MOUSEMOVE:
            if (!isTracking) {
                    TRACKMOUSEEVENT tme;
                    tme.cbSize = sizeof(tme);
                    tme.dwFlags = TME_HOVER | TME_LEAVE;
                    tme.hwndTrack = hwnd;
                    tme.dwHoverTime = HOVER_TIME; // Time in milliseconds
                    TrackMouseEvent(&tme);
                    isTracking = TRUE;
            }

            if (dragging) {
                // Get current mouse position relative to the parent window
                GetCursorPos(&pt);
                ScreenToClient(GetParent(hwnd), &pt);

                if( pSymbol ) {
                    hb_vmPushSymbol( pSymbol );
                    hb_vmPushNil();
                    hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
                    hb_vmPushLong( msg );
                    hb_vmPushNumInt( pt.x );
                    hb_vmPushNumInt( pt.y );
                    hb_vmDo( 4 );
                }
            } else {
                if( pSymbol ) {
                        hb_vmPushSymbol( pSymbol );
                        hb_vmPushNil();
                        hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
                        hb_vmPushLong( WM_SETFOCUS );
                        hb_vmPushNumInt( 0 );
                        hb_vmPushNumInt( 0 );
                        hb_vmDo( 4 );
                    }
            }
            break;

        case WM_LBUTTONUP:
            if (dragging) {
                dragging = FALSE;
                ReleaseCapture();

                // Get the final mouse position
                GetCursorPos(&pt);
                ScreenToClient(GetParent(hwnd), &pt);

                if( pSymbol ) {
                    hb_vmPushSymbol( pSymbol );
                    hb_vmPushNil();
                    hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
                    hb_vmPushLong( msg );
                    hb_vmPushNumInt( pt.x );
                    hb_vmPushNumInt( pt.y );
                    hb_vmDo( 4 );
                }
            }
            break;

        case WM_MOUSEHOVER:
            if( pSymbol ) {
                hb_vmPushSymbol( pSymbol );
                hb_vmPushNil();
                hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
                hb_vmPushLong( msg );
                hb_vmPushNumInt( wParam );
                hb_vmPushNumInt( lParam );
                hb_vmDo( 4 );
            }
            isTracking = FALSE;
            break;

        case WM_MOUSELEAVE: 
            if( pSymbol ) {
                hb_vmPushSymbol( pSymbol );
                hb_vmPushNil();
                hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
                hb_vmPushLong( msg );
                hb_vmPushNumInt( wParam );
                hb_vmPushNumInt( lParam );
                hb_vmDo( 4 );
            }
            isTracking = FALSE;
            break;

        case WM_PAINT: {
            PAINTSTRUCT ps;
            BeginPaint(hwnd, &ps);

            if( pSymbol ) {
                hb_vmPushSymbol( pSymbol );
                hb_vmPushNil();
                hb_vmPushNumInt( ( HB_PTRUINT ) hwnd );
                hb_vmPushLong( msg );
                hb_vmPushNumInt( wParam );
                hb_vmPushNumInt( lParam );
                hb_vmDo( 4 );
            }
            EndPaint(hwnd, &ps);
            break;
        }

        default:
            return DefWindowProc(hwnd, msg, wParam, lParam);

    }
    return 0;
}
