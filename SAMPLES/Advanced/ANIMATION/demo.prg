/*
 * Application Name : WinAnim (Bitmap Animation Demo)
 * Author           : Brook Miles (original), adapted by Grigory Filatov
 * Framework        : Harbour MiniGUI Extended Edition (HMG Extended)
 * Description      : Demonstrates basic bitmap animation using native Windows API and HMG frontend.
 */

ANNOUNCE RDDSYS

#include <hmg.ch>

/*
 * FUNCTION Main()
 *
 * Initializes the main application window, loads bitmap resources, sets up the timer,
 * and handles cleanup on exit.
 *
 * Purpose:
 *   This is the main entry point for the application. It creates the primary window,
 *   loads animation resources, binds event handlers, and starts the message loop.
 *
 * Notes:
 *   Bitmap resources "BALLBMP" and "MASKBMP" must be available in the executable resource section.
 */
FUNCTION Main()
   LOCAL aBitmaps

   DEFINE WINDOW MainWin ;
      WIDTH  320 ;
      HEIGHT 240 ;
      TITLE "Bitmap Animation Demo" ;
      MAIN ;
      ON INIT      ( aBitmaps := LoadAndInitResources() ) ;
      ON RELEASE   AEval( aBitmaps, {|hBmp| DeleteObject( hBmp ) } ) ;
      ON PAINT     OnPaintHandler( This.Handle )

      // Optional: Add edge style to give the window a recessed look
      ChangeStyle( This.Handle, WS_EX_CLIENTEDGE, , .T. )

   END WINDOW

   MainWin.Center()
   MainWin.Activate()

RETURN NIL

/*
 * FUNCTION LoadAndInitResources()
 *
 * Loads bitmap resources, initializes the animation state, and starts the update timer.
 *
 * Returns:
 *   Array of bitmap handles: { hbmBall, hbmMask }
 *
 * Purpose:
 *   Abstracts resource loading and initialization logic for better modularity.
 *   Ensures error handling if resources are missing or corrupted.
 */
FUNCTION LoadAndInitResources()
   LOCAL hbmBall, hbmMask

   hbmBall := LoadBitmap( "BALLBMP" )
   hbmMask := LoadBitmap( "MASKBMP" )

   IF Empty( hbmBall ) .OR. Empty( hbmMask )
      MsgStop( "Failed to load bitmap resources.", "Initialization Error" )
      QUIT
   ENDIF

   // Initialize ball properties in native C backend
   InitBall( hbmBall, hbmMask )

   // Create a timer to refresh animation roughly every 33ms (~30 FPS)
   DEFINE TIMER UpdateTimer OF MainWin INTERVAL 33 ACTION OnTimerHandler( ThisWindow.Handle )

RETURN { hbmBall, hbmMask }

/*
 * PROCEDURE OnTimerHandler( hwnd )
 *
 * Timer event handler that updates and redraws the ball.
 *
 * Parameters:
 *   hwnd : Window handle (numeric) of the main window.
 *
 * Purpose:
 *   Called by the system timer every 33 milliseconds. It performs one full animation cycle:
 *   erase -> update -> redraw.
 */
PROCEDURE OnTimerHandler( hwnd )
   LOCAL hdcWindow := GetDC( hwnd )

   EraseBall( hdcWindow )     // Clear previous frame
   UpdateBall( hwnd )         // Update position and bounce logic
   DrawBall( hdcWindow )      // Draw updated ball

   ReleaseDC( hwnd, hdcWindow )
RETURN

/*
 * PROCEDURE OnPaintHandler( hwnd )
 *
 * Paint event handler to draw the ball when the window is redrawn.
 *
 * Parameters:
 *   hwnd : Window handle (numeric) of the main window.
 *
 * Purpose:
 *   Called by the operating system when the window needs repainting (e.g., after being uncovered).
 *   Re-renders the ball without updating its position.
 */
PROCEDURE OnPaintHandler( hwnd )
   LOCAL hdcWindow, ps

   hdcWindow := BeginPaint( hwnd, @ps )
   DrawBall( hdcWindow )
   EndPaint( hwnd, ps )
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"

/*---------------------------------
 * Constants and Globals
 *---------------------------------*/
#define BALL_SPEED 4  // Default speed in pixels per tick

// Global bitmap handles
static HBITMAP g_hBmpBall = NULL;
static HBITMAP g_hBmpMask = NULL;
static BITMAP  g_ballBitmapInfo;

// Ball position and velocity
static int g_ballX = 0, g_ballY = 0;
static int g_deltaX = BALL_SPEED, g_deltaY = BALL_SPEED;

/*---------------------------------
 * Function Prototypes
 *---------------------------------*/
void InitBall(HBITMAP hBall, HBITMAP hMask);
void EraseBall(HDC hdc);
void DrawBall(HDC hdc);
void UpdateBall(HWND hwnd);

/*---------------------------------
 * Function: InitBall
 *
 * Initializes the global ball image and movement settings.
 *---------------------------------*/
void InitBall(HBITMAP hBall, HBITMAP hMask)
{
    g_hBmpBall = hBall;
    g_hBmpMask = hMask;

    GetObject(g_hBmpBall, sizeof(g_ballBitmapInfo), &g_ballBitmapInfo);

    g_ballX = 0;
    g_ballY = 0;
    g_deltaX = BALL_SPEED;
    g_deltaY = BALL_SPEED;
}

/*---------------------------------
 * Function: EraseBall
 *
 * Clears the ball’s previous position using system background color.
 *---------------------------------*/
void EraseBall(HDC hdc)
{
    RECT rc;
    rc.left   = g_ballX;
    rc.top    = g_ballY;
    rc.right  = g_ballX + g_ballBitmapInfo.bmWidth;
    rc.bottom = g_ballY + g_ballBitmapInfo.bmHeight;

    FillRect(hdc, &rc, GetSysColorBrush(COLOR_BTNFACE));
}

/*---------------------------------
 * Function: DrawBall
 *
 * Draws the ball bitmap using transparency mask (mask + image).
 *---------------------------------*/
void DrawBall(HDC hdc)
{
    HGDIOBJ hOldBmp;
    HDC hdcMem = CreateCompatibleDC(hdc);
    if (!hdcMem) return;

    hOldBmp = SelectObject(hdcMem, g_hBmpMask);
    BitBlt(hdc, g_ballX, g_ballY, g_ballBitmapInfo.bmWidth, g_ballBitmapInfo.bmHeight, hdcMem, 0, 0, SRCAND);

    SelectObject(hdcMem, g_hBmpBall);
    BitBlt(hdc, g_ballX, g_ballY, g_ballBitmapInfo.bmWidth, g_ballBitmapInfo.bmHeight, hdcMem, 0, 0, SRCPAINT);

    SelectObject(hdcMem, hOldBmp);
    DeleteDC(hdcMem);
}

/*---------------------------------
 * Function: UpdateBall
 *
 * Moves the ball and handles bouncing off window edges.
 *---------------------------------*/
void UpdateBall(HWND hwnd)
{
    RECT rcClient;
    GetClientRect(hwnd, &rcClient);

    g_ballX += g_deltaX;
    g_ballY += g_deltaY;

    if (g_ballX < 0) {
        g_ballX = 0;
        g_deltaX = BALL_SPEED;
    } else if (g_ballX + g_ballBitmapInfo.bmWidth > rcClient.right) {
        g_ballX = rcClient.right - g_ballBitmapInfo.bmWidth;
        g_deltaX = -BALL_SPEED;
    }

    if (g_ballY < 0) {
        g_ballY = 0;
        g_deltaY = BALL_SPEED;
    } else if (g_ballY + g_ballBitmapInfo.bmHeight > rcClient.bottom) {
        g_ballY = rcClient.bottom - g_ballBitmapInfo.bmHeight;
        g_deltaY = -BALL_SPEED;
    }
}

/*---------------------------------
 * Harbour Bridge Functions
 *---------------------------------*/
HB_FUNC(INITBALL)
{
    HBITMAP hBall = (HBITMAP) hb_parnl(1);
    HBITMAP hMask = (HBITMAP) hb_parnl(2);
    InitBall(hBall, hMask);
}

HB_FUNC(ERASEBALL)
{
    HDC hdc = (HDC) hb_parnl(1);
    EraseBall(hdc);
}

HB_FUNC(DRAWBALL)
{
    HDC hdc = (HDC) hb_parnl(1);
    DrawBall(hdc);
}

HB_FUNC(UPDATEBALL)
{
    HWND hwnd = (HWND) hb_parnl(1);
    UpdateBall(hwnd);
}

#pragma ENDDUMP
