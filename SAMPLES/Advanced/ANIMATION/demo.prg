/*
   Name: WinAnim
   Author: Brook Miles
   Description: Making an animation in windows

   Adapted for MiniGUI Extended Edition by Grigory Filatov - 2018
 */

ANNOUNCE RDDSYS

#include <hmg.ch>

/*
 * FUNCTION Main()
 *
 * This is the main function of the application. It defines and activates the main window,
 * initializes resources, and sets up event handlers.
 *
 * Purpose:
 *   This function serves as the entry point for the application. It creates the main window,
 *   sets its properties (size, title), and defines event handlers for initialization,
 *   window release, and painting. The window is then centered and activated, starting
 *   the application's event loop.
 *
 * Notes:
 *   The ON RELEASE event handler ensures that bitmaps are deleted when the window is closed,
 *   preventing memory leaks. The ON PAINT event handler calls OnPaintProc to handle window
 *   redrawing.
 */
FUNCTION Main()

   LOCAL aBitmaps

   DEFINE WINDOW Main ;
         WIDTH 320 HEIGHT 240 ;
         TITLE "A Bitmap Program" ;
         MAIN ;
         ON INIT ( aBitmaps := InitMain() ) ;
         ON RELEASE AEval( aBitmaps, {| hBmp | DeleteObject( hBmp ) } ) ;
         ON PAINT OnPaintProc( This.Handle )

      ChangeStyle( this.Handle, WS_EX_CLIENTEDGE, , .T. )

   END WINDOW

   Main.Center()
   Main.Activate()

RETURN NIL

/*
 * FUNCTION InitMain()
 *
 * Initializes the bitmaps used for the animation and sets up the timer.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   An array containing the handles of the loaded bitmaps (hbmBall, hbmMask).
 *
 * Purpose:
 *   This function loads the bitmap resources ("BALLBMP" and "MASKBMP") required for the
 *   animation. It checks if the bitmaps were loaded successfully and quits the application
 *   if either load fails. It then calls InitBall (defined in the #pragma BEGINDUMP section)
 *   to initialize the ball's properties and sets up a timer to trigger the animation updates.
 *   The timer's interval is set to 33 milliseconds, resulting in approximately 30 frames per second.
 *
 * Notes:
 *   The function assumes that the "BALLBMP" and "MASKBMP" files are located in the same directory
 *   as the executable or in a directory specified in the PATH environment variable.
 */
FUNCTION InitMain

   LOCAL hbmBall, hbmMask

   hbmBall := LoadBitmap( "BALLBMP" )
   hbmMask := LoadBitmap( "MASKBMP" )

   IF Empty( hbmBall ) .OR. Empty( hbmMask )
      MsgStop( "Load of resources failed.", "Error" )
      QUIT
   ENDIF

   InitBall( hbmBall, hbmMask )

   DEFINE TIMER Timer_1 OF Main INTERVAL 33 ACTION OnTimerProc( ThisWindow.Handle )

RETURN { hbmBall, hbmMask }

/*
 * PROCEDURE OnTimerProc( hwnd )
 *
 * Handles the timer event, updating and redrawing the ball animation.
 *
 * Parameters:
 *   hwnd : The handle of the window.
 *
 * Purpose:
 *   This procedure is called periodically by the timer. It retrieves the device context (HDC)
 *   of the window, erases the previous position of the ball, updates the ball's position,
 *   draws the ball at its new position, and releases the device context. This sequence creates
 *   the animation effect.
 *
 * Notes:
 *   The function relies on the EraseBall, UpdateBall, and DrawBall functions (defined in the
 *   #pragma BEGINDUMP section) to perform the actual animation logic.
 */
PROCEDURE OnTimerProc( hwnd )

   LOCAL hdcWindow

   hdcWindow := GetDC( hwnd )

   EraseBall( hdcWindow )
   UpdateBall( hwnd )
   DrawBall( hdcWindow )

   ReleaseDC( hwnd, hdcWindow )

RETURN

/*
 * PROCEDURE OnPaintProc( hwnd )
 *
 * Handles the WM_PAINT message, redrawing the ball when the window needs to be repainted.
 *
 * Parameters:
 *   hwnd : The handle of the window.
 *
 * Purpose:
 *   This procedure is called when the window needs to be repainted (e.g., when it is resized,
 *   uncovered, or restored). It retrieves the device context (HDC) of the window using
 *   BeginPaint, draws the ball using DrawBall, and releases the device context using EndPaint.
 *   This ensures that the ball is always visible, even after the window has been obscured.
 *
 * Notes:
 *   The ps variable (PAINTSTRUCT) is passed by reference to BeginPaint and EndPaint, as required
 *   by the Windows API.
 */
PROCEDURE OnPaintProc( hwnd )

   LOCAL hdcWindow, ps

   hdcWindow := BeginPaint( hWnd, @ps )
   DrawBall( hdcWindow )
   EndPaint( hWnd, ps )

RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"

// Global variables to store the handles of the ball and mask bitmaps.
HBITMAP hbmBall, hbmMask;
// Global variable to store the bitmap information.
BITMAP  bm;

// Global variables to store the ball's position and movement direction.
int ballX, ballY;
int deltaX, deltaY;

// Global variable to control the speed of the ball.
int deltaValue = 4;

/*
 * FUNCTION InitBall( hBall, hMask )
 *
 * Initializes the ball's properties, including its bitmaps, initial position, and movement direction.
 *
 * Parameters:
 *   hBall : The handle of the ball bitmap.
 *   hMask : The handle of the mask bitmap.
 *
 * Purpose:
 *   This function sets up the initial state of the ball for the animation. It assigns the provided
 *   bitmap handles to the global variables hbmBall and hbmMask. It also retrieves the bitmap
 *   information (width and height) using GetObject and stores it in the bm structure. The ball's
 *   initial position is set to (0, 0), and its initial movement direction is set to deltaValue
 *   in both the X and Y directions.
 *
 * Notes:
 *   The mask bitmap is used to create transparency for the ball. The ball's movement direction
 *   is controlled by the deltaX and deltaY variables, which determine how many pixels the ball
 *   moves in each direction per frame.
 */
void InitBall( HBITMAP hBall, HBITMAP hMask )
{
   hbmBall = hBall;
   hbmMask = hMask;

   GetObject( hbmBall, sizeof( bm ), &bm );

   ballX  = 0;
   ballY  = 0;
   deltaX = deltaValue;
   deltaY = deltaValue;
}

/*
 * FUNCTION EraseBall( hdc )
 *
 * Erases the ball from the specified device context by filling the ball's previous rectangle with the background color.
 *
 * Parameters:
 *   hdc : The handle of the device context.
 *
 * Purpose:
 *   This function removes the ball from its previous position on the screen before it is redrawn at its new position.
 *   It creates a rectangle (RECT) that represents the ball's previous bounding box and then fills this rectangle
 *   with the window's background color using FillRect. This effectively erases the ball from the screen.
 *
 * Notes:
 *   The COLOR_BTNFACE + 1 constant represents the system color for the face of a button, which is typically used
 *   as the background color for windows.
 */
void EraseBall( HDC hdc )
{
   RECT rc;

   rc.left   = ballX;
   rc.top    = ballY;
   rc.right  = ballX + bm.bmWidth;
   rc.bottom = ballY + bm.bmHeight;

   FillRect( hdc, &rc, ( HBRUSH ) ( COLOR_BTNFACE + 1 ) );
}

/*
 * FUNCTION DrawBall( hdc )
 *
 * Draws the ball onto the specified device context using a mask to create transparency.
 *
 * Parameters:
 *   hdc : The handle of the device context.
 *
 * Purpose:
 *   This function draws the ball onto the screen at its current position (ballX, ballY). It uses a mask bitmap
 *   (hbmMask) to create transparency, allowing the background to show through the ball. The function first creates
 *   a compatible device context (hdcMemory) and selects the mask bitmap into it. It then uses BitBlt with the
 *   SRCAND raster operation to apply the mask to the destination device context (hdc). This makes the transparent
 *   parts of the mask bitmap transparent in the destination. Next, the function selects the ball bitmap (hbmBall)
 *   into the memory device context and uses BitBlt with the SRCPAINT raster operation to draw the ball onto the
 *   destination device context. This draws the opaque parts of the ball over the transparent parts created by the mask.
 *   Finally, the function deletes the memory device context to free resources.
 *
 * Notes:
 *   The SRCAND and SRCPAINT raster operations are used to combine the mask and ball bitmaps in a way that creates
 *   transparency. The CreateCompatibleDC function creates a device context that is compatible with the specified
 *   device context (hdc), allowing bitmaps to be transferred between them efficiently.
 */
void DrawBall( HDC hdc )
{
   HDC hdcMemory;

   hdcMemory = CreateCompatibleDC( hdc );

   SelectObject( hdcMemory, hbmMask );
   BitBlt( hdc, ballX, ballY, bm.bmWidth, bm.bmHeight, hdcMemory, 0, 0, SRCAND );

   SelectObject( hdcMemory, hbmBall );
   BitBlt( hdc, ballX, ballY, bm.bmWidth, bm.bmHeight, hdcMemory, 0, 0, SRCPAINT );

   DeleteDC( hdcMemory );
}

/*
 * FUNCTION UpdateBall( hwnd )
 *
 * Updates the ball's position and handles collision detection with the window boundaries.
 *
 * Parameters:
 *   hwnd : The handle of the window.
 *
 * Purpose:
 *   This function updates the ball's position based on its current movement direction (deltaX, deltaY).
 *   It also checks if the ball has collided with the edges of the window. If a collision is detected,
 *   the ball's direction is reversed, causing it to bounce off the edge. The function retrieves the
 *   window's client rectangle using GetClientRect and uses this rectangle to determine the window boundaries.
 *
 * Notes:
 *   The function ensures that the ball stays within the window boundaries by reversing its direction when
 *   it hits an edge. The deltaValue variable controls the speed of the ball.
 */
void UpdateBall( HWND hwnd )
{
   RECT rc;

   GetClientRect( hwnd, &rc );

   ballX += deltaX;
   ballY += deltaY;

   if( ballX < 0 )
   {
      ballX  = 0;
      deltaX = deltaValue;
   }
   else if( ballX + bm.bmWidth > rc.right )
   {
      ballX  = rc.right - bm.bmWidth;
      deltaX = -deltaValue;
   }

   if( ballY < 0 )
   {
      ballY  = 0;
      deltaY = deltaValue;
   }
   else if( ballY + bm.bmHeight > rc.bottom )
   {
      ballY  = rc.bottom - bm.bmHeight;
      deltaY = -deltaValue;
   }
}

/*
 * HB_FUNC( INITBALL )
 *
 * Harbour function wrapper for the InitBall C function.
 *
 * Parameters:
 *   1: hBall - The handle of the ball bitmap (passed as a number).
 *   2: hMask - The handle of the mask bitmap (passed as a number).
 *
 * Purpose:
 *   This function acts as a bridge between the Harbour code and the C code. It receives the bitmap
 *   handles as numeric parameters from Harbour, casts them to HBITMAP pointers, and then calls the
 *   InitBall C function to initialize the ball's properties.
 *
 * Notes:
 *   The hb_parnl function is used to retrieve numeric parameters from Harbour. The (HBITMAP) cast
 *   converts the numeric value to a HBITMAP pointer.
 */
HB_FUNC( INITBALL )
{
   HBITMAP hbmBall = ( HBITMAP ) hb_parnl( 1 );
   HBITMAP hbmMask = ( HBITMAP ) hb_parnl( 2 );

   InitBall( hbmBall, hbmMask );
}

/*
 * HB_FUNC( ERASEBALL )
 *
 * Harbour function wrapper for the EraseBall C function.
 *
 * Parameters:
 *   1: hdc - The handle of the device context (passed as a number).
 *
 * Purpose:
 *   This function acts as a bridge between the Harbour code and the C code. It receives the device
 *   context handle as a numeric parameter from Harbour, casts it to an HDC pointer, and then calls
 *   the EraseBall C function to erase the ball from the screen.
 *
 * Notes:
 *   The hb_parnl function is used to retrieve numeric parameters from Harbour. The (HDC) cast
 *   converts the numeric value to an HDC pointer.
 */
HB_FUNC( ERASEBALL )
{
   HDC hdc = ( HDC ) hb_parnl( 1 );

   EraseBall( hdc );
}

/*
 * HB_FUNC( DRAWBALL )
 *
 * Harbour function wrapper for the DrawBall C function.
 *
 * Parameters:
 *   1: hdc - The handle of the device context (passed as a number).
 *
 * Purpose:
 *   This function acts as a bridge between the Harbour code and the C code. It receives the device
 *   context handle as a numeric parameter from Harbour, casts it to an HDC pointer, and then calls
 *   the DrawBall C function to draw the ball onto the screen.
 *
 * Notes:
 *   The hb_parnl function is used to retrieve numeric parameters from Harbour. The (HDC) cast
 *   converts the numeric value to an HDC pointer.
 */
HB_FUNC( DRAWBALL )
{
   HDC hdc = ( HDC ) hb_parnl( 1 );

   DrawBall( hdc );
}

/*
 * HB_FUNC( UPDATEBALL )
 *
 * Harbour function wrapper for the UpdateBall C function.
 *
 * Parameters:
 *   1: hwnd - The handle of the window (passed as a number).
 *
 * Purpose:
 *   This function acts as a bridge between the Harbour code and the C code. It receives the window
 *   handle as a numeric parameter from Harbour, casts it to an HWND pointer, and then calls the
 *   UpdateBall C function to update the ball's position and handle collision detection.
 *
 * Notes:
 *   The hb_parnl function is used to retrieve numeric parameters from Harbour. The (HWND) cast
 *   converts the numeric value to an HWND pointer.
 */
HB_FUNC( UPDATEBALL )
{
   HWND hwnd = ( HWND ) hb_parnl( 1 );

   UpdateBall( hwnd );
}

#pragma ENDDUMP
