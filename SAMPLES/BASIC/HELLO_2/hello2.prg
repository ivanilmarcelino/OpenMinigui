/*
 * Harbour MiniGUI Hello World Demo
 *
 * A example of using of Win32 API for calculation of controls size and placement.
*/

#include "minigui.ch"

PROCEDURE Main()

   LOCAL aBtn

   DEFINE WINDOW Win_1 ;
         CLIENTAREA 640, 480 ;
         TITLE 'Hello' ;
         WINDOWTYPE MAIN ;
         BACKCOLOR WHITE ;
         ON INIT OnSizeProc( aBtn ) ;
         ON SIZE OnSizeProc( aBtn ) ;
         ON MAXIMIZE OnSizeProc( aBtn )

      @ 0, 0 LABEL Label_1 ;
         VALUE 'Hello, World!' ;
         HEIGHT thisWindow.Height / 2 ;
         BACKCOLOR WHITE ;
         SIZE 10 BOLD ;
         CENTERALIGN VCENTERALIGN

      aBtn := GetButtonSize( this.Handle )

      DEFINE BUTTONEX Button_1
         ROW 0
         COL 0
         WIDTH aBtn[ 1 ]
         HEIGHT aBtn[ 2 ]
         CAPTION 'Click Here' + CRLF + 'For Exit'
         ACTION thisWindow.Release()
         FONTSIZE 10
         FONTBOLD .T.
      END BUTTONEX

   END WINDOW

   Win_1.Center()
   Win_1.Activate()

RETURN


PROCEDURE OnSizeProc( aBtn )

   LOCAL hwndButton, left, top, cx, cy
   LOCAL ClientWidth, ClientHeight

   /* The window size is changing. */
   ClientWidth := this.ClientWidth
   ClientHeight := this.ClientHeight

   /* Draw "Hello, World" in the middle of the upper half of the window. */
   this.Label_1.WIDTH := ClientWidth
   this.Label_1.HEIGHT := ClientHeight / 2

   hwndButton := this.Button_1.Handle
   cx := aBtn[ 1 ]
   cy := aBtn[ 2 ]

   /* Place the button in the center of the bottom half of the window. */
   left := ( ClientWidth - cx ) / 2
   top := ClientHeight * 3 / 4 - cy / 2

   MoveWindow ( hwndButton, left, top, cx, cy, .T. )

RETURN

#define SYSTEM_FIXED_FONT   16

FUNCTION GetButtonSize( hwnd )

   LOCAL hdc, tm, cx, cy
   LOCAL tmAveCharWidth, tmHeight, tmExternalLeading

   hdc := GetDC ( hwnd )

   /* We use the system fixed font size to choose a button size. */
   SelectObject ( hdc, GetStockObject ( SYSTEM_FIXED_FONT ) )

   tm := GetTextMetric ( hdc )
   tmHeight := tm[ 1 ]
   tmAveCharWidth := tm[ 2 ]
   tmExternalLeading := tm[ 7 ]
   cx := tmAveCharWidth * 24
   cy := ( tmHeight + tmExternalLeading ) * 3

   ReleaseDC ( hwnd, hdc )

RETURN { cx, cy }
