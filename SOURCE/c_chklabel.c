/*----------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

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
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other 
   files to produce an executable, this does not by itself cause the resulting 
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the 
   Harbour-Minigui library code into it.

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

 ---------------------------------------------------------------------------*/

/* Include necessary headers for MiniGUI, Windows controls, and Harbour API */
#include <mgdefs.h>
#include <commctrl.h>

/* Define static class name for older Borland compilers */
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_STATIC "Static"
#endif
#include "hbapiitm.h"
#include "hbvm.h"

/* Declare window procedure for custom check label control */
LRESULT APIENTRY  ChkLabelFunc( HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam );
static WNDPROC    LabelOldWndProc;                 // Store original window procedure

/* External function to load images with specified parameters */
extern HBITMAP    HMG_LoadPicture
                  (
                     const char  *FileName,        // Image file path
                     int         New_Width,        // Desired width
                     int         New_Height,       // Desired height
                     HWND        hWnd,             // Parent window handle
                     int         ScaleStretch,     // Scaling option
                     int         Transparent,      // Transparency option
                     long        BackgroundColor,  // Background color
                     int         AdjustImage,      // Image adjustment flag
                     HB_BOOL     bAlphaFormat,     // Alpha format support
                     int         iAlpfaConstant    // Alpha constant value
                  );
#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );            // Convert ANSI to Unicode string
#endif
HINSTANCE         GetInstance( void );             // Get current instance handle
HINSTANCE         GetResources( void );            // Get resource instance handle

/* Structure to store check label control state */
typedef struct
{
   BOOL     lCheck;                    // Indicates if the control is checked
   int      cxLeftEdge, cxRightEdge;   // Window border sizes
   int      cxButton;                  // Check box button width
   int      cxSpace;                   // Spacing between check box and label
   BOOL     lLeftCheck;                // Check box on left side flag
   HBITMAP  himage;                    // Bitmap for checked state
   HBITMAP  himagemask;                // Mask for checked state bitmap
   HBITMAP  himage2;                   // Bitmap for unchecked state
   HBITMAP  himagemask2;               // Mask for unchecked state bitmap
} INSCHK, *PINSCHK;

/* Create a bitmap mask for transparent image rendering */
HBITMAP CreateBitmapMask( HBITMAP hbmColour, COLORREF crTransparent )
{
   HDC      hdcMem, hdcMem2;           // Device contexts for bitmap operations
   HBITMAP  hbmMask;                   // Mask bitmap
   BITMAP   bm;  // Bitmap information structure
   GetObject( hbmColour, sizeof( BITMAP ), &bm );                 // Get bitmap properties
   hbmMask = CreateBitmap( bm.bmWidth, bm.bmHeight, 1, 1, NULL ); // Create monochrome bitmap
   hdcMem = CreateCompatibleDC( 0 );      // Create memory DC for source bitmap
   if( !hdcMem )
   {
      return NULL;                        // Return NULL on failure
   }

   hdcMem2 = CreateCompatibleDC( 0 );     // Create memory DC for mask
   if( !hdcMem2 )
   {
      DeleteDC( hdcMem );                 // Clean up source DC
      return NULL;                        // Return NULL on failure
   }

   SelectObject( hdcMem, hbmColour );     // Select source bitmap into DC
   SelectObject( hdcMem2, hbmMask );      // Select mask bitmap into DC
   SetBkColor( hdcMem2, crTransparent );  // Set transparent color for mask
   BitBlt( hdcMem2, 0, 0, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCCOPY );   // Copy bitmap to mask
   BitBlt( hdcMem, 0, 0, bm.bmWidth, bm.bmHeight, hdcMem2, 0, 0, SRCINVERT ); // Invert mask onto source
   DeleteDC( hdcMem );     // Clean up source DC
   DeleteDC( hdcMem2 );    // Clean up mask DC
   return hbmMask;         // Return created mask
}

/* Calculate the check box position within the label */
void GetCheck( INSCHK *pbtn, RECT *rect )
{
   if( pbtn->lLeftCheck )  // If check box is on the left
   {
      rect->right = rect->left + pbtn->cxButton;                     // Adjust right edge
   }
   else
   {
      rect->left = rect->right - pbtn->cxButton;                     // Adjust left edge
   }

   if( pbtn->cxRightEdge > pbtn->cxLeftEdge )                        // Adjust for border asymmetry
   {
      OffsetRect( rect, pbtn->cxRightEdge - pbtn->cxLeftEdge, 0 );   // Shift rectangle
   }
}

/* Free memory allocated for check label structure */
static void FreeInsChk( PINSCHK pbtn )
{
   if( !pbtn )             // Check if structure exists
   {
      return;
   }

   if( pbtn->himage )      // Free checked state bitmap
   {
      DeleteObject( pbtn->himage );
   }

   if( pbtn->himagemask )  // Free checked state mask
   {
      DeleteObject( pbtn->himagemask );
   }

   if( pbtn->himage2 )     // Free unchecked state bitmap
   {
      DeleteObject( pbtn->himage2 );
   }

   if( pbtn->himagemask2 ) // Free unchecked state mask
   {
      DeleteObject( pbtn->himagemask2 );
   }

   HeapFree( GetProcessHeap(), 0, pbtn ); // Free structure memory
}

/* Initialize check box in label control */
BOOL InsertCheck( HWND hWnd, HBITMAP himage, HBITMAP himage2, int BtnWidth, BOOL lCheck, BOOL lLeftCheck )
{
   INSCHK   *pbtn = ( INSCHK * ) GetWindowLongPtr( hWnd, GWLP_USERDATA );  // Get existing structure

   // Free existing structure if it exists
   if( pbtn )
   {
      if( pbtn->himage )                     // Free checked state bitmap
      {
         DeleteObject( pbtn->himage );
      }

      if( pbtn->himagemask )                 // Free checked state mask
      {
         DeleteObject( pbtn->himagemask );
      }

      if( pbtn->himage2 )                    // Free unchecked state bitmap
      {
         DeleteObject( pbtn->himage2 );
      }

      if( pbtn->himagemask2 )                // Free unchecked state mask
      {
         DeleteObject( pbtn->himagemask2 );
      }

      HeapFree( GetProcessHeap(), 0, pbtn ); // Free structure
      SetWindowLongPtr( hWnd, GWLP_USERDATA, ( LONG_PTR ) NULL ); // Clear user data
   }

   pbtn = ( INSCHK * ) HeapAlloc( GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof( INSCHK ) ); // Allocate new structure
   if( !pbtn )
   {
      return FALSE;                 // Return FALSE on allocation failure
   }

   pbtn->lCheck = lCheck;           // Set checked state
   pbtn->lLeftCheck = lLeftCheck;   // Set check box position flag
   pbtn->cxButton = BtnWidth;       // Set button width
   if( pbtn->cxButton < 0 )         // Validate button width
   {
      pbtn->cxButton = GetSystemMetrics( SM_CXVSCROLL ); // Use default width
   }

   pbtn->himage = himage;     // Store checked state bitmap
   pbtn->himage2 = himage2;   // Store unchecked state bitmap
   pbtn->cxSpace = GetSystemMetrics( SM_CXSIZEFRAME ) / 4;  // Set spacing
   if( himage != NULL )          // Create mask for checked state
   {
      pbtn->himagemask = CreateBitmapMask( himage, RGB( 0, 0, 0 ) );
      if( !pbtn->himagemask )
      {
         FreeInsChk( pbtn );     // Free structure on failure
         return FALSE;
      }
   }
   else
   {
      pbtn->himagemask = NULL;   // No mask if no image
   }

   if( himage2 != NULL )         // Create mask for unchecked state
   {
      pbtn->himagemask2 = CreateBitmapMask( himage2, RGB( 0, 0, 0 ) );
      if( !pbtn->himagemask2 )
      {
         FreeInsChk( pbtn );     // Free structure on failure
         return FALSE;
      }
   }
   else
   {
      pbtn->himagemask2 = NULL;  // No mask if no image
   }

   SetWindowLongPtr( hWnd, GWLP_USERDATA, ( LONG_PTR ) pbtn ); // Associate structure with window

   // Force non-client area update
   SetWindowPos( hWnd, 0, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOZORDER );

   return TRUE;            // Return success
}

/* Draw check mark or image in the label */
static void DrawCheck( HWND hWnd, INSCHK *pbtn, RECT *prect )
{
   HBITMAP  hBitmap;       // Bitmap to draw
   HBITMAP  hBitmapMask;   // Corresponding mask
   HDC      hdc = GetWindowDC( hWnd ); // Get window DC
   if( !hdc )
   {
      return;  // Exit if DC unavailable
   }

   hBitmap = pbtn->lCheck ? pbtn->himage : pbtn->himage2;               // Select bitmap based on state
   hBitmapMask = pbtn->lCheck ? pbtn->himagemask : pbtn->himagemask2;   // Select mask
   if( hBitmap == NULL )   // Draw text if no bitmap
   {
      FillRect( hdc, prect, GetSysColorBrush( COLOR_WINDOW ) );   // Fill background
      SetBkMode( hdc, TRANSPARENT );   // Set transparent background mode
      if( pbtn->lCheck )
      {
         DrawText( hdc, TEXT( "V" ), 1, prect, DT_CENTER | DT_VCENTER | DT_SINGLELINE );  // Draw check mark
      }
      else
      {
         DrawText( hdc, TEXT( " " ), 1, prect, DT_CENTER | DT_VCENTER | DT_SINGLELINE );  // Draw space
      }
   }
   else
   {
      int      wRow = prect->top;                                                // Top position
      int      wCol = prect->left;                                               // Left position
      HBITMAP  hbmOld;                                                           // Previous bitmap handle
      BITMAP   bm;                                                               // Bitmap information
      HDC      hdcMem = CreateCompatibleDC( hdc );                               // Create memory DC
      if( !hdcMem )
      {
         ReleaseDC( hWnd, hdc );                                                 // Release window DC
         return;
      }

      hbmOld = ( HBITMAP ) SelectObject( hdcMem, hBitmapMask );                  // Select mask into DC
      GetObject( hBitmap, sizeof( bm ), &bm );                                   // Get bitmap properties
      BitBlt( hdc, wCol, wRow, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCAND );  // Apply mask
      SelectObject( hdcMem, hBitmap );    // Select bitmap into DC
      BitBlt( hdc, wCol, wRow, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCPAINT );   // Draw bitmap
      SelectObject( hdcMem, hbmOld );     // Restore original bitmap
      DeleteDC( hdcMem );                 // Clean up memory DC
   }

   ReleaseDC( hWnd, hdc );                // Release window DC
}

/* Harbour function to create check label control */
HB_FUNC( INITCHKLABEL )
{
   HWND     hbutton;                      // Window handle for control
   HBITMAP  himage = NULL;                // Bitmap for checked state
   HBITMAP  himage2 = NULL;               // Bitmap for unchecked state
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );  // Window caption (ANSI)
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Convert to Unicode
#endif
   int      BtnWidth = hb_parni( 7 );     // Button width parameter
   DWORD    Style = WS_CHILD | SS_NOTIFY; // Base window styles
   DWORD    ExStyle = hb_parl( 12 ) ? WS_EX_CLIENTEDGE : 0; // Extended style for client edge
   if( hb_parl( 11 ) )           // Add border style
   {
      Style |= WS_BORDER;
   }

   if( hb_parl( 13 ) )           // Add horizontal scroll style
   {
      Style |= WS_HSCROLL;
   }

   if( hb_parl( 14 ) )           // Add vertical scroll style
   {
      Style |= WS_VSCROLL;
   }

   if( hb_parl( 15 ) )           // Add transparent style
   {
      ExStyle |= WS_EX_TRANSPARENT;
   }

   if( !hb_parl( 16 ) )          // Add visible style if not hidden
   {
      Style |= WS_VISIBLE;
   }

   if( hb_parl( 17 ) )           // Align text right
   {
      Style |= ES_RIGHT;
   }

   if( hb_parl( 18 ) )           // Center text
   {
      Style |= ES_CENTER;
   }

   if( hb_parl( 23 ) )           // Center image vertically
   {
      Style |= SS_CENTERIMAGE;
   }

   hbutton = CreateWindowEx      // Create the static control
   ( ExStyle,                    // Extended window styles
      WC_STATIC,                 // Static control class
      lpWindowName,              // Window caption
      Style,                     // Window styles
      hb_parni( 4 ),             // X position
      hb_parni( 5 ),             // Y position
      hb_parni( 6 ),             // Width
      hb_parni( 7 ),             // Height
      hmg_par_raw_HWND( 1 ),     // Parent window handle
      hmg_par_raw_HMENU( 3 ),    // Menu handle
      GetInstance(),             // Instance handle
      NULL                       // No creation parameters
   );

   if( !hbutton )                // Check for creation failure
   {
#ifdef UNICODE
      hb_xfree( lpWindowName );  // Free Unicode string
#endif
      hb_ret();                  // Return on failure
      return;
   }

   if( hb_parc( 19 ) != NULL )   // Load checked state image
   {
      himage = HMG_LoadPicture( hb_parc( 19 ), -1, -1, NULL, 0, 0, -1, 0, HB_FALSE, 255 );
   }

   if( hb_parc( 20 ) != NULL )   // Load unchecked state image
   {
      himage2 = HMG_LoadPicture( hb_parc( 20 ), -1, -1, NULL, 0, 0, -1, 0, HB_FALSE, 255 );
   }

   if( !InsertCheck( hbutton, himage, himage2, BtnWidth, hb_parl( 22 ), hb_parl( 21 ) ) ) // Initialize check box
   {
      if( himage )               // Free checked state image
      {
         DeleteObject( himage );
      }

      if( himage2 )              // Free unchecked state image
      {
         DeleteObject( himage2 );
      }

      DestroyWindow( hbutton );  // Destroy control on failure
#ifdef UNICODE
      hb_xfree( lpWindowName );  // Free Unicode string
#endif
      hb_ret();                  // Return on failure
      return;
   }

   LabelOldWndProc = SubclassWindow1( hbutton, ChkLabelFunc ); // Subclass window procedure
   SetWindowPos( hbutton, 0, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOZORDER );  // Update non-client area
   hmg_ret_raw_HWND( hbutton );           // Return control handle
#ifdef UNICODE
   hb_xfree( lpWindowName );              // Free Unicode string
#endif
}

/* Harbour function to set check label state */
HB_FUNC( SETCHKLABEL )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 ); // Get window handle
   RECT     rect; // Rectangle for drawing
   INSCHK   *pbtn = ( INSCHK * ) GetWindowLongPtr( hWnd, GWLP_USERDATA );  // Get control data
   if( !pbtn )
   {
      return;                    // Exit if no data
   }

   pbtn->lCheck = hb_parl( 2 );  // Set checked state
   GetWindowRect( hWnd, &rect ); // Get window rectangle
   OffsetRect( &rect, -rect.left, -rect.top );  // Normalize coordinates
   ShowWindow( hWnd, SW_HIDE );                 // Hide window
   InvalidateRect( hWnd, &rect, TRUE );         // Invalidate rectangle for redraw
   GetCheck( pbtn, &rect );                     // Calculate check box position
   DrawCheck( hWnd, pbtn, &rect );              // Draw check mark or image
   ShowWindow( hWnd, SW_SHOW );                 // Show window
}

/* Harbour function to get check label state */
HB_FUNC( GETCHKLABEL )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );       // Get window handle
   INSCHK   *pbtn = ( INSCHK * ) GetWindowLongPtr( hWnd, GWLP_USERDATA );  // Get control data
   if( !pbtn )
   {
      hb_retl( FALSE );                // Return FALSE if no data
      return;
   }

   hb_retl( ( BOOL ) pbtn->lCheck );   // Return checked state
}

/* Harbour function to replace check label images */
HB_FUNC( REPLACECHECKIMAGE )
{
   HBITMAP  himage = NULL;             // New checked state image
   HBITMAP  himage2 = NULL;            // New unchecked state image
   if( hb_parc( 2 ) != NULL )          // Load new checked state image
   {
      himage = HMG_LoadPicture( hb_parc( 2 ), -1, -1, NULL, 0, 0, -1, 0, HB_FALSE, 255 );
   }

   if( hb_parc( 3 ) != NULL )          // Load new unchecked state image
   {
      himage2 = HMG_LoadPicture( hb_parc( 3 ), -1, -1, NULL, 0, 0, -1, 0, HB_FALSE, 255 );
   }

   if( !InsertCheck( ( HWND ) HB_PARNL( 1 ), himage, himage2, hb_parni( 4 ), hb_parl( 5 ), hb_parl( 6 ) ) ) // Update images
   {
      if( himage )                     // Free checked state image on failure
      {
         DeleteObject( himage );
      }

      if( himage2 )                    // Free unchecked state image on failure
      {
         DeleteObject( himage2 );
      }
   }
}

/* Call Harbour event handler for label events */
static void CallLabelEvent( HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB   pSymbol = NULL;   // Symbol for event handler
   if( !pSymbol )                      // Initialize symbol if not set
   {
      pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OLABELEVENTS" ) );
   }

   if( pSymbol )                       // If symbol exists, call Harbour function
   {
      hb_vmPushSymbol( pSymbol );      // Push symbol to stack
      hb_vmPushNil();                  // Push NIL as object
      hb_vmPushNumInt( ( HB_PTRUINT ) hWnd );   // Push window handle
      hb_vmPushLong( Msg );                     // Push message
      hb_vmPushNumInt( wParam );                // Push wParam
      hb_vmPushNumInt( lParam );                // Push lParam
      hb_vmDo( 4 );                    // Execute with 4 parameters
   }
}

/* Window procedure for custom check label control */
LRESULT APIENTRY ChkLabelFunc( HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   LRESULT  r;                   // Return value
   INSCHK   *pbtn = ( INSCHK * ) GetWindowLongPtr( hWnd, GWLP_USERDATA );  // Get control data
   switch( Msg )                 // Handle messages
   {
      case WM_NCDESTROY:         // Window is being destroyed
         {
            FreeInsChk( pbtn );  // Free control structure
            SetWindowLongPtr( hWnd, GWLP_USERDATA, 0 );  // Clear user data
         }
         break;

      case WM_NCCALCSIZE:              // Calculate non-client area
         {
            RECT  *prect;              // Rectangle pointer
            RECT  oldrect;             // Original rectangle
            if( !pbtn )                // Check if control data exists
            {
               break;
            }

            prect = ( RECT * ) lParam; // Get rectangle from lParam
            oldrect = *prect;          // Save original rectangle
            CallWindowProc( LabelOldWndProc, hWnd, Msg, wParam, lParam );  // Call original procedure
            SendMessage( hWnd, WM_SETREDRAW, 1, 0 );              // Enable redraw
            if( pbtn->lLeftCheck )                                // Adjust for left-aligned check box
            {
               pbtn->cxLeftEdge = prect->left - oldrect.left;     // Calculate left edge
               pbtn->cxRightEdge = oldrect.right - prect->right;  // Calculate right edge
               prect->left += pbtn->cxButton + pbtn->cxSpace;     // Adjust left position
            }
            else  // Adjust for right-aligned check box
            {
               pbtn->cxLeftEdge = prect->left - oldrect.left;     // Calculate left edge
               pbtn->cxRightEdge = oldrect.right - prect->right;  // Calculate right edge
               prect->right -= pbtn->cxButton + pbtn->cxSpace;    // Adjust right position
            }

            return 0;   // Return success
         }

      case WM_NCPAINT:  // Paint non-client area
         CallWindowProc( LabelOldWndProc, hWnd, Msg, wParam, lParam );  // Call original procedure
         if( pbtn )     // If control data exists
         {
            RECT  rect; // Rectangle for drawing
            GetWindowRect( hWnd, &rect );                // Get window rectangle
            OffsetRect( &rect, -rect.left, -rect.top );  // Normalize coordinates
            GetCheck( pbtn, &rect );                     // Calculate check box position
            DrawCheck( hWnd, pbtn, &rect );              // Draw check mark or image
         }

         return 0;                  // Return success

      case WM_MOUSEMOVE:            // Handle mouse movement
         {
            TRACKMOUSEEVENT   tme;  // Track mouse event structure
            tme.cbSize = sizeof( TRACKMOUSEEVENT );   // Set structure size
            tme.dwFlags = TME_LEAVE;                  // Track mouse leave event
            tme.hwndTrack = hWnd;                     // Set window handle
            tme.dwHoverTime = HOVER_DEFAULT;          // Use default hover time
            _TrackMouseEvent( &tme );                 // Register mouse tracking
         }

         CallLabelEvent( hWnd, Msg, wParam, lParam ); // Call event handler
         r = hmg_par_LRESULT( -1 );                   // Get return value from Harbour
         return( r != 0 ) ? r : CallWindowProc( LabelOldWndProc, hWnd, 0, 0, 0 );   // Return or call original

      case WM_MOUSELEAVE:  // Handle mouse leave
         CallLabelEvent( hWnd, Msg, wParam, lParam ); // Call event handler
         r = hmg_par_LRESULT( -1 );                   // Get return value from Harbour
         return( r != 0 ) ? r : CallWindowProc( LabelOldWndProc, hWnd, 0, 0, 0 );   // Return or call original
   }

   return CallWindowProc( LabelOldWndProc, hWnd, Msg, wParam, lParam );             // Pass unhandled messages to original procedure
}
