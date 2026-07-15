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
	Copyright 1999-2026, https://harbour.github.io/

	"WHAT32"
	Copyright 2002 AJ Wos <andrwos@aust1.net> 

	"HWGUI"
  	Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>
 ---------------------------------------------------------------------------*/

#include <mgdefs.h>
#include <commctrl.h>

// Compatibility fix for legacy Borland compilers that do not define WC_STATIC.
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_STATIC "Static"
#endif
#include "hbapiitm.h"
#include "hbvm.h"

// Forward declaration of the subclassed window procedure for the CheckLabel control.
LRESULT APIENTRY  ChkLabelFunc( HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam );

// Stores the original window procedure of the Static control to allow message chaining (CallWindowProc).
static WNDPROC    LabelOldWndProc;

// External HMG function to handle image loading with support for scaling and transparency.
extern HBITMAP    HMG_LoadPicture
                  (
                     const char  *FileName,
                     int         New_Width,
                     int         New_Height,
                     HWND        hWnd,
                     int         ScaleStretch,
                     int         Transparent,
                     long        BackgroundColor,
                     int         AdjustImage,
                     HB_BOOL     bAlphaFormat,
                     int         iAlpfaConstant
                  );

#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );
#endif
HINSTANCE         GetInstance( void );
HINSTANCE         GetResources( void );

/*
   INSCHK Structure
   Internal state container for the CheckLabel control.
   This structure is attached to the Win32 window via GWLP_USERDATA.
*/
typedef struct
{
   BOOL     lCheck;                    // Current toggle state (Checked/Unchecked).
   int      cxLeftEdge, cxRightEdge;   // Calculated widths of the non-client borders.
   int      cxButton;                  // Width allocated for the checkbox/image.
   int      cxSpace;                   // Horizontal padding between the checkbox and the label text.
   BOOL     lLeftCheck;                // Positioning flag: TRUE for left-aligned, FALSE for right-aligned.
   HBITMAP  himage;                    // Bitmap handle for the 'Checked' state.
   HBITMAP  himagemask;                // Transparency mask for the 'Checked' bitmap.
   HBITMAP  himage2;                   // Bitmap handle for the 'Unchecked' state.
   HBITMAP  himagemask2;               // Transparency mask for the 'Unchecked' bitmap.
} INSCHK, *PINSCHK;

/*
   Function: CreateBitmapMask
   Purpose:  Generates a monochrome mask bitmap from a color bitmap based on a transparent color.
   Input:    hbmColour (Source bitmap), crTransparent (Color to treat as transparent).
   Returns:  HBITMAP (The generated mask) or NULL on failure.
   Logic:    Uses BitBlt with SRCCOPY and SRCINVERT to isolate the background color, 
             enabling transparent drawing on DCs that don't support alpha channels natively.
*/
HBITMAP CreateBitmapMask( HBITMAP hbmColour, COLORREF crTransparent )
{
   HDC      hdcMem, hdcMem2;
   HBITMAP  hbmMask;
   BITMAP   bm;

   GetObject( hbmColour, sizeof( BITMAP ), &bm );
   hbmMask = CreateBitmap( bm.bmWidth, bm.bmHeight, 1, 1, NULL );

   hdcMem = CreateCompatibleDC( 0 );
   if( !hdcMem )
   {
      return NULL;
   }

   hdcMem2 = CreateCompatibleDC( 0 );
   if( !hdcMem2 )
   {
      DeleteDC( hdcMem );
      return NULL;
   }

   SelectObject( hdcMem, hbmColour );
   SelectObject( hdcMem2, hbmMask );

   // Setting the background color of the source DC determines which color becomes 'white' in the mono mask.
   SetBkColor( hdcMem2, crTransparent );
   BitBlt( hdcMem2, 0, 0, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCCOPY );

   // Invert the source bitmap against the mask to prepare for SRCPAINT operations.
   BitBlt( hdcMem, 0, 0, bm.bmWidth, bm.bmHeight, hdcMem2, 0, 0, SRCINVERT );

   DeleteDC( hdcMem );
   DeleteDC( hdcMem2 );

   return hbmMask;
}

/*
   Function: GetCheck
   Purpose:  Calculates the bounding rectangle where the checkbox/image should be drawn.
   Input:    pbtn (Control state), rect (Window rectangle to be modified).
   Logic:    Adjusts the input rectangle based on whether the checkbox is on the left or right 
             side of the control, accounting for non-client edge offsets.
*/
void GetCheck( INSCHK *pbtn, RECT *rect )
{
   if( pbtn->lLeftCheck )
   {
      rect->right = rect->left + pbtn->cxButton;
   }
   else
   {
      rect->left = rect->right - pbtn->cxButton;
   }

   // Adjust for asymmetrical borders (e.g., specific themes or styles).
   if( pbtn->cxRightEdge > pbtn->cxLeftEdge )
   {
      OffsetRect( rect, pbtn->cxRightEdge - pbtn->cxLeftEdge, 0 );
   }
}

/*
   Function: FreeInsChk
   Purpose:  Performs memory cleanup for the INSCHK structure and its GDI objects.
   Input:    pbtn (Pointer to the structure to free).
   Side Effects: Deletes GDI bitmaps and masks from memory to prevent leaks.
*/
static void FreeInsChk( PINSCHK pbtn )
{
   if( !pbtn )
   {
      return;
   }

   if( pbtn->himage )
   {
      DeleteObject( pbtn->himage );
   }

   if( pbtn->himagemask )
   {
      DeleteObject( pbtn->himagemask );
   }

   if( pbtn->himage2 )
   {
      DeleteObject( pbtn->himage2 );
   }

   if( pbtn->himagemask2 )
   {
      DeleteObject( pbtn->himagemask2 );
   }

   HeapFree( GetProcessHeap(), 0, pbtn );
}

/*
   Function: InsertCheck
   Purpose:  Initializes or updates the internal state of a CheckLabel control.
   Input:    hWnd (Control handle), himage/himage2 (Bitmaps), BtnWidth (Size), 
             lCheck (Initial state), lLeftCheck (Alignment).
   Returns:  BOOL (Success/Failure).
   Logic:    Allocates the INSCHK structure and calculates transparency masks. 
             It uses SetWindowPos with SWP_FRAMECHANGED to force the OS to recalculate 
             the non-client area via WM_NCCALCSIZE.
*/
BOOL InsertCheck( HWND hWnd, HBITMAP himage, HBITMAP himage2, int BtnWidth, BOOL lCheck, BOOL lLeftCheck )
{
   // Retrieve existing data to clean up if this is a re-initialization.
   INSCHK   *pbtn = ( INSCHK * ) GetWindowLongPtr( hWnd, GWLP_USERDATA );

   if( pbtn )
   {
      FreeInsChk( pbtn );
      SetWindowLongPtr( hWnd, GWLP_USERDATA, ( LONG_PTR ) NULL );
   }

   pbtn = ( INSCHK * ) HeapAlloc( GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof( INSCHK ) );
   if( !pbtn )
   {
      return FALSE;
   }

   pbtn->lCheck = lCheck;
   pbtn->lLeftCheck = lLeftCheck;
   pbtn->cxButton = BtnWidth;

   // Fallback to system default scrollbar width if no specific width is provided.
   if( pbtn->cxButton < 0 )
   {
      pbtn->cxButton = GetSystemMetrics( SM_CXVSCROLL );
   }

   pbtn->himage = himage;
   pbtn->himage2 = himage2;
   pbtn->cxSpace = GetSystemMetrics( SM_CXSIZEFRAME ) / 4;

   // Generate masks for both states to support transparent rendering.
   if( himage != NULL )
   {
      pbtn->himagemask = CreateBitmapMask( himage, RGB( 0, 0, 0 ) );
      if( !pbtn->himagemask )
      {
         FreeInsChk( pbtn );
         return FALSE;
      }
   }
   else
   {
      pbtn->himagemask = NULL;         // No mask if no image
   }

   if( himage2 != NULL )
   {
      pbtn->himagemask2 = CreateBitmapMask( himage2, RGB( 0, 0, 0 ) );
      if( !pbtn->himagemask2 )
      {
         FreeInsChk( pbtn );
         return FALSE;
      }
   }
   else
   {
      pbtn->himagemask2 = NULL;        // No mask if no image
   }

   SetWindowLongPtr( hWnd, GWLP_USERDATA, ( LONG_PTR ) pbtn );

   // Trigger non-client area recalculation.
   SetWindowPos( hWnd, 0, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOZORDER );

   return TRUE;
}

/*
   Function: DrawCheck
   Purpose:  Renders the checkbox or custom image onto the window's Device Context.
   Input:    hWnd (Control handle), pbtn (State data), prect (Target rectangle).
   Logic:    If no bitmaps are provided, it draws a simple text-based "V" or space.
             If bitmaps exist, it performs a masked BitBlt (AND/PAINT) to achieve transparency.
             Note: Uses GetWindowDC because drawing occurs in the non-client area.
*/
static void DrawCheck( HWND hWnd, INSCHK *pbtn, RECT *prect )
{
   HBITMAP  hBitmap;
   HBITMAP  hBitmapMask;
   HDC      hdc = GetWindowDC( hWnd );

   if( !hdc )
   {
      return;
   }

   hBitmap = pbtn->lCheck ? pbtn->himage : pbtn->himage2;
   hBitmapMask = pbtn->lCheck ? pbtn->himagemask : pbtn->himagemask2;

   if( hBitmap == NULL )
   {
      // Default rendering: Text-based checkmark.
      FillRect( hdc, prect, GetSysColorBrush( COLOR_WINDOW ) );
      SetBkMode( hdc, TRANSPARENT );
      if( pbtn->lCheck )
      {
         DrawText( hdc, TEXT( "V" ), 1, prect, DT_CENTER | DT_VCENTER | DT_SINGLELINE );
      }
      else
      {
         DrawText( hdc, TEXT( " " ), 1, prect, DT_CENTER | DT_VCENTER | DT_SINGLELINE );  // Draw space
      }
   }
   else
   {
      // Graphical rendering: Masked bitmap.
      int      wRow = prect->top;
      int      wCol = prect->left;
      HBITMAP  hbmOld;
      BITMAP   bm;
      HDC      hdcMem = CreateCompatibleDC( hdc );

      if( !hdcMem )
      {
         ReleaseDC( hWnd, hdc );
         return;
      }

      // Step 1: Apply the mask using SRCAND (filters out the background).
      hbmOld = ( HBITMAP ) SelectObject( hdcMem, hBitmapMask );
      GetObject( hBitmap, sizeof( bm ), &bm );
      BitBlt( hdc, wCol, wRow, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCAND );

      // Step 2: Apply the image using SRCPAINT (paints the actual pixels).
      SelectObject( hdcMem, hBitmap );
      BitBlt( hdc, wCol, wRow, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCPAINT );

      SelectObject( hdcMem, hbmOld );
      DeleteDC( hdcMem );
   }

   ReleaseDC( hWnd, hdc );
}

/*
   HB_FUNC: INITCHKLABEL
   Purpose: Harbour-level constructor for the CheckLabel control.
   Params:  Various HMG control properties (Parent, Caption, Coords, Styles, Images, etc.).
   Returns: HWND of the created control.
   Logic:   Creates a standard Win32 Static control with SS_NOTIFY, then subclasses it 
            to inject checkbox behavior and custom non-client area drawing.
*/
HB_FUNC( INITCHKLABEL )
{
   HWND     hbutton;
   HBITMAP  himage = NULL;
   HBITMAP  himage2 = NULL;
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   int      BtnWidth = hb_parni( 7 );

   // Define base styles. SS_NOTIFY is required for the Static control to receive mouse clicks.
   DWORD    Style = WS_CHILD | SS_NOTIFY;
   DWORD    ExStyle = hb_parl( 12 ) ? WS_EX_CLIENTEDGE : 0;

   // Map Harbour logical parameters to Win32 Window Styles.
   if( hb_parl( 11 ) )
   {
      Style |= WS_BORDER;
   }

   if( hb_parl( 13 ) )
   {
      Style |= WS_HSCROLL;
   }

   if( hb_parl( 14 ) )
   {
      Style |= WS_VSCROLL;
   }

   if( hb_parl( 15 ) )
   {
      ExStyle |= WS_EX_TRANSPARENT;
   }

   if( !hb_parl( 16 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( hb_parl( 17 ) )
   {
      Style |= ES_RIGHT;
   }

   if( hb_parl( 18 ) )
   {
      Style |= ES_CENTER;
   }

   if( hb_parl( 23 ) )
   {
      Style |= SS_CENTERIMAGE;
   }

   hbutton = CreateWindowEx
      (
         ExStyle,
         WC_STATIC,
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
      );

   if( !hbutton )
   {
#ifdef UNICODE
      hb_xfree( lpWindowName );
#endif
      hb_ret();
      return;
   }

   // Load custom images if paths are provided in parameters 19 and 20.
   if( hb_parc( 19 ) != NULL )
   {
      himage = HMG_LoadPicture( hb_parc( 19 ), -1, -1, NULL, 0, 0, -1, 0, HB_FALSE, 255 );
   }

   if( hb_parc( 20 ) != NULL )
   {
      himage2 = HMG_LoadPicture( hb_parc( 20 ), -1, -1, NULL, 0, 0, -1, 0, HB_FALSE, 255 );
   }

   // Initialize the internal structure.
   if( !InsertCheck( hbutton, himage, himage2, BtnWidth, hb_parl( 22 ), hb_parl( 21 ) ) )
   {
      if( himage )
      {
         DeleteObject( himage );
      }

      if( himage2 )
      {
         DeleteObject( himage2 );
      }

      DestroyWindow( hbutton );
#ifdef UNICODE
      hb_xfree( lpWindowName );
#endif
      hb_ret();
      return;
   }

   // Subclass the control to intercept painting and sizing messages.
   LabelOldWndProc = SubclassWindow1( hbutton, ChkLabelFunc );

   // Force a frame update to ensure the checkbox space is reserved immediately.
   SetWindowPos( hbutton, 0, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOZORDER );

   hmg_ret_raw_HWND( hbutton );
#ifdef UNICODE
   hb_xfree( lpWindowName );
#endif
}

/*
   HB_FUNC: SETCHKLABEL
   Purpose: Updates the checked state of the control from Harbour.
   Params:  1: HWND, 2: Logical (Checked state).
   Logic:   Updates the internal structure and forces a redraw of the non-client area.
*/
HB_FUNC( SETCHKLABEL )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   RECT     rect;
   INSCHK   *pbtn = ( INSCHK * ) GetWindowLongPtr( hWnd, GWLP_USERDATA );

   if( !pbtn )
   {
      return;
   }

   pbtn->lCheck = hb_parl( 2 );

   GetWindowRect( hWnd, &rect );
   OffsetRect( &rect, -rect.left, -rect.top );

   // Redraw sequence: Hide/Invalidate/Draw/Show ensures a clean visual update.
   ShowWindow( hWnd, SW_HIDE );
   InvalidateRect( hWnd, &rect, TRUE );
   GetCheck( pbtn, &rect );
   DrawCheck( hWnd, pbtn, &rect );
   ShowWindow( hWnd, SW_SHOW );
}

/*
   HB_FUNC: GETCHKLABEL
   Purpose: Retrieves the current checked state.
   Params:  1: HWND.
   Returns: Logical.
*/
HB_FUNC( GETCHKLABEL )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   INSCHK   *pbtn = ( INSCHK * ) GetWindowLongPtr( hWnd, GWLP_USERDATA );

   if( !pbtn )
   {
      hb_retl( FALSE );
      return;
   }

   hb_retl( ( BOOL ) pbtn->lCheck );
}

/*
   HB_FUNC: REPLACECHECKIMAGE
   Purpose: Dynamically changes the bitmaps used for the checkbox states.
   Params:  1: HWND, 2: Path Checked, 3: Path Unchecked, 4: Width, 5: State, 6: Alignment.
*/
HB_FUNC( REPLACECHECKIMAGE )
{
   HBITMAP  himage = NULL;
   HBITMAP  himage2 = NULL;

   if( hb_parc( 2 ) != NULL )
   {
      himage = HMG_LoadPicture( hb_parc( 2 ), -1, -1, NULL, 0, 0, -1, 0, HB_FALSE, 255 );
   }

   if( hb_parc( 3 ) != NULL )
   {
      himage2 = HMG_LoadPicture( hb_parc( 3 ), -1, -1, NULL, 0, 0, -1, 0, HB_FALSE, 255 );
   }

   if( !InsertCheck( ( HWND ) HB_PARNL( 1 ), himage, himage2, hb_parni( 4 ), hb_parl( 5 ), hb_parl( 6 ) ) )
   {
      if( himage )
      {
         DeleteObject( himage );
      }

      if( himage2 )
      {
         DeleteObject( himage2 );
      }
   }
}

/*
   Function: CallLabelEvent
   Purpose:  Routes Win32 messages to a Harbour-level event handler function.
   Input:    hWnd, Msg, wParam, lParam.
   Logic:    Looks for a Harbour function named "OLABELEVENTS" and executes it via the VM.
             This allows HMG users to handle low-level events in PRG code.
*/
static void CallLabelEvent( HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB   pSymbol = NULL;

   if( !pSymbol )
   {
      pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OLABELEVENTS" ) );
   }

   if( pSymbol )
   {
      hb_vmPushSymbol( pSymbol );
      hb_vmPushNil();
      hb_vmPushNumInt( ( HB_PTRUINT ) hWnd );
      hb_vmPushLong( Msg );
      hb_vmPushNumInt( wParam );
      hb_vmPushNumInt( lParam );
      hb_vmDo( 4 );
   }
}

/*
   Function: ChkLabelFunc
   Purpose:  Subclassed Window Procedure for the CheckLabel control.
   Logic:    Intercepts non-client messages to "steal" space from the label for the checkbox.
             - WM_NCCALCSIZE: Shrinks the client area so the label text doesn't overlap the checkbox.
             - WM_NCPAINT: Draws the checkbox in the newly created margin.
             - WM_NCDESTROY: Ensures memory cleanup when the window is closed.
*/
LRESULT APIENTRY ChkLabelFunc( HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   LRESULT  r;
   INSCHK   *pbtn = ( INSCHK * ) GetWindowLongPtr( hWnd, GWLP_USERDATA );

   switch( Msg )
   {
      case WM_NCDESTROY:
         {
            FreeInsChk( pbtn );
            SetWindowLongPtr( hWnd, GWLP_USERDATA, 0 );
         }
         break;

      case WM_NCCALCSIZE:
         {
            RECT  *prect;
            RECT  oldrect;

            if( !pbtn )
            {
               break;
            }

            prect = ( RECT * ) lParam;
            oldrect = *prect;

            // Let the original procedure calculate standard borders first.
            CallWindowProc( LabelOldWndProc, hWnd, Msg, wParam, lParam );

            // Enable redraw to prevent artifacts during resizing.
            SendMessage( hWnd, WM_SETREDRAW, 1, 0 );

            // Calculate the actual border thickness provided by the OS/Theme.
            pbtn->cxLeftEdge = prect->left - oldrect.left;
            pbtn->cxRightEdge = oldrect.right - prect->right;

            // Shrink the client area rectangle to reserve space for our custom drawing.
            if( pbtn->lLeftCheck )
            {
               prect->left += pbtn->cxButton + pbtn->cxSpace;
            }
            else
            {
               prect->right -= pbtn->cxButton + pbtn->cxSpace;
            }

            return 0;
         }

      case WM_NCPAINT:
         // Draw standard borders first.
         CallWindowProc( LabelOldWndProc, hWnd, Msg, wParam, lParam );

         if( pbtn )
         {
            RECT  rect;
            GetWindowRect( hWnd, &rect );
            OffsetRect( &rect, -rect.left, -rect.top );

            // Draw the checkbox in the non-client area reserved during WM_NCCALCSIZE.
            GetCheck( pbtn, &rect );
            DrawCheck( hWnd, pbtn, &rect );
         }

         return 0;

      case WM_MOUSEMOVE:
         {
            // Ensure we receive WM_MOUSELEAVE by tracking the mouse.
            TRACKMOUSEEVENT   tme;
            tme.cbSize = sizeof( TRACKMOUSEEVENT );
            tme.dwFlags = TME_LEAVE;
            tme.hwndTrack = hWnd;
            tme.dwHoverTime = HOVER_DEFAULT;
            _TrackMouseEvent( &tme );
         }

         CallLabelEvent( hWnd, Msg, wParam, lParam );
         r = hmg_par_LRESULT( -1 );
         return( r != 0 ) ? r : CallWindowProc( LabelOldWndProc, hWnd, 0, 0, 0 );

      case WM_MOUSELEAVE:
         CallLabelEvent( hWnd, Msg, wParam, lParam );
         r = hmg_par_LRESULT( -1 );
         return( r != 0 ) ? r : CallWindowProc( LabelOldWndProc, hWnd, 0, 0, 0 );
   }

   // Pass all other messages to the original Static control handler.
   return CallWindowProc( LabelOldWndProc, hWnd, Msg, wParam, lParam );
}
