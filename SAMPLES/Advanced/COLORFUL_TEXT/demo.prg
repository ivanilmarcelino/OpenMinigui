/*
 * Rendering multi-colored text on the form
 *
 * Adapted for MiniGUI by Grigory Filatov
 */

#include "hmg.ch"
#include "i_winuser.ch"

/*------------------------------------------------------------------------------*/
FUNCTION Main
/*------------------------------------------------------------------------------*/
/*
 * This is the main function of the application. It defines the main window,
 * sets up the font and colors, and activates the window.
 *
 * Purpose:
 *   This function serves as the entry point for the application. It initializes
 *   the main window, defines the visual elements (font, colors), and starts the
 *   application's event loop.  It demonstrates how to create a window and
 *   display multi-colored text within it.
 *
 * Notes:
 *   - The application uses a shuffled array of colors to display each character
 *     of the text in a different color.
 *   - The font "IMPACT" is used, and if it's not already defined, it will be created.
 *   - The window's ON PAINT event is handled by the App_OnPaint function, which
 *     is responsible for drawing the multi-colored text.
 */
   LOCAL cText := "MULTI-COLOR TEXT"
   LOCAL hFont

   LOCAL aColors := { ;
      METRO_LIME, METRO_GREEN, METRO_EMERALD, METRO_TEAL, METRO_CYAN, ;
      METRO_COBALT, METRO_INDIGO, METRO_VIOLET, METRO_PINK, METRO_MAGENTA, ;
      METRO_CRIMSON, METRO_RED, METRO_ORANGE, METRO_AMBER, METRO_YELLOW, ;
      METRO_BROWN, METRO_OLIVE, METRO_STEEL, METRO_MAUVE, METRO_TAUPE }

   // Colors randomly
   AShuffle( aColors )

   hFont := GetFontHandle( "Font_Text" )
   IF hFont == 0
      DEFINE FONT Font_Text FONTNAME "IMPACT" SIZE 48
   ENDIF

   hFont := GetFontHandle( "Font_Text" )

   SET EVENTS FUNCTION TO App_OnEvents

   DEFINE WINDOW Form_Main ;
         AT 0, 0 ;
         WIDTH 600 HEIGHT 400 ;
         TITLE cText ;
         MAIN ;
         ON SIZE InvalidateRect( This.Handle, 0 ) ;
         ON MAXIMIZE InvalidateRect( This.Handle, 0 ) ;
         ON PAINT App_OnPaint( This.Handle, hFont, cText, aColors )

      DEFINE STATUSBAR FONT "Arial" SIZE 12
         STATUSITEM "Color Text" BACKCOLOR HMG_n2RGB( GetSysColor( COLOR_GRADIENTINACTIVECAPTION ) )
      END STATUSBAR

      ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   CENTER WINDOW Form_Main

   ACTIVATE WINDOW Form_Main

RETURN NIL

#define DT_SINGLELINE  32
#define DT_CALCRECT    1024
/*------------------------------------------------------------------------------*/
FUNCTION App_OnPaint( hWnd, hFont, cText, aColors )
/*------------------------------------------------------------------------------*/
/*
 * This function is called when the window needs to be repainted. It draws the
 * multi-colored text on the window.
 *
 * Parameters:
 *   hWnd    : (HWND) Handle to the window.
 *   hFont   : (HFONT) Handle to the font to be used for drawing the text.
 *   cText   : (Character) The text string to be displayed.
 *   aColors : (Array) An array of color values (integers) to be used for each character.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function handles the WM_PAINT message for the main window. It retrieves
 *   the client area of the window, calculates the center position for the text,
 *   and then draws each character of the text in a different color from the
 *   provided color array. This creates the multi-colored text effect.
 *
 * Notes:
 *   - The function uses the DrawTextEx function (defined in the C section below)
 *     to draw the text with specified formatting and colors.
 *   - The SetBkMode function is used to set the background mode to transparent
 *     (OPAQUE = 0, TRANSPARENT = 1) so that the text is drawn without a background color.
 *   - The status bar text is also updated to be centered.
 */
   LOCAL aRect := { 0, 0, 0, 0 }
   LOCAL c, n, hDC, nRight, bk

   GetCliAreaRect( hWnd, aRect )

   // Centered Statusbar Text
   Form_Main.Statusbar.Item( 1 ) := PadC( "Color Text", aRect[ 4 ] / 4 - GetBorderWidth() )

   hDC := GetDC( hWnd )

   // Center Text
   n := DrawTextEx( hDC, cText, aRect, DT_SINGLELINE + DT_CALCRECT, hFont, 0, @nRight )
   aRect[ 1 ] += ( aRect[ 3 ] - aRect[ 1 ] - n - Form_Main.Statusbar.Height - 2 ) / 2
   aRect[ 2 ] += Int( ( aRect[ 4 ] - nRight - GetBorderWidth() ) / 2 )

   // Paint Text
   bk := SetBkMode( hDC, 1 )
   FOR n := 1 TO Len( cText )
      c := SubStr( cText, n, 1 )
      DrawTextEx( hDC, c, aRect, DT_SINGLELINE + DT_CALCRECT, hFont, 0, @nRight )
      DrawTextEx( hDC, c, aRect, DT_SINGLELINE, hFont, aColors[ n ] )
      aRect[ 2 ] := nRight
   NEXT
   SetBkMode( hDC, bk )

   ReleaseDC( hWnd, hDC )

RETURN NIL

/*------------------------------------------------------------------------------*/
FUNCTION App_OnEvents( hWnd, nMsg, wParam, lParam )
/*------------------------------------------------------------------------------*/
/*
 * This function is the event handler for the application. It handles various
 * window messages, such as WM_SIZE, and performs actions accordingly.
 *
 * Parameters:
 *   hWnd    : (HWND) Handle to the window that received the message.
 *   nMsg    : (Integer) The message code (e.g., WM_SIZE, WM_PAINT).
 *   wParam  : (Integer) Message-specific information.  Often used to pass flags or handles.
 *   lParam  : (Integer) Message-specific information.  Often used to pass pointers to data structures.
 *
 * Returns:
 *   nResult : (Integer) The result of the event handling.  The meaning of the result depends on the message.
 *
 * Purpose:
 *   This function acts as the central event handler for the application's main
 *   window. It intercepts Windows messages and performs actions based on the
 *   message type.  Specifically, it handles the WM_SIZE message to adjust the
 *   layout of controls within the window when the window is resized, maximized,
 *   or minimized.  It also calls the default HMG event handler (Events()) for
 *   messages that it doesn't explicitly handle.
 *
 * Notes:
 *   - The function iterates through the controls associated with the window and
 *     adjusts their positions and sizes as needed.
 *   - It also calls user-defined event procedures (e.g., _HMG_aFormSizeProcedure)
 *     associated with the window.
 *   - The function uses the _HMG_aControlHandles, _HMG_aControlParentHandles,
 *     and _HMG_aControlType arrays to access information about the controls.
 */
   LOCAL nResult
   LOCAL ControlCount, i, k, x

   SWITCH nMsg

   CASE WM_SIZE

      ControlCount := Len ( _HMG_aControlHandles )

      i := AScan ( _HMG_aFormHandles, hWnd )

      IF i > 0

            IF ( k := _HMG_aFormReBarHandle [i] ) > 0

               SizeRebar ( k )
               RebarHeight ( k )
               RedrawWindow ( k )

            ENDIF

            FOR x := 1 TO ControlCount

               IF _HMG_aControlParentHandles [x] == hWnd

                  IF _HMG_aControlType [x] == "MESSAGEBAR"

                     MoveWindow( _HMG_aControlHandles [x] , 0 , 0 , 0 , 0 , .T. )
                     RefreshItemBar ( _HMG_aControlHandles [x] , _GetStatusItemWidth( hWnd, 1 ) )

                     IF ( k := GetControlIndex( 'ProgressMessage', GetParentFormName( x ) ) ) != 0
                        RefreshProgressItem ( _HMG_aControlMiscData1 [k, 1], _HMG_aControlHandles [k], _HMG_aControlMiscData1 [k, 2] )
                     ENDIF
                     EXIT

                  ENDIF

               ENDIF

            NEXT x

            IF _HMG_MainActive == .T.

               IF wParam == SIZE_MAXIMIZED

                  _DoWindowEventProcedure ( _HMG_aFormMaximizeProcedure [i], i )

                  IF _HMG_AutoAdjust .AND. _HMG_MainClientMDIHandle == 0
                     _Autoadjust( hWnd )
                  ENDIF

               ELSEIF wParam == SIZE_MINIMIZED

                  _DoWindowEventProcedure ( _HMG_aFormMinimizeProcedure [i], i )

               ELSEIF wParam == SIZE_RESTORED .AND. !IsWindowSized( hWnd )

                  _DoWindowEventProcedure ( _HMG_aFormRestoreProcedure [i], i )

               ELSE

                  _DoWindowEventProcedure ( _HMG_aFormSizeProcedure [i], i )

                  IF _HMG_AutoAdjust .AND. _HMG_MainClientMDIHandle == 0
                     _Autoadjust( hWnd )
                  ENDIF

               ENDIF

            ENDIF

      ENDIF

      FOR i := 1 TO ControlCount

         IF _HMG_aControlParentHandles [i] == hWnd

            IF _HMG_aControlType [i] == "TOOLBAR"
               SendMessage ( _HMG_aControlHandles [i], TB_AUTOSIZE, 0, 0 )
            ENDIF

         ENDIF

      NEXT i

      nResult := 0
      EXIT

   OTHERWISE
      nResult := Events( hWnd, nMsg, wParam, lParam )

   END SWITCH

RETURN nResult

/*------------------------------------------------------------------------------*/
FUNCTION AShuffle( aArray )
/*------------------------------------------------------------------------------*/
/*
 * This function shuffles the elements of an array in a random order.
 *
 * Parameters:
 *   aArray : (Array) The array to be shuffled.  This array is passed by reference,
 *            so the original array will be modified.
 *
 * Returns:
 *   aArray : (Array) The shuffled array.  This is the same array that was passed
 *            as input, but with its elements rearranged in a random order.
 *
 * Purpose:
 *   This function implements a shuffling algorithm to randomize the order of
 *   elements within an array.  It's used in this application to randomize the
 *   order of colors used to display the multi-colored text.  This ensures that
 *   each character is displayed in a different, randomly selected color.
 *
 * Notes:
 *   - The function uses a temporary array 'a' to keep track of the indices that
 *     have already been used.
 *   - The function modifies the original array directly (passed by reference).
 */
   LOCAL n, i, j, a := {}

   IF ( n := Len( aArray ) ) > 1

      FOR i := 1 TO n

         REPEAT
            j := Random( n )
            IF AScan( a, j ) == 0
               AAdd( a, j )
            ENDIF
         UNTIL Len( a ) < i

         j := aArray[ i ]
         aArray[ i ] := aArray[ a[ i ] ]
         aArray[ a[ i ] ] := j

      NEXT i

   ENDIF

RETURN aArray

/*------------------------------------------------------------------------------*
 * Low Level C Routines
 *------------------------------------------------------------------------------*/

#pragma BEGINDUMP

#include <mgdefs.h>

RECT *Param2Rect( int iParam, RECT *prct )
/*
   Converts a Harbour array parameter to a RECT structure.
 
   Parameters:
       iParam - The index of the Harbour array parameter (1-based).
       prct   - A pointer to a RECT structure.  This structure will be populated
                with the values from the Harbour array.
 
   Return:
       A pointer to the same RECT structure that was passed as input (prct).
 
   Purpose:
       This function bridges the gap between Harbour's array data type and the
       Windows API's RECT structure.  It allows Harbour code to easily pass
       rectangle coordinates to Windows functions that expect a RECT structure.
 
   Notes:
       - The Harbour array is expected to contain four numeric elements: top, left,
         bottom, and right, in that order.
       - If the array parameter is missing or invalid, default values are assigned
         to the RECT structure.
*/
{
   if( hb_pcount() >= iParam && HB_ISARRAY( iParam ) )
   {
      prct->top = hb_parvnl( iParam, 1 );
      prct->left = hb_parvnl( iParam, 2 );
      prct->bottom = hb_parvnl( iParam, 3 );
      prct->right = hb_parvnl( iParam, 4 );
   }
   else
   {
      prct->top = 0;
      prct->left = 0;
      prct->bottom = 14;
      prct->right = 0;
   }

   return( prct );
}

HB_FUNC( GETCLIAREARECT )
/*
   Retrieves the client area rectangle of a window.

   Parameters:
       hWnd  : (HWND) Handle to the window.
       aRect : (Array, passed by reference) An array to store the rectangle coordinates.
               The array must have at least four elements. The function will store
               the top, left, bottom, and right coordinates of the client area in
               the first four elements of the array.

   Returns:
       .T. : (Logical) If the function succeeds in retrieving the client area.
       .F. : (Logical) If the function fails (e.g., invalid window handle).

   Purpose:
       This function provides a way to obtain the dimensions of a window's client
       area, which is the area within the window's borders and excluding the title
       bar and menu.  This information is often needed to position and size
       controls within the window correctly.

   Notes:
       - The function uses the GetClientRect Windows API function to retrieve the
         client area rectangle.
       - The function modifies the array passed as the second parameter by storing
         the rectangle coordinates in it.
*/
{
   RECT  rect;

   hb_retl( GetClientRect( hmg_par_raw_HWND( 1 ), &rect ) );

   HB_STORVNL( rect.top, 2, 1 );
   HB_STORVNL( rect.left, 2, 2 );
   HB_STORVNL( rect.bottom, 2, 3 );
   HB_STORVNL( rect.right, 2, 4 );
}

HB_FUNC( DRAWTEXTEX )   // ( hDC, cText, aRect, nStyle, [hFont], [nClr], [@nRight] ) --> nHeight
/*
   Draws formatted text in the specified rectangle using a device context.

   Parameters:
       hDC    : (HDC) Handle to the device context.  This specifies the drawing surface.
       cText  : (Character) The text string to be drawn.
       aRect  : (Array) An array containing the rectangle coordinates (top, left, bottom, right).
       nStyle : (Integer) Formatting options (e.g., DT_SINGLELINE, DT_CALCRECT).  These flags control how the text is drawn.
       hFont  : (HFONT, Optional) Handle to the font to use. If omitted, the default font of the device context is used.
       nClr   : (Integer, Optional) The color of the text (as a COLORREF value). If omitted, the default text color of the device context is used.
       nRight : (Numeric, Optional, passed by reference) A variable to store the right coordinate of the drawn text.  This is only used when the DT_CALCRECT style is specified.

   Returns:
       nHeight : (Integer) The height of the drawn text (in logical units).

   Purpose:
       This function provides a flexible way to draw text within a specified rectangle,
       allowing control over formatting, font, and color.  It's a wrapper around the
       Windows API's DrawTextEx function, making it easier to use from Harbour.  The
       DT_CALCRECT style is particularly useful for calculating the required size of
       the rectangle to fit the text before actually drawing it.

   Notes:
       - The function dynamically allocates memory for the text string using hb_xgrab
         and hb_xfree.
       - The function handles optional parameters for font and color.
       - The function uses SelectObject to select the specified font into the device
         context and restores the original font after drawing.
       - The function uses SetTextColor to set the text color and restores the
         original color after drawing.
       - The nRight parameter is only used when DT_CALCRECT is specified, and it
         returns the right coordinate of the calculated rectangle.
*/
{
   HDC      hDC;
   RECT     rct;
   LPCSTR   szText;
   DWORD    dwStyle;
   int      iLen, iRet = 0;
   HFONT    hFont, hOldFont;
   COLORREF nClr, nOldClr;
   BOOL     bColor = FALSE;
   BOOL     bFont = FALSE;

   if( hb_pcount() > 1 && HB_ISCHAR( 2 ) )
   {
      hDC = hmg_par_raw_HDC( 1 );

      iLen = hb_parclen( 2 );

      Param2Rect( 3, &rct );

      dwStyle = hb_pcount() > 3 && HB_ISNUM( 4 ) ? hb_parnl( 4 ) : DT_NOCLIP | DT_SINGLELINE;

      if( hb_pcount() > 4 && HB_ISNUM( 5 ) && ( GetObjectType( hmg_par_raw_HGDIOBJ( 5 ) ) == OBJ_FONT ) )
      {
         hFont = hmg_par_raw_HFONT( 5 );
         hOldFont = SelectObject( hDC, hFont );
         bFont = TRUE;
      }

      if( hb_pcount() > 5 && HB_ISNUM( 6 ) )
      {
         bColor = TRUE;
         nClr = ( COLORREF ) ( hb_parnl( 6 ) & 0xffffff );
         nOldClr = SetTextColor( hDC, nClr );
      }

      szText = hb_xgrab( iLen + 1 );
      memcpy( ( void * ) szText, ( void * ) hb_parc( 2 ), iLen );

      iRet = DrawTextEx( hDC, ( LPSTR ) szText, iLen, &rct, dwStyle, NULL );

      hb_xfree( ( void * ) szText );

      if( bFont )
      {
         SelectObject( hDC, hOldFont );
      }

      if( bColor )
      {
         SetTextColor( hDC, nOldClr );
      }

      if( hb_pcount() > 6 && HB_ISBYREF( 7 ) && ( dwStyle & DT_CALCRECT ) )
      {
         hb_storni( rct.right, 7 );
      }
   }

   hb_retni( iRet );
}

#pragma ENDDUMP
