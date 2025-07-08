/*
 * Rendering multi-colored text on the form
 *
 * Adapted for MiniGUI by Grigory Filatov
 */

#include "hmg.ch"
#include "i_winuser.ch"

*------------------------------------------------------------------------------*
FUNCTION Main
*------------------------------------------------------------------------------*
/*
   This is the main function of the application. It defines the main window,
   sets up the font and colors, and activates the window.
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

#define DT_SINGLELINE   32
#define DT_CALCRECT     1024
*------------------------------------------------------------------------------*
FUNCTION App_OnPaint( hWnd, hFont, cText, aColors )
*------------------------------------------------------------------------------*
/*
   This function is called when the window needs to be repainted. It draws the
   multi-colored text on the window.

   Input parameters:
       hWnd    - Handle to the window
       hFont   - Handle to the font
       cText   - The text to be displayed
       aColors - An array of colors to use for the text

   Return value:
       NIL
*/
   LOCAL aRect := { 0, 0, 0, 0 }
   LOCAL c, n, hDC, nRight, bk

   GetCliAreaRect( hWnd, aRect )

   // Centered Statusbar Text
   Form_Main.Statusbar.Item( 1 ) := PadC( "Color Text", aRect[ 4 ] / 4 )

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

*------------------------------------------------------------------------------*
FUNCTION App_OnEvents( hWnd, nMsg, wParam, lParam )
*------------------------------------------------------------------------------*
/*
   This function is the event handler for the application. It handles various
   window messages, such as WM_SIZE, and performs actions accordingly.

   Input parameters:
       hWnd    - Handle to the window
       nMsg    - The message code
       wParam  - Message-specific information
       lParam  - Message-specific information

   Return value:
       nResult - The result of the event handling
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

*------------------------------------------------------------------------------*
FUNCTION AShuffle( aArray )
*------------------------------------------------------------------------------*
/*
   This function shuffles the elements of an array in a random order.

   Input parameters:
       aArray - The array to be shuffled (passed by reference)

   Return value:
       aArray - The shuffled array
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

*------------------------------------------------------------------------------*
* Low Level C Routines
*------------------------------------------------------------------------------*

#pragma BEGINDUMP

#include <mgdefs.h>

RECT *Param2Rect( int iParam, RECT *prct )
/*
   Converts a Harbour array parameter to a RECT structure.
 
   Parameters:
       iParam - The index of the Harbour array parameter
       prct   - A pointer to the RECT structure to be filled
 
   Return:
       A pointer to the filled RECT structure.
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
   This function retrieves the client area rectangle of a window.

   Input parameters:
       hWnd - Handle to the window

   Return value:
       .T. - If the function succeeds
       .F. - If the function fails

       The function also stores the rectangle coordinates in an array passed by reference as the second parameter.
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
   This function draws formatted text in the specified rectangle.

   Input parameters:
       hDC    - Handle to the device context
       cText  - The text to be drawn
       aRect  - An array containing the rectangle coordinates (top, left, bottom, right)
       nStyle - The formatting options (e.g., DT_SINGLELINE, DT_CALCRECT)
       hFont  - (Optional) Handle to the font to use
       nClr   - (Optional) The color of the text
       nRight - (Optional) Variable to store the right coordinate of the drawn text (used with DT_CALCRECT)

   Return value:
       nHeight - The height of the drawn text
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
