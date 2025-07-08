/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "hmg.ch"

PROCEDURE Main

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 400 ;
         HEIGHT 200 ;
         TITLE 'Transparency Sample' ;
         MAIN

      DEFINE BUTTON Button_1
         ROW 10
         COL 10
         CAPTION 'Set Transparency ON'
         WIDTH 140
         ACTION Form_1.Slider_1.VALUE := 70
      END BUTTON

      DEFINE BUTTON Button_2
         ROW 40
         COL 10
         WIDTH 140
         CAPTION 'Set Transparency OFF'
         ACTION ( Form_1.Slider_1.VALUE := 100, ;
            RemoveTransparency( Application.Handle ) )
      END BUTTON

      DEFINE SLIDER Slider_1
         ROW 80
         COL 10
         VALUE 100
         WIDTH 310
         HEIGHT 50
         RANGEMIN 0
         RANGEMAX 100
         ON SCROLL Slider_Change()
         ON CHANGE Slider_Change()
      END SLIDER

      DEFINE TEXTBOX TextBox_1
         ROW 85
         COL 325
         VALUE "100 %"
         WIDTH 50
         MAXLENGTH 5
      END TEXTBOX

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN

/*
*/
FUNCTION Slider_Change

   LOCAL nValue := Form_1.Slider_1.VALUE

   Form_1.TextBox_1.VALUE := Str( nValue, 3 ) + " %"

   IF nValue < 100

      SET WINDOW Form_1 TRANSPARENT TO ( 255 * nValue ) / 100

   ELSE

      SET WINDOW Form_1 TO OPAQUE

   ENDIF

RETURN NIL

/*
*/
#define GWL_EXSTYLE		(-20)
#define WS_EX_LAYERED	524288

PROCEDURE RemoveTransparency( hWnd )

   SetWindowLong( hWnd, GWL_EXSTYLE, hb_bitAnd( GetWindowLong( hWnd, GWL_EXSTYLE ), hb_bitNot( WS_EX_LAYERED ) ) )

   RedrawWindow( hWnd )

RETURN
