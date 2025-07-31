/*
   Defining a contrasting text color for a form

   Adapted for MiniGUI by Grigory Filatov
*/

#include "minigui.ch"

/*
 * FUNCTION Main()
 *
 * Creates and displays 30 windows with contrasting text colors.
 *
 * Purpose:
 *   This function demonstrates how to dynamically create multiple windows in HMG Extended and set their background and text colors to ensure good contrast.
 *   It generates 30 windows, each with a random background color and a text color (either black or white) chosen to contrast with the background.
 *   The windows are positioned in a grid-like layout on the screen.
 *   This is useful for creating visually appealing and readable user interfaces, especially when dealing with dynamically generated content or themes.
 *
 * Notes:
 *   The function uses HMG_GetUniqueName() to generate unique names for each window.
 *   The ContrastClr() function is used to determine the appropriate text color based on the background color.
 *   The windows are activated using ACTIVATE WINDOW NOWAIT, except for the last window, which is activated using ACTIVATE WINDOW.
 *   The last window has an ON KEY ESCAPE event handler that releases the window when the Escape key is pressed.
 */
FUNCTION Main()

   LOCAL aDlg[ 30 ], oFont, nClrText, nClrBack, n
   LOCAL nTop := -100
   LOCAL nLeft := 1200

   SET WINDOW MAIN OFF

   DEFINE FONT oFont FONTNAME "VERDANA" SIZE 16 DEFAULT

   FOR n := 1 TO 30

      nClrBack := hb_RandomInt( 0, CLR_WHITE )
      nClrText := ContrastClr( nClrBack )

      aDlg[ n ] := HMG_GetUniqueName( "Form_" )

      DEFINE WINDOW ( aDlg[ n ] ) ;
         AT - 200, -200 ;
         CLIENTAREA 150, 100 ;
         TITLE aDlg[ n ] ;
         NOMINIMIZE NOMAXIMIZE ;
         BACKCOLOR nClrBack ;
         ON INIT ( SetProperty( this.NAME, "Row", nTop ), SetProperty( this.NAME, "Col", nLeft ) )

         @ 40, 20 LABEL l1 VALUE "ANY TEXT" AUTOSIZE OF aDlg[ n ] CENTERALIGN VCENTERALIGN ;
            FONTCOLOR nClrText BACKCOLOR nClrBack

         IF nLeft >= 1100
            nTop += 140
            nLeft := 100
         ELSE
            nLeft += 200
         ENDIF

      END WINDOW

      IF n < 30
         ACTIVATE WINDOW ( aDlg[ n ] ) NOWAIT
      ELSE
         ON KEY ESCAPE OF ( aDlg[ 30 ] ) ACTION DoMethod( aDlg[ 30 ], "Release" )
      ENDIF

   NEXT

   ACTIVATE WINDOW ( aDlg[ 30 ] )

RETURN NIL

/*
 * FUNCTION ContrastClr( nClr )
 *
 * Determines the appropriate text color (black or white) based on the luminance of a given color.
 *
 * Parameters:
 *   nClr (NUMERIC): The color value for which to determine the contrasting text color.  It should be a numeric representation of a color, typically in the format 0x00RRGGBB.
 *
 * Returns:
 *   NUMERIC: CLR_WHITE if the luminance of the input color is less than 128, CLR_BLACK otherwise.  Returns CLR_WHITE if the input is not a valid color.
 *
 * Purpose:
 *   This function is used to ensure that text displayed on a background of a certain color is easily readable.  It calculates the luminance of the background color and chooses either black or white as the text color, depending on whether the background is light or dark.
 *   This is a common technique in UI design to improve accessibility and readability.
 *
 * Notes:
 *   The function calculates luminance using the formula: 0.299 * Red + 0.587 * Green + 0.114 * Blue.
 *   The function returns CLR_WHITE if the input color is not a valid numeric color value.
 */
FUNCTION ContrastClr( nClr )

   LOCAL nLuma

   IF HB_ISNUMERIC( nClr ) .AND. nClr >= 0 .AND. nClr <= 0x00ffffff
      nLuma := ( 0.299 * GetRed( nClr ) + 0.587 * GetGreen( nClr ) + 0.114 * GetBlue( nClr ) )
   ELSE
      RETURN CLR_WHITE
   ENDIF

RETURN iif( nLuma < 128, CLR_WHITE, CLR_BLACK )
