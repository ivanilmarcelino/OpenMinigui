/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Variable RadioGroup horizontal nofixed
 * By Pierpaolo Martinello 2018
 *
 */

#include "hmg.ch"

FUNCTION Main()

   SET FONT TO 'MS Shell Dlg', 10

   DEFINE WINDOW MainWA ;
         MAIN ;
         ROW 100 ;
         COL 100 ;
         WIDTH 100 ;
         HEIGHT 110 ;
         TITLE "Variable RadioGroup" ;
         NOSIZE ;
         NOMAXIMIZE ;
         NOMINIMIZE

      DEFINE FRAME RadioFR
         ROW 10
         COL 10
         WIDTH 50
         HEIGHT 55
         CAPTION "RadioGroup"
      END FRAME

      DEFINE RADIOGROUP RadioRG
         ROW 30
         COL 20
         WIDTH 50
         VALUE 3
         OPTIONS { "Item1", "Item 2 with long text oh yeah!", "Item3 short", "Item 4 long text" }
         SPACING 10
         HORIZONTAL .T.
         AUTOSIZE .T.
      END RADIOGROUP

      MainWa.RadioFR.WIDTH := MainWa.RadioFR.COL + GetRadioGroupWidth( "RadioRG", "MainWA" )

      MainWa.WIDTH := MainWa.RadioFR.WIDTH + 28

      ON KEY ESCAPE ACTION MainWA.Release()

   END WINDOW

   MainWA.Center()
   MainWA.Activate()

RETURN NIL


FUNCTION GetRadioGroupWidth( cControl, cForm ) // for Horizontal RadioGroup

   LOCAL nCount, aHandle, aWidth, nSpace, n, nWidth := 0

   IF _IsControlDefined( cControl, cForm )

      aHandle := GetControlHandle( cControl, cForm )
      nCount := Len( aHandle )
      aWidth := Array( nCount )
      nSpace := GetProperty( cForm, cControl, "Spacing" )

      FOR n := 1 TO nCount
         aWidth[ n ] := GetTextWidth( 0, GetWindowText( aHandle[ n ] ), GetWindowFont( aHandle[ n ] ) ) + 21
         nWidth += nSpace + aWidth[ n ]
      NEXT

   ENDIF

RETURN nWidth
