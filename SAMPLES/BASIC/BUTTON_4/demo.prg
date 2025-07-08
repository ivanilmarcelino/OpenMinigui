/*
 * MINIGUI - Harbour Win32 GUI library
*/

#include "minigui.ch"

FUNCTION Main()

   LOCAL aBtn, n, nCnt
   LOCAL cPngPath := hb_DirBase() + "images\"
   LOCAL aPng := {}

   AEval( { 3, 1, 2, 6, 7, 8, 9, 10 }, {| c | ;
      AAdd( aPng, cPngPath + "image" + hb_ntos( c ) + ".png" ) } )

   nCnt := Len( aPng )
   aBtn := Array( nCnt )

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 80 + 70 * nCnt ;
         HEIGHT 430 ;
         MAIN ;
         TITLE 'Disabled Buttons Test' ;
         NOSIZE ;
         NOMAXIMIZE

      @ 40, 40 LABEL Lbl_1 VALUE "ENABLED BUTTONS NORMAL LOOK" AUTOSIZE

      FOR n := 1 TO nCnt

         aBtn[ n ] := HMG_GetUniqueName()

         DEFINE BUTTON ( aBtn[ n ] )
            ROW 70
            COL n * 70 - 30
            ACTION NIL
            PICTURE aPng[ n ]
            WIDTH 64
            HEIGHT 64
         END BUTTON

      NEXT

      @ 150, 40 LABEL Lbl_2 VALUE "LOADED PICTURES NORMAL LOOK" AUTOSIZE

      FOR n := 1 TO nCnt

         aBtn[ n ] := HMG_GetUniqueName()

         DEFINE IMAGE ( aBtn[ n ] )
            ROW 180
            COL n * 70 - 20
            ACTION NIL
            PICTURE aPng[ n ]
            WIDTH 48
            HEIGHT 48
            STRETCH .T.
            IF n == 4
               HEIGHT 55
            ENDIF
         END IMAGE

      NEXT

      @ 260, 40 LABEL Lbl_3 VALUE "DISABLED BUTTONS" AUTOSIZE

      FOR n := 1 TO nCnt

         aBtn[ n ] := HMG_GetUniqueName()

         DEFINE BUTTON ( aBtn[ n ] )
            ROW 290
            COL n * 70 - 30
            ACTION NIL
            PICTURE aPng[ n ]
            WIDTH 64
            HEIGHT 64
         END BUTTON

         Form_1.( aBtn[ n ] ).Enabled := .F.

      NEXT

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL
