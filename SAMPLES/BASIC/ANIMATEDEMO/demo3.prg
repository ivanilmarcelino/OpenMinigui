/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

ANNOUNCE RDDSYS

#include "hmg.ch"

PROCEDURE Main()

   DEFINE WINDOW Form_1 ;
         WIDTH 410 ;
         HEIGHT 290 ;
         TITLE 'ANIMATE Switch Test' ;
         MAIN

      @ 160, 20 BUTTON Button_D1 ;
         CAPTION "Change animation file" ;
         ACTION SetAni() ;
         WIDTH 355 HEIGHT 35

      @ 205, 20 BUTTON Button_D2 ;
         CAPTION "Exit" ;
         ACTION ThisWindow.RELEASE ;
         WIDTH 355 HEIGHT 35

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1 ON INIT SetAni()

RETURN


PROCEDURE SetAni()

   STATIC nFile := 0

   LOCAL aFiles := { "santa.gif", "process.avi", "sample.avi" }
   LOCAL cFile, r, c

   IF IsControlDefined( ANIM_1, Form_1 )
      IF nFile > 1
         DoMethod( "Form_1", "ANIM_1", "Release" )
      ELSE
         RELEASE ANIGIF ANIM_1 OF Form_1
      ENDIF
   ENDIF

   IF ++nFile > 3
      nFile := 1
   ENDIF

   cFile := aFiles[ nFile ]

   ERASE WINDOW Form_1

   IF nFile > 1

      DEFINE ANIMATEBOX ANIM_1
         ROW 20
         COL 40
         PARENT Form_1
         FILE cFile
         AUTOPLAY .T.
         TRANSPARENT .T.
         BORDER .T.
      END ANIMATEBOX

   ELSE

      DEFINE ANIGIF ANIM_1
         ROW 20
         COL 20
         WIDTH hb_GetImageSize( cFile ) [ 1 ]
         HEIGHT hb_GetImageSize( cFile ) [ 2 ]
         PARENT Form_1
         PICTURE cFile
         DELAY 90
      END ANIGIF

      r := Form_1.ANIM_1.ROW
      c := Form_1.ANIM_1.COL

      DRAW RECTANGLE IN WINDOW Form_1 ;
         AT r - 1, c - 1 TO r + Form_1.ANIM_1.HEIGHT + 2, c + Form_1.ANIM_1.WIDTH + 2 ;
         PENCOLOR { 100, 100, 100 }

   ENDIF

RETURN
