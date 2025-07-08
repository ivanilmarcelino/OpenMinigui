/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-10 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2013 Simon Norbert <simon.n@t-online.hu>
*/

#include <minigui.ch>

PROCEDURE MAIN

   LOCAL cTitle := 'One Instance Sample'

   OnlyOneInstance( cTitle )

   DEFINE WINDOW Main ;
         WIDTH 600 ;
         HEIGHT 400 ;
         TITLE cTitle ;
         MAIN

   END WINDOW

   Main.Center
   Main.Activate

RETURN


FUNCTION OnlyOneInstance( cAppTitle )

   LOCAL hWnd := FindWindowEx( ,,, cAppTitle )

   IF hWnd # 0
      iif( IsIconic( hWnd ), _Restore( hWnd ), SetForeGroundWindow( hWnd ) )
      ExitProcess()
   ENDIF

RETURN NIL
