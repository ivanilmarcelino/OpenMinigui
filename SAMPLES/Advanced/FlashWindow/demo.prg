/*
 * MiniGUI Flashing Window Demo
 */

#include "hmg.ch"

#define APP_TITLE "Hello World!"

PROCEDURE MAIN

   LOCAL hWnd, lSuccess

   SET MULTIPLE ON

   hWnd := FindWindowEx( 0, 0, 0, APP_TITLE )

   IF hWnd > 0

      IF IsIconic( hWnd )
         _Restore( hWnd )
      ELSE
         SetActiveWindow( hWnd )
         _Minimize( hWnd )
         _Restore( hWnd )
      ENDIF

   ELSE

      DEFINE WINDOW Win_1 ;
            CLIENTAREA 400, 350 ;
            TITLE APP_TITLE ;
            MAIN ;
            ON MINIMIZE Flashing( This.Name, 5, {|| IsIconic( This.Handle ) } )

         ON KEY CONTROL + SPACE ACTION _Minimize( ThisWindow.Handle ) TO lSuccess
         IF ! lSuccess
            MsgAlert( "Can not established the Ctrl+Space hotkey!", "Alert" )
         ENDIF

      END WINDOW

      CENTER WINDOW Win_1

      ACTIVATE WINDOW Win_1

   ENDIF

RETURN

/*
 * Flashing( [<cFormName>], [<nBlinks>], [<bWhen>] )
 */
FUNCTION Flashing( cForm, nBlinks, bWhen )

   LOCAL lResult

   DEFAULT cForm := ThisWindow.Name, nBlinks := 300, bWhen := {|| .T. }

   IF ( lResult := Eval( bWhen ) )
      FLASH WINDOW &cForm COUNT nBlinks INTERVAL 0
   ENDIF

RETURN lResult
