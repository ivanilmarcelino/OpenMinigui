/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by Edward 28/Nov/2019
 *
 */

#define _NO_BTN_PICTURE_

#include "hmg.ch"


FUNCTION MAIN

   SET DIALOGBOX POSITION CENTER OF PARENT

   DEFINE WINDOW Win_1 ;
      ROW 0 ;
      COL 0 ;
      WIDTH 370 ;
      HEIGHT 150 ;
      TITLE 'Win 1' ;
      WINDOWTYPE MAIN

      DEFINE BUTTON B_1
         ROW 10
         COL 10
         WIDTH 90
         HEIGHT 28
         ACTION MsgInfo( "Form Name - " + FocusedWindow.Name + CRLF + ;
                "Form Title - " + FocusedWindow.Title + CRLF + ;
                "ClientArea Width - " + hb_ntos( FocusedWindow.ClientWidth ) + CRLF + ;
                "ClientArea Height - " + hb_ntos( FocusedWindow.ClientHeight ) + CRLF + ;
                "Focused Control - " + FocusedWindow.FocusedControl )
      END BUTTON

      DEFINE STATUSBAR FONT 'Verdana' SIZE 8
      END STATUSBAR

      DEFINE TIMER Timer_1 ;
         INTERVAL 100 ;
         ACTION ( getFocusedForm(), setFocusedForm() )

   END WINDOW

   DEFINE WINDOW Win_2 ;
      ROW 150 ;
      COL 150 ;
      WIDTH 370 ;
      HEIGHT 150 ;
      TITLE 'Win 2'
   END WINDOW

   DEFINE WINDOW Win_3 ;
      ROW 300 ;
      COL 300 ;
      WIDTH 370 ;
      HEIGHT 150 ;
      TITLE 'Win 3'
   END WINDOW

   DEFINE WINDOW Win_4 ;
      ROW 450 ;
      COL 450 ;
      WIDTH 370 ;
      HEIGHT 150 ;
      TITLE 'Win 4'
   END WINDOW

   ACTIVATE WINDOW Win_1, Win_2, Win_3, Win_4

RETURN NIL


FUNCTION getFocusedForm()

   Win_1.StatusBar.Item( 1 ) := "FocusedWindow.Name=" + FocusedWindow.Name + " , ThisWindow.Name=" + ThisWindow.Name
   Win_1.B_1.Caption := FocusedWindow.Title

RETURN NIL


FUNCTION setFocusedForm()

   LOCAL aPos := GetCursorPos ()
   LOCAL nHWnd := WindowFromPoint ( { aPos[ 2 ], aPos[ 1 ] } )

   IF ASCAN( _HMG_aFormHandles, nHWnd ) > 0
      BringWindowToTop( nHWnd )
      SetFocus( nHWnd )
   ENDIF

RETURN NIL
