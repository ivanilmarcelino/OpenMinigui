#ifndef _MIXEDMODE_
#define _MIXEDMODE_
#endif

#include "minigui.ch"

/*
Note: This sample was modified from c:\MiniGUI\SAMPLES\BASIC\MixedMode\demo.prg
      by grzegorz.wojnarowski (at) gmail.com  2023-01-04

- HB_FUNC( HIDECONSOLE ) and HB_FUNC( SETCONSOLETITLE ) now use GetConsoleWindow()
   - now we can set any name for Console Title
   - no problem when .exe is started from .lnk file

- console window is displayed every time when INKEY() is invoked,
  now we can use any function written in Clipper with no modification :-)
  note: INKEY() is used internally by many Clipper functions: READ, WAIT, MENU TO etc.
        and we need console only when it waits for our reaction

- console window is hidden every time when Clipper function ends
  (checked by main window timer)

PROBLEM TO SOLVE: hide and restore all opened windows, including MODAL windows - see FUNCTION SetConsole( lOn )
   note: we need to hide windows when console is active to avoid "Program is not responding" message

*/


STATIC cConsoleTitle := 'My DEMO console name'
STATIC lConsoleOn := .F.


FUNCTION MAIN

   // hide "::" delimiters for GET's in console mode
   SET DELIMITERS OFF

   // set console size
   SetMode( 25, 80 )

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 640 HEIGHT 480 ;
      TITLE 'Harbour MiniGUI Demo' ;
      MAIN ;
      FONT 'Arial' SIZE 10

      // show console when INKEY() is invoked (for READ, MENU TO, WAIT etc.)
      SETINKEYBEFOREBLOCK( {|| iif( ! lConsoleOn, SetConsole( .T. ), ) } )

      // hide console when inactive
      DEFINE TIMER tmTimerConsoleOff OF Form_1 INTERVAL 100 ACTION iif( lConsoleOn, SetConsole( .F. ), )

      DEFINE LABEL Label1
         ROW 10
         COL 10
         VALUE "Good morning"
      END LABEL

      DEFINE GETBOX Box11
         ROW 10
         COL 100
         VALUE 2.5
         PICTURE '999,999,999,999.9999'
      END GETBOX

      DEFINE BUTTON Btn1
         ROW 50
         COL 10
         CAPTION 'DOS 1'
         ACTION This.CAPTION := AllTrim( Str( FN1() ) )
      END BUTTON

      DEFINE BUTTON Btn2
         ROW 100
         COL 10
         CAPTION 'Message'
         ACTION FN2( 3 )
      END BUTTON

      DEFINE BUTTON Btn3
         ROW 150
         COL 10
         CAPTION 'GET READ'
         ACTION This.CAPTION := AllTrim( FN3( This.Caption ) )
      END BUTTON

      DEFINE BUTTON Btn4
         ROW 200
         COL 10
         CAPTION 'ALERT()'
         ACTION This.CAPTION := 'ALERT = ' + LTrim( Str( Alert( 'Set caption for button', { 'One', 'Two' } ) ) )
      END BUTTON

      DEFINE BUTTON Btn5
         ROW 250
         COL 10
         CAPTION 'MENU TO'
         ACTION This.CAPTION := AllTrim( FN4() )
      END BUTTON

      DEFINE BUTTON Btn6
         ROW 300
         COL 10
         CAPTION 'Modal Window'
         ACTION TestModal()
      END BUTTON

      DEFINE BUTTON BtnExit
         ROW 350
         COL 10
         CAPTION 'EXIT'
         ACTION Form_1.Release()
      END BUTTON

   END WINDOW

   HideConsole()
   Form_1.Center()
   Form_1.Activate()

RETURN NIL


FUNCTION SetConsole( lOn )

   LOCAL cForm
   STATIC aHide := {}

   IF lOn
      // hide all my windows and show console with focus

      IF ! lConsoleOn
         lConsoleOn := .T.

         aHide := {}
         FOR EACH cForm IN HMG_GetForms()
            IF _IsWindowDefined( cForm )
               HIDE WINDOW &cForm
               AAdd( aHide, cForm )
            ENDIF
         NEXT

         SetConsoleTitle( cConsoleTitle )
         ShowConsole()
         SetForegroundConsole()

      ENDIF

   ELSE
      // hide console and show all my windows with focus

      IF lConsoleOn
         lConsoleOn := .F.

         HideConsole()

         FOR EACH cForm IN aHide
            IF _IsWindowDefined( cForm )
               SHOW WINDOW &cForm
            ENDIF
         NEXT

      ENDIF

   ENDIF

RETURN NIL


FUNCTION FN1()

   LOCAL r1 := 0
   LOCAL r2 := 0

   MEMVAR GetList

   SetConsole( .T. )

   CLEAR SCREEN

   @ 10, 10 SAY ' S1 ' GET r1 PICT '9999'
   @ 12, 10 SAY ' S2 ' GET r2 PICT '9999'
   READ

   SetConsole( .F. )

RETURN R2


FUNCTION FN2( n )

   LOCAL i

   SetConsole( .T. )

   CLEAR SCREEN

   FOR i := 1 TO n
      ?i
   NEXT
   WAIT

   SetConsole( .F. )

   MsgBox( 'bla' + Transform( n, '99' ), 'Title' )

RETURN NIL


FUNCTION FN3( c )

   MEMVAR GetList

   CLEAR SCREEN

   c := PadR( c, 30 )

   @ 10, 10 SAY 'Button caption ' GET c
   READ

   c := AllTrim( c )

RETURN( c )


#include "box.ch"

FUNCTION FN4()

   LOCAL i, c

   CLEAR SCREEN

   @ 8, 8, 16, 18 BOX B_DOUBLE

   @ 10, 10 PROMPT 'Menu 1'
   @ 12, 10 PROMPT 'Menu 2'
   @ 14, 10 PROMPT 'Menu 3'

   MENU TO i

   c := 'Menu = ' + LTrim( Str( i ) )

RETURN( c )


FUNCTION TestModal()

   DEFINE WINDOW Form_2 ;
      AT 0, 0 ;
      WIDTH 480 HEIGHT 320 ;
      TITLE 'Modal Window' ;
      MODAL

      DEFINE BUTTON Btn1
         ROW 50
         COL 10
         CAPTION 'DOS 1'
         ACTION This.CAPTION := AllTrim( Str( FN1() ) )
      END BUTTON

      DEFINE BUTTON Btn2
         ROW 100
         COL 10
         CAPTION 'Message'
         ACTION FN2( 3 )
      END BUTTON

      DEFINE BUTTON Btn3
         ROW 150
         COL 10
         CAPTION 'GET READ'
         ACTION This.CAPTION := AllTrim( FN3( This.Caption ) )
      END BUTTON

      DEFINE BUTTON BtnExit
         ROW 200
         COL 10
         CAPTION 'EXIT MODAL'
         ACTION Form_2.Release()
      END BUTTON

   END WINDOW

   Form_2.Center()
   Form_2.Activate()

RETURN NIL


#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"

HB_FUNC( HIDECONSOLE )
{
   ShowWindow( ( HWND ) GetConsoleWindow(), SW_HIDE );
}

HB_FUNC( SHOWCONSOLE )
{
   ShowWindow( ( HWND ) GetConsoleWindow(), SW_RESTORE );
}

HB_FUNC( SETFOREGROUNDCONSOLE )
{
   SetForegroundWindow( ( HWND ) GetConsoleWindow() );
}

HB_FUNC( SETCONSOLETITLE )
{
   SetConsoleTitle( hb_parc( 1 ) );
}

#pragma ENDDUMP
