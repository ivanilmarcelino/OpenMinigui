/*
 * Harbour MiniGUI Demo
*/

#include "minigui.ch"

FUNCTION Main

	LOCAL cTitle := 'WAIT WINDOW DEMO'

	IF GetFontHandle( "DlgFont" ) == 0
		DEFINE FONT DlgFont FONTNAME "Tahoma" SIZE 10
	ENDIF

	DEFINE WINDOW Win_1 ;
		AT 0,0 ;
		WIDTH 400 ;
		HEIGHT 400 ;
		TITLE cTitle ;
		MAIN

		@ 035,70 BUTTON Button_0 ;
                         CAPTION 'WAIT WINDOW "Press any key..."' ;
                         ACTION Test() WIDTH 250

		@ 070,70 BUTTON Button_1 ;
                         CAPTION 'WAIT WINDOW "Processing..." NOWAIT' ;
                         ACTION Test1() WIDTH 250

		@ 105,70 BUTTON Button_2 ;
                         CAPTION 'WAIT CLEAR' ;
                         ACTION Test2( cTitle ) WIDTH 250

	END WINDOW

	CENTER WINDOW Win_1 

	ACTIVATE WINDOW Win_1

RETURN Nil


PROCEDURE Test()

	SET WAITWINDOW PROMPT TO "Press any key to continue..." FONTCOLOR RED BACKCOLOR YELLOW

	WAIT WINDOW .F.

RETURN


PROCEDURE Test1()

	SET WAITWINDOW PROMPT TO "Please press the 'WAIT CLEAR' button for closing of this intermediate window..."
	SET WAITWINDOW FONT "DlgFont" FONTCOLOR NAVY BACKCOLOR WHITE

	WAIT WINDOW .T.

	Win_1.Title := "Processing..."

	Win_1.Setfocus()

RETURN


PROCEDURE Test2( cTitle )

	WAIT CLEAR

	Win_1.Title := cTitle

RETURN
