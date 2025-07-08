/*
 * Harbour MiniGUI Hello World Demo
 * (c) 2002-2009 Roberto Lopez <harbourminigui@gmail.com>
*/

#include "minigui.ch"

DECLARE WINDOW Win_2

FUNCTION Main

	LOCAL i, cForm

	DEFINE WINDOW Win_1 ;
		TITLE 'Hello World!' ;
		/*WINDOWTYPE*/ MAIN ;
		ON INIT Win_2.Setfocus()

	END WINDOW

	DEFINE WINDOW Win_3 ;
		TITLE 'Child Window' ;
		/*WINDOWTYPE*/ CHILD

	END WINDOW

	DEFINE WINDOW Win_2 ;
		TITLE 'Modal Window' ;
		/*WINDOWTYPE*/ MODAL ;
		ON RELEASE SetFocusToChildWin()

	END WINDOW

	FOR i := 1 TO 3
		cForm := "Win_" + Str( i, 1 )
		_DefineHotKey( cForm, 0, VK_ESCAPE, hb_MacroBlock( "_ReleaseWindow('" + cForm + "')" ) )
	NEXT

	Win_2.Center
	Win_3.Center

	ACTIVATE WINDOW Win_3, Win_2, Win_1

RETURN NIL


PROCEDURE SetFocusToChildWin()

	DEFINE TIMER NUL
		PARENT Win_1
		INTERVAL 20
		ONCE .T.
		ACTION Win_3.Setfocus()
	END TIMER

RETURN
