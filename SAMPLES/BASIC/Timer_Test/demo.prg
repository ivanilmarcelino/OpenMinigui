/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "minigui.ch"

Function Main

	DEFINE WINDOW Form_1 ;
		AT 0,0 ;
		WIDTH 400 ;
		HEIGHT 400 ;
		TITLE 'Timer Test' ;
		MAIN

		@ 10,10 LABEL Label_1 SIZE 12

		DEFINE TIMER Timer_1
			INTERVAL 1000
			ACTION This.Label_1.Value := Time()
		END TIMER

		DEFINE TIMER Timer_2
			INTERVAL 2500
			ACTION PlayBeep()
			ONCE .T.
		END TIMER

	END WINDOW

	ACTIVATE WINDOW Form_1 ON INIT This.Center

Return Nil
