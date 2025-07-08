/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "minigui.ch"

Function Main

	DEFINE WINDOW Form_1 ;
		WIDTH 400 ;
		HEIGHT 400 ;
		TITLE 'Timer Test 2' ;
		MAIN 

		DEFINE TIMER Timer_1
			INTERVAL 10000
			ACTION TimerTest()
		END TIMER

	END WINDOW

	ACTIVATE WINDOW Form_1

Return Nil

Procedure TimerTest()

	DEACTIVATE TIMER Timer_1 OF Form_1
	MsgInfo ('Hey')
	ACTIVATE TIMER Timer_1 OF Form_1

Return