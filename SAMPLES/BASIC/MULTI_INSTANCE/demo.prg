/*
 * MiniGUI Multiple Instances Demo
 * (c) 2003 Roberto Lopez
 */

#include "minigui.ch"

Function Main

	DEFINE WINDOW Form_Main ;
		AT 0,0 ;
		WIDTH 400 ;
		HEIGHT 200 ;
		TITLE 'Multi-Instance Demo' ;
		MAIN NOMAXIMIZE NOSIZE

		@ 10,100 BUTTON button_1 ;
			CAPTION 'Close All Child Windows' ; 
			ACTION ReleaseAllWindowsExceptMain() ;
			WIDTH 180 ;
			FONT 'Arial' ;
			SIZE 10 

		@ 40,100 BUTTON button_2 ;
			CAPTION 'Count All Child Windows' ; 
			ACTION MsgInfo( 'Count Of Child Windows: ' + hb_ntos(CountAllChildWindows()), 'Result' ) ;
			WIDTH 180 ;
			FONT 'Arial' ;
			SIZE 10 

		@ 70,100 BUTTON button_3 ;
			CAPTION 'Reload All Child Windows' ; 
			ACTION IF( Empty(CountAllChildWindows()), LoadAllChildWindows(.T.), ;
				MsgStop( 'Close All Child Windows Before Loading', 'Warning' ) ) ;
			WIDTH 180 ;
			FONT 'Arial' ;
			SIZE 10 

	END WINDOW

	LoadAllChildWindows(.F.)

	ACTIVATE WINDOW Form_Main , Form_1 , Form_2 , Form_3 , Form_4 , Form_5

Return Nil

*----------------------------------------
Function LoadAllChildWindows( lActivate )
*----------------------------------------

	LOAD WINDOW BaseForm AS Form_1
	LOAD WINDOW BaseForm AS Form_2
	LOAD WINDOW BaseForm AS Form_3
	LOAD WINDOW BaseForm AS Form_4
	LOAD WINDOW BaseForm AS Form_5

	Form_1.Row := 50
	Form_2.Row := 100
	Form_3.Row := 150
	Form_4.Row := 200
	Form_5.Row := 250

	Form_1.Col := 50
	Form_2.Col := 100
	Form_3.Col := 150
	Form_4.Col := 200
	Form_5.Col := 250

	Form_1.Title := 'Instance 1'
	Form_2.Title := 'Instance 2'
	Form_3.Title := 'Instance 3'
	Form_4.Title := 'Instance 4'
	Form_5.Title := 'Instance 5'

	IF lActivate
		ACTIVATE WINDOW Form_1 , Form_2 , Form_3 , Form_4 , Form_5
	ENDIF

Return Nil

*----------------------------------------
Function CountAllChildWindows
*----------------------------------------
LOCAL aStdForms := HMG_GetForms ('S')

Return LEN (aStdForms)

/*
  Function: ReleaseAllWindowsExceptMain
  Purpose: Release ALL WINDOWS EXCEPT the MAIN Window
*/
*----------------------------------------
Function ReleaseAllWindowsExceptMain
*----------------------------------------
LOCAL cForm, aStdForms := HMG_GetForms ('S')

	FOR EACH cForm IN aStdForms
		_ReleaseWindow (cForm)
	NEXT

Return Nil
