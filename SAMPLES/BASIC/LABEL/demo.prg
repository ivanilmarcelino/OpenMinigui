/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
*/

#include "minigui.ch"

Function Main

	DEFINE WINDOW Form_Main ;
		WIDTH 640 HEIGHT 480 ;
		TITLE 'MiniGUI Label Demo' ;
		MAIN ;
		BACKCOLOR SILVER

		@ 200,10 LABEL Label_1 ;
			WIDTH 610 HEIGHT 40 ;
			VALUE 'Click Me !' ;
			ON CLICK MsgInfo('Label Clicked!') ;
			ON MOUSEHOVER ( RC_CURSOR( "MINIGUI_FINGER" ), Form_Main.Label_1.FontBold := .T. ) ;
			ON MOUSELEAVE Form_Main.Label_1.FontBold := .F. ;
			FONT 'Arial' SIZE 24 ;
			TRANSPARENT ;
			CENTERALIGN

		DEFINE LABEL Label_2
			ROW	10
			COL	180
			VALUE 'Some text'
			AUTOSIZE .T.
			ONMOUSEHOVER ( RC_CURSOR( "MINIGUI_FINGER" ), Form_Main.Label_2.FontBold := .T. )
			ONMOUSELEAVE Form_Main.Label_2.FontBold := .F.
			FONTNAME 'Times New Roman'
			FONTSIZE 12
			FONTCOLOR BLUE
		END LABEL

		@ 350,180 RADIOGROUP Radio_1 ;
			OPTIONS { 'One', 'Two', 'Three', 'Four' } ;
			AUTOSIZE ;
			VALUE 1 ;
			HORIZONTAL

		DEFINE CHECKBOX Check_1
			ROW	400
			COL	180
			CAPTION 'Change CheckBox and RadioGroup Transparent' 
			VALUE .F.
			ONCHANGE ( SetProperty( 'Form_Main', This.Name, 'Transparent', This.Value ), ;
				SetProperty( 'Form_Main', 'Radio_1', 'Transparent', This.Value ) )
			AUTOSIZE .T.
 		END CHECKBOX

		ON KEY ESCAPE ACTION ThisWindow.Release

	END WINDOW

	Form_Main.Label_2.Value := 'Hello All, This Is An AutoSizable Label !'
	Form_Main.Label_2.OnClick := { || SetProperty( 'Form_Main', 'Label_2', 'Transparent', !GetProperty( 'Form_Main', 'Label_2', 'Transparent' ) ) }
	Form_Main.Label_2.Transparent := .T.

	CENTER WINDOW Form_Main

	ACTIVATE WINDOW Form_Main

Return Nil
