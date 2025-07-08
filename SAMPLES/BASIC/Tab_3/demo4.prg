/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Grigory Filatov <gfilatov@gmail.com>
*/

#include "minigui.ch"

Function Main

	SET FONT TO 'MS Shell Dlg', 12

	DEFINE WINDOW Form_1 ;
		CLIENTAREA 500, 400 ;
		TITLE 'Harbour MiniGUI Demo' ;
		MAIN ;
		ON SIZE SizeTest()

		DEFINE MAIN MENU

			DEFINE POPUP 'File'
				MENUITEM 'Exit' ACTION ThisWindow.Release
			END POPUP

			DEFINE POPUP 'Help'
				MENUITEM 'About' ACTION MsgInfo( MiniguiVersion(), 'Tab Control Demo', , .f. )
			END POPUP

		END MENU

		SetTab_1()

	END WINDOW

	Form_1.Center

	Form_1.Activate

Return Nil


Procedure SizeTest()

	Form_1.Tab_1.Width := Form_1.Width - 40
	Form_1.Tab_1.Height := Form_1.Height - 80

Return


#define COLOR_BTNFACE 15

Procedure SetTab_1( nValue, aAccessAllowed )
Local nColor := GetSysColor( COLOR_BTNFACE )
Local aColor := { GetRed( nColor ), GetGreen( nColor ), GetBlue( nColor ) }

	DEFAULT aAccessAllowed := { .f., .t., .f., .t., .f. }
	DEFAULT nValue := AScan( aAccessAllowed, .t. )

	IF IsControlDefined(Tab_1, Form_1)
		Form_1.Tab_1.Release
	ENDIF

	DEFINE TAB Tab_1 ;
		OF Form_1 ;
		AT 10,10 ;
		WIDTH Form_1.Width - 40 ;
		HEIGHT Form_1.Height - 80 ;
		VALUE nValue ;
		BACKCOLOR aColor ;
		HOTTRACK ;
		HTFORECOLOR BLUE ;
		HTINACTIVECOLOR GRAY

		PAGE 'Page &1' TOOLTIP 'Page 1'

		      @ 100,100 BUTTON Button_1 CAPTION "Test" WIDTH 50 HEIGHT 50 ACTION MsgInfo( 'Test! 1' )

		END PAGE

		PAGE 'Page &2' TOOLTIP 'Page 2'

		      @ 100,100 BUTTON Button_2 CAPTION "Test" WIDTH 50 HEIGHT 50 ACTION MsgInfo( 'Test! 2' )

		END PAGE

		PAGE 'Page &3' TOOLTIP 'Page 3'

		      @ 100,100 BUTTON Button_3 CAPTION "Test" WIDTH 50 HEIGHT 50 ACTION MsgInfo( 'Test! 3' )

		END PAGE

		PAGE 'Page &4' TOOLTIP 'Page 4'

		      @ 100,100 BUTTON Button_4 CAPTION "Test" WIDTH 50 HEIGHT 50 ACTION MsgInfo( 'Test! 4' )

		END PAGE

		PAGE 'Page &5' TOOLTIP 'Page 5'

		      @ 100,100 BUTTON Button_5 CAPTION "Test" WIDTH 50 HEIGHT 50 ACTION MsgInfo( 'Test! 5' )

		END PAGE

	END TAB

	Form_1.Tab_1.ONCHANGE := {|| aAccessAllowed[ This.Value ] }

Return
