/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2006-2019 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"
/*
*/
Function Main

	DEFINE WINDOW Form_Main ;
		WIDTH 640 HEIGHT 480 ;
		TITLE 'Main Window' ;
		MAIN ;
		NOSHOW ;
		ON INIT ShowSplash()

	END WINDOW

	INIT SPLASH WINDOW

	CENTER WINDOW Form_Main

	ACTIVATE WINDOW Form_Main

Return Nil
/*
*/
Procedure ShowSplash()

	SHOW SPLASH WINDOW ;
		PICTURE 'DEMO' ;
		DELAY 3

Return
