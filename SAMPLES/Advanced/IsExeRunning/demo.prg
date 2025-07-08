/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-05 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2006 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

#define APP_TITLE 'Main Window'

FUNCTION Main()
Local hWnd

    IF IsExeRunning( cFileNoPath( App.ExeName ) )

	MsgAlert( "The " + APP_TITLE + " is already running!", "Warning" )

	hWnd := FindWindowEx( ,,, APP_TITLE )

	IF hWnd > 0

		IF IsIconic( hWnd )
			_Restore( hWnd )
		ELSE
			SetForeGroundWindow( hWnd )
		ENDIF
	ELSE

		MsgStop( "Cannot find application window!", "Error", , .f. )

	ENDIF

    ELSE

	DEFINE WINDOW Form_Main ;
		WIDTH 640 HEIGHT 480 ;
		TITLE APP_TITLE ;
		MAIN

	END WINDOW

	CENTER WINDOW Form_Main

	ACTIVATE WINDOW Form_Main

    ENDIF

RETURN NIL
