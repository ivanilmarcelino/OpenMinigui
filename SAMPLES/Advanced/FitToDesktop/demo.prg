/*
* MiniGUI FitToDesktop Demo
* (c) 2005 Jacek Kubica <kubica@wssk.wroc.pl>
*
* This demo shows how to get width and height of the client area for a full-screen window 
* on the primary display monitor, in pixels and get the coordinates of the portion of the screen 
* not obscured by the system taskbar or by application desktop toolbars
*
*/

#include "hmg.ch"


Procedure Main

	DEFINE WINDOW Form_1 ;
		AT GetDesktopRealTop(),GetDesktopRealLeft() ;
		WIDTH GetDesktopRealWidth() ;
		HEIGHT GetDesktopRealHeight() ;
		TITLE 'MiniGUI FitToDesktop Demo' ;
		MAIN 

		DEFINE MAIN MENU
                  DEFINE POPUP "&Test"
                    MENUITEM "Fit it now !"	ACTION FitIt()
                    SEPARATOR	
                    MENUITEM 'Exit'		ACTION Form_1.Release
	          END POPUP
		END MENU

	END WINDOW
	
	ACTIVATE WINDOW Form_1

Return


Procedure FitIt()

	Form_1.Row := GetDesktopRealTop()
	Form_1.Col := GetDesktopRealLeft()
	Form_1.Width := GetDesktopRealWidth()
	Form_1.Height := GetDesktopRealHeight()

Return
