/*
 * Vista Command Link Button Demo
 * (c) 2016 Grigory Filatov
 */

#include "minigui.ch"

PROCEDURE Main()

	LOCAL cBut1, cBut2

	IF ! IsWinNT() .OR. ! ISVISTAORLATER()
		MsgStop( 'This Program Runs In WinVista Or Later!', 'Stop' )
		Return
	ENDIF

	DEFINE WINDOW Win1 ;
		AT 0,0 ;
		WIDTH 400 ;
		HEIGHT 300 ;
		TITLE 'Vista Command Link Button Demo' ;
		MAIN ;
		ON RELEASE Win1.(cBut1).Release

		DEFINE MAIN MENU
			DEFINE POPUP 'Methods'
				MENUITEM 'Custom Method: SetFocus' ACTION Win1.(cBut1).SetFocus
				MENUITEM 'Custom Method: Disable' ACTION Win1.(cBut1).Disable
				MENUITEM 'Custom Method: Enable' ACTION Win1.(cBut1).Enable
				MENUITEM 'Custom Method: SetShield' ACTION Win1.(cBut1).SetShield
			END POPUP
			DEFINE POPUP 'Properties'
				MENUITEM 'Custom Property: Handle (Get)' ACTION MsgInfo ( Win1.(cBut1).Handle )
				MENUITEM 'Custom Property: Handle (Set)' ACTION Win1.(cBut1).Handle := 1
				MENUITEM 'Custom Property: Caption (Get)' ACTION MsgInfo ( Win1.(cBut1).Caption )
				MENUITEM 'Custom Property: Caption (Set)' ACTION Win1.(cBut1).Caption := 'New Caption'
				MENUITEM 'Custom Property: NoteText (Get)' ACTION MsgInfo ( Win1.(cBut1).NoteText )
				MENUITEM 'Custom Property: NoteText (Set)' ACTION Win1.(cBut1).NoteText := 'New Note'
				MENUITEM 'Custom Property: Picture (Get)' ACTION MsgInfo ( Win1.(cBut1).Picture )
				MENUITEM 'Custom Property: Picture (Set)' ACTION Win1.(cBut1).Picture := 'button.bmp'
			END POPUP
		END MENU

		@ 10 , 10 CLBUTTON NUL ;
			CAPTION 'Command Link' ;
			NOTETEXT "This is a test note" ;
			ACTION MsgInfo('Click!') DEFAULT

		@ 100 , 10 CLBUTTON NUL ;
			CAPTION 'Command Link 2' ;
			NOTETEXT "This is a test note 2" ;
			ACTION MsgInfo('Click 2!')

		cBut1 := HMG_GetFormControls( This.Name, "CLBUTTON" )[ 1 ]
		cBut2 := HMG_GetFormControls( This.Name, "CLBUTTON" )[ 2 ]

		IF ! wapi_IsUserAnAdmin()
			Win1.(cBut2).SetShield()
		ENDIF

	END WINDOW

	CENTER WINDOW Win1

	ACTIVATE WINDOW Win1

RETURN
