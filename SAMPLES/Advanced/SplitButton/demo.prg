/*
 * MiniGUI User Components Demo
 * (c) 2017 Grigory Filatov
 */

#include "minigui.ch"

Procedure Main

	IF !IsWinNT() .OR. !IsVistaOrLater()
		MsgStop( 'This Program Runs In WinVista Or Later!', 'Stop' )
		Return
	ENDIF

	SET FONT TO 'MS Shell Dlg', 8

	DEFINE WINDOW Win1 ;
		AT 0,0 ;
		WIDTH 400 ;
		HEIGHT 300 ;
		TITLE 'Vista Split Button Demo' ;
		MAIN

		DEFINE MAIN MENU
			DEFINE POPUP 'Methods'
				MENUITEM 'Custom Method: SetFocus' ACTION Win1.Test.SetFocus
				MENUITEM 'Custom Method: Disable'  ACTION Win1.Test.Disable
				MENUITEM 'Custom Method: Enable'   ACTION Win1.Test.Enable
			END POPUP
			DEFINE POPUP 'Properties'
				MENUITEM 'Set New Font'            ACTION Win1.Test.FontName := "Tahoma"
				MENUITEM 'Set Font to Bold'        ACTION Win1.Test.FontBold := .T.
				MENUITEM 'Set Font Size to 12'     ACTION Win1.Test.FontSize := 12
				MENUITEM 'Get Font Size'           ACTION MsgInfo( Win1.Test.FontSize )
				MENUITEM 'Get Icon Property'       ACTION MsgInfo ( Win1.Test_2.Icon )
				MENUITEM 'Set Icon Property'       ACTION ChangeSPBIcon( "2MG64.ico" )
			END POPUP
		END MENU

		@ 10 , 10 SPLITBUTTON test ;
			CAPTION 'Split Button' ;
			ACTION MsgInfo( "Split Button 1", "Pressed" ) ;
			TOOLTIP 'Split Button 1' ;
			DEFAULT

		DEFINE DROPDOWN MENU BUTTON test
			MENUITEM "Split"  ACTION Win1.Test.Caption := "Split"
			MENUITEM "Button" ACTION Win1.Test.Caption := "Button"
		END MENU

		DEFINE SPLITBUTTON test_2
			ROW    100
			COL    10
			WIDTH  148
			HEIGHT 38
			CAPTION 'Split Button 2'
			ICON 'MG32'
			ACTION MsgInfo( "Split Button 2", "Pressed" )
			TOOLTIP 'Split Button 2'
		END SPLITBUTTON

		DEFINE DROPDOWN MENU BUTTON test_2
			MENUITEM "Drop Down Menu 1" ACTION MsgInfo( "Button Drop Down Menu 1", "Pressed" )
			MENUITEM "Drop Down Menu 2" ACTION MsgInfo( "Button Drop Down Menu 2", "Pressed" )
		END MENU

		ON KEY F4 ACTION LaunchMenu( GetControlIndex( Win1.FocusedControl, "Win1" ) )
		ON KEY ALT+DOWN ACTION LaunchMenu( GetControlIndex( Win1.FocusedControl, "Win1" ) )

	END WINDOW

	CENTER WINDOW Win1

	ACTIVATE WINDOW Win1

Return


*------------------------------------------------------------------------------*
FUNCTION LaunchMenu( i )
*------------------------------------------------------------------------------*
   LOCAL aPos := {0, 0, 0, 0}

   IF i > 0 .AND. _HMG_aControlType[ i ] == 'SPBUTTON'

      GetWindowRect( _HMG_aControlHandles[ i ], aPos )

      TrackPopupMenu( _HMG_aControlRangeMax[ i ], aPos[ 1 ] + 1, ;
         aPos[ 2 ] + _HMG_aControlHeight[ i ], _HMG_aControlParentHandles[ i ] )

   ENDIF

RETURN Nil


*------------------------------------------------------------------------------*
FUNCTION ChangeSPBIcon( cIcon )
*------------------------------------------------------------------------------*

   Win1.Test_2.Height := 38*2
   Win1.Test_2.Width := 108*2

   Win1.Test_2.Icon := cIcon

   Win1.Test_2.FontBold := .T.

   Win1.Test_2.Redraw

RETURN Nil
