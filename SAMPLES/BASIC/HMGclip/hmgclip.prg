/*

      hmgclip.prg
      An example of integrating HMG GUI controls in Harbour.

 */

#include "minigui.ch"

SET PROCEDURE TO empdet


FUNCTION Main()

   SET CENTURY ON

   DEFINE WINDOW Main ;
      CLIENTAREA 800, 642 ;
      TITLE "Visual Clipper via HMG" ;
      ICON "IDI_MAIN" ;
      MAIN ;
      NOMAXIMIZE ;
      NOSIZE ;
      ON INIT BuildMainMenu( This.Name )

      DEFINE STATUSBAR
         STATUSITEM ""
      END STATUSBAR

      ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   Main.Center()
   Main.Activate()

RETURN NIL


PROCEDURE BuildMainMenu( cMain )

	SET MENUSTYLE EXTENDED

  	SetMenuBitmapHeight( BmpSize( "IDI_MENU1" )[ 1 ] ) 

	SetTheme()

	DEFINE MAIN MENU OF (cMain)
		DEFINE POPUP "&File"
			MENUITEM "&Employee Details" ACTION empdet() IMAGE "IDI_MENU2"
			MENUITEM "&Radio Buttons" ACTION RadioButton() IMAGE "IDI_MENU2"
			SEPARATOR
			MENUITEM "E&xit" ACTION ReleaseAllWindows() IMAGE "IDI_MENU1"
		END POPUP
		DEFINE POPUP "&Help"
			MENUITEM "Check for Updates" ACTION NIL DISABLED
			MENUITEM "About" ACTION MsgInfo( "An example of integrating HMG GUI controls in Harbour" )
		END POPUP
	END MENU

RETURN


STATIC PROCEDURE SetTheme()

	LOCAL aColors := GetMenuColors()

	aColors[ MNUCLR_MENUBARBACKGROUND1 ]  := GetSysColor( 15 )
	aColors[ MNUCLR_MENUBARBACKGROUND2 ]  := GetSysColor( 15 )
	aColors[ MNUCLR_MENUBARTEXT ]         := GetSysColor(  7 )
	aColors[ MNUCLR_MENUBARSELECTEDTEXT ] := GetSysColor( 14 )
	aColors[ MNUCLR_MENUBARGRAYEDTEXT ]   := GetSysColor( 17 )
	aColors[ MNUCLR_MENUBARSELECTEDITEM1 ]:= GetSysColor( 13 )
	aColors[ MNUCLR_MENUBARSELECTEDITEM2 ]:= GetSysColor( 13 )

	aColors[ MNUCLR_MENUITEMTEXT ]        := GetSysColor(  7 )  
	aColors[ MNUCLR_MENUITEMSELECTEDTEXT ]:= GetSysColor( 14 )  
	aColors[ MNUCLR_MENUITEMGRAYEDTEXT ]  := GetSysColor( 17 )   

	aColors[ MNUCLR_MENUITEMBACKGROUND1 ] := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )
	aColors[ MNUCLR_MENUITEMBACKGROUND2 ] := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )

	aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND1 ]   := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )
	aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND2 ]   := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )

	aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND1 ] := GetSysColor( 13 )
	aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND2 ] := GetSysColor( 13 )

	aColors[ MNUCLR_IMAGEBACKGROUND1 ] := RGB( 192, 216, 248 )
	aColors[ MNUCLR_IMAGEBACKGROUND2 ] := RGB( 128, 168, 220 )

	aColors[ MNUCLR_SEPARATOR1 ] := GetSysColor( 17 )
	aColors[ MNUCLR_SEPARATOR2 ] := GetSysColor( 14 )

	aColors[ MNUCLR_SELECTEDITEMBORDER1 ] := GetSysColor( 13 ) 
	aColors[ MNUCLR_SELECTEDITEMBORDER2 ] := GetSysColor( 13 )
	aColors[ MNUCLR_SELECTEDITEMBORDER3 ] := GetSysColor( 17 )
	aColors[ MNUCLR_SELECTEDITEMBORDER4 ] := GetSysColor( 14 )

	SET MENUCURSOR FULL

	SET MENUSEPARATOR DOUBLE RIGHTALIGN

	SET MENUITEM BORDER FLAT

	SetMenuColors( aColors )

RETURN

// EOF: HMGCLIP.PRG
