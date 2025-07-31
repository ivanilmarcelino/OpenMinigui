/*
 * Author: P.Chornyj <myorg63@mail.ru>
*/

#include "minigui.ch"

PROCEDURE Main

	DEFINE FONT font_0 FONTNAME GetDefaultFontName() SIZE 10
	DEFINE FONT font_1 FONTNAME 'Times New Roman' SIZE 10 BOLD
	DEFINE FONT font_2 FONTNAME 'Arial'   SIZE 12 ITALIC
	DEFINE FONT font_3 FONTNAME 'Verdana' SIZE 14 UNDERLINE  
	DEFINE FONT font_4 FONTNAME 'Courier' SIZE 16 STRIKEOUT

	SET MENUSTYLE EXTENDED
//	SET MENUSTYLE STANDARD

//  	SetMenuBitmapHeight( 16 ) 
//  	SetMenuBitmapHeight( BmpSize( "NEW" )[ 1 ] ) 

	DEFINE WINDOW Form_1 ;
	WIDTH 640 HEIGHT 480 ;
	TITLE "MenuEx Test" ICON "SMILE" NOTIFYICON "SMILE" ;
	MAIN

	DEFINE MAIN MENU
		POPUP "&File" FONT "font_0"
			ITEM "&New" + space(18) + "Ctrl+N"       ACTION MsgInfo( "File:New" )     IMAGE "NEW" 
			ITEM "&Open" + space(17) + "Ctrl+O"      ACTION MsgInfo( "File:Open" )    
		        ITEM "&Save" + space(17) + "Ctrl+S"      ACTION MsgInfo( "File:Save" )    IMAGE "SAVE"  
		        ITEM "Save &As.." ACTION MsgInfo( "File:Save As" ) IMAGE "SAVE_AS" 
		        SEPARATOR
			ITEM "&Print" + space(18) + "Ctrl+P"           ACTION MsgInfo( "File:Print" ) IMAGE "PRINTER" 
			ITEM "Print Pre&view"   ACTION MsgInfo( "File:Print Preview" ) 
			SEPARATOR
			ITEM "E&xit" + space(19) + "Alt+F4"  ACTION Form_1.Release IMAGE "DOOR"
		END POPUP

		POPUP "F&onts" FONT "font_0"
			ITEM "10- Bold"       FONT "font_1"
			ITEM "12- Italic"     FONT "font_2"
			ITEM "14- UnderLine"  FONT "font_3"
			ITEM "16- StrikeOut"  FONT "font_4"
		END POPUP

		POPUP "&Test" FONT "font_0"
			ITEM "Item 1" ACTION MsgInfo( Str( GetMenuItemCount( GetMenu( _HMG_MainHandle ) ) ) ) 
			ITEM "Item 2" ACTION MsgInfo( "Item 2" )

			POPUP "Item 3"
          			ITEM "Item 3.1" ACTION MsgInfo( "Item 3.1" ) 
				ITEM "Item 3.2" ACTION MsgInfo ( "Item 3.2" )

				POPUP "Item 3.3"
					ITEM "Item 3.3.1" ACTION MsgInfo ( "Item 3.3.1" )
					ITEM "Item 3.3.2" ACTION MsgInfo ( "Item 3.3.2" )

					POPUP "Item 3.3.3" 	
						ITEM "Item 3.3.3.1" ACTION MsgInfo ( "Item 3.3.3.1" ) 
						ITEM "Item 3.3.3.2" ACTION MsgInfo ( "Item 3.3.3.2" )
						ITEM "Item 3.3.3.3" ACTION MsgInfo ( "Item 3.3.3.3" )
						ITEM "Item 3.3.3.4" ACTION MsgInfo ( "Item 3.3.3.4" )
						ITEM "Item 3.3.3.5" ACTION MsgInfo ( "Item 3.3.3.5" )
						ITEM "Item 3.3.3.6" ACTION MsgInfo ( "Item 3.3.3.6" )  
					END POPUP

					ITEM "Item 3.3.4" ACTION MsgInfo ( "Item 3.3.4" )
				END POPUP
			END POPUP
			ITEM "Item 4" ACTION MsgInfo ( "Item 4" ) DISABLED
		END POPUP

		POPUP "T&est 1-2" FONT "font_0"
			ITEM "Test 1.1" ACTION Test1( "1" ) NAME Test11 CHECKED CHECKMARK "TICK"
			ITEM "Test 1.2" ACTION Test1( "2" ) NAME Test12 CHECKED CHECKMARK "TICK"
			ITEM "Test 1.3" ACTION Test1( "3" ) NAME Test13 CHECKED 
			SEPARATOR
			ITEM "Test 1.4" ACTION Test2( "4" ) NAME Test14 CHECKED CHECKMARK "SHADING"
			ITEM "Test 1.5" ACTION Test2( "5" ) NAME Test15 CHECKMARK "SHADING"
			ITEM "Test 1.6" ACTION Test2( "6" ) NAME Test16 CHECKMARK "SHADING" IMAGE "BUG"
		END POPUP

		POPUP "Te&st 3" FONT "font_0"
			ITEM "Test 2.1" NAME Test21 
			ITEM "Test 2.2" NAME Test22 
			ITEM "Test 2.3" NAME Test23 CHECKED CHECKMARK "MARK"
			SEPARATOR
			ITEM "Disable Items" ACTION Test3( _GetMenuItemCaption( "SetOnOff", "Form_1" ) <> "Disable Items" ) NAME SetOnOff
		END POPUP

		POPUP "&UI theme" FONT "font_0"
			ITEM "Default" ACTION HMG_SetMenuTheme( MNUCLR_THEME_DEFAULT ) NAME Theme0
			SEPARATOR
			ITEM "Classic" ACTION HMG_SetMenuTheme( MNUCLR_THEME_XP ) NAME Theme1
			ITEM "Office 2000 theme" ACTION HMG_SetMenuTheme( MNUCLR_THEME_2000 ) NAME Theme2
			ITEM "Dark theme" ACTION HMG_SetMenuTheme( MNUCLR_THEME_DARK ) NAME Theme3
			ITEM "User Defined" ACTION ( HMG_SetMenuTheme(), ;
				HMG_SetMenuTheme( MNUCLR_THEME_USER_DEFINED, , GetWin7Theme() ) ) NAME Theme99
		END POPUP

		AEval( Array(3), { |x, i| SetProperty( "Form_1", "Theme" + hb_ntos(i), "Enabled", IsExtendedMenuStyleActive() ) } )

		POPUP "&Misc" FONT "font_0"
			ITEM "Get MenuBitmap Height" ACTION MsgInfo ( "Current height is " + Ltrim( Str( GetMenuBitmapHeight() ) ) )
		END POPUP

		POPUP "&Help" FONT "font_0"
			ITEM "Index" IMAGE "BMPHELP"
			ITEM "Using help" 
			SEPARATOR
			ITEM "Online forum" IMAGE "WORLD"
			ITEM "Buy/register" IMAGE "CART_ADD"
			SEPARATOR
			ITEM "About" ACTION MsgInfo ( MiniGuiVersion() ) ICON "SMILE"
		END POPUP
	END MENU

	DEFINE NOTIFY MENU 
		ITEM "About..." ACTION MsgInfo( MiniGuiVersion() ) IMAGE "ABOUT"

		POPUP "Options"
	 		ITEM "Autorun" ACTION ToggleAutorun() NAME SetAuto CHECKED CHECKMARK "CHECK"
		END POPUP

		POPUP "Notify Icon"
			ITEM "Get Notify Icon Name" ACTION MsgInfo( Form_1.NotifyIcon ) 
			ITEM "Change Notify Icon"   ACTION Form_1.NotifyIcon := "Demo2.ico"
		END POPUP

		SEPARATOR

		ITEM "Exit Application" ACTION Form_1.Release IMAGE "res\cancel.bmp"
	END MENU

	DEFINE CONTEXT MENU
		POPUP "Context item 1"
			ITEM "Context item 1.1" ACTION MsgInfo( "Context item 1.1" ) ICON "SMILE"
	        ITEM "Context item 1.2" ACTION MsgInfo( "Context item 1.2" )

			POPUP 'Context item 1.3'
				ITEM "Context item 1.3.1" ACTION MsgInfo( "Context item 1.3.1" ) IMAGE "BUG"
				SEPARATOR
		        ITEM "Context item 1.3.2" ACTION MsgInfo( "Context item 1.3.2" ) CHECKED CHECKMARK "CHECK"
			END POPUP
		END POPUP

		ITEM "Context item 2 - Simple"   ACTION MsgInfo( "Context item 2 - Simple" ) CHECKED CHECKMARK "CHECK"
		ITEM "Context item 3 - Disabled" ACTION MsgInfo( "Context item 3 - Disabled" ) DISABLED
		SEPARATOR
		POPUP "Context item 4"
			ITEM "Context item 4.1" ACTION MsgInfo( "Context item 4.1" )
			ITEM "Context item 4.2" ACTION MsgInfo( "Context item 4.2" )
			ITEM "Context item 4.3" ACTION MsgInfo( "Context item 4.3" ) DISABLED
		END POPUP
	END MENU

	IF IsExtendedMenuStyleActive()
		HMG_SetMenuTheme()
	ENDIF

	END WINDOW

	CENTER   WINDOW  Form_1
	ACTIVATE WINDOW  Form_1

RETURN

/*
*/
STATIC PROCEDURE ToggleAutorun

	Form_1.SetAuto.Checked := !Form_1.SetAuto.Checked

	_SetMenuItemBitmap( "SetAuto" , "Form_1" , if( Form_1.SetAuto.Checked == .T., NIL, "UNCHECK" ) )

	MsgInfo( "Autorun is " + ;
		if( Form_1.SetAuto.Checked == .T., "enabled", "disabled") )
RETURN

/*
*/
STATIC PROCEDURE Test1( param )

LOCAL lChecked 

	lChecked := GetProperty( "Form_1", "Test1"+param , "Checked" )
	SetProperty( "Form_1", "Test1"+param , "Checked", !lChecked )

	MsgInfo( "Item Test1"+param + " is " + ;
		if( GetProperty( "Form_1", "Test1"+param , "Checked" ) == .T., ;
                	"checked", "unchecked" ) )
RETURN

/*
*/
STATIC PROCEDURE Test2( param )

	SetProperty( "Form_1", "Test1"+param , "Checked" , .T. )

	SWITCH param
	CASE "4"
		SetProperty( "Form_1", "Test15", "Checked" , .F. )		
		SetProperty( "Form_1", "Test16", "Checked" , .F. )	
		EXIT
	CASE "5"
		SetProperty( "Form_1", "Test14", "Checked" , .F. )		
		SetProperty( "Form_1", "Test16", "Checked" , .F. )		
		EXIT
	CASE "6"
		SetProperty( "Form_1", "Test14", "Checked" , .F. )		
		SetProperty( "Form_1", "Test15", "Checked" , .F. )		
	END

	PlayBeep()

RETURN

/*
*/
STATIC PROCEDURE Test3( param )

	_SetMenuItemCaption( "SetOnOff", "Form_1", iif( param == .F., "Enable Items", "Disable Items" ) )

	SetProperty( "Form_1", "Test21", "Enabled", param )
	SetProperty( "Form_1", "Test22", "Enabled", param )
	SetProperty( "Form_1", "Test23", "Enabled", param )

	MsgInfo( "Items Test21-Test23 is " + ;
		if( GetProperty( "Form_1", "Test21", "Enabled" ) == .F., ;
                	"disabled", "enabled" ) )
RETURN

/*
*/
FUNCTION GetWin7Theme()

	LOCAL aUserDefined := Array( 24 )

	aUserDefined[ MNUCLR_MENUBARBACKGROUND1 ] := GetSysColor( 15 )
	aUserDefined[ MNUCLR_MENUBARBACKGROUND2 ] := RGB( 211, 218, 237 )
	aUserDefined[ MNUCLR_MENUBARTEXT ] := RGB( 0, 0, 0 )
	aUserDefined[ MNUCLR_MENUBARSELECTEDTEXT ] := RGB( 0, 0, 0 )
	aUserDefined[ MNUCLR_MENUBARGRAYEDTEXT ] := GetSysColor( 17 )
	aUserDefined[ MNUCLR_MENUBARSELECTEDITEM1 ] := RGB( 174, 206, 246 )
	aUserDefined[ MNUCLR_MENUBARSELECTEDITEM2 ] := RGB( 174, 206, 246 )

	aUserDefined[ MNUCLR_MENUITEMTEXT ] := GetSysColor( 7 )
	aUserDefined[ MNUCLR_MENUITEMSELECTEDTEXT ] := GetSysColor( 7 )
	aUserDefined[ MNUCLR_MENUITEMGRAYEDTEXT ] := GetSysColor( 17 )
	aUserDefined[ MNUCLR_MENUITEMBACKGROUND1 ] := RGB( 240, 240, 240 )
	aUserDefined[ MNUCLR_MENUITEMBACKGROUND2 ] := RGB( 240, 240, 240 )
	aUserDefined[ MNUCLR_MENUITEMSELECTEDBACKGROUND1 ] := RGB( 232, 238, 246 )
	aUserDefined[ MNUCLR_MENUITEMSELECTEDBACKGROUND2 ] := RGB( 232, 238, 246 )
	aUserDefined[ MNUCLR_MENUITEMGRAYEDBACKGROUND1 ] := RGB( 240, 240, 240 )
	aUserDefined[ MNUCLR_MENUITEMGRAYEDBACKGROUND2 ] := RGB( 240, 240, 240 )

	aUserDefined[ MNUCLR_IMAGEBACKGROUND1 ] := RGB( 241, 241, 241 )
	aUserDefined[ MNUCLR_IMAGEBACKGROUND2 ] := RGB( 241, 241, 241 )

	aUserDefined[ MNUCLR_SEPARATOR1 ] := RGB( 224, 224, 224 )
	aUserDefined[ MNUCLR_SEPARATOR2 ] := RGB( 255, 255, 255 )

	aUserDefined[ MNUCLR_SELECTEDITEMBORDER1 ] := RGB( 174, 206, 246 )
	aUserDefined[ MNUCLR_SELECTEDITEMBORDER2 ] := RGB( 174, 206, 246 )
	aUserDefined[ MNUCLR_SELECTEDITEMBORDER3 ] := RGB( 174, 206, 246 )
	aUserDefined[ MNUCLR_SELECTEDITEMBORDER4 ] := RGB( 174, 206, 246 )

RETURN aUserDefined
