/*
 * MiniGUI Menu Demo
 */

#include "minigui.ch"

PROCEDURE Main( ... )

   LOCAL n
   LOCAL m_char

   IF PCount() == 0
      SET MENUSTYLE EXTENDED
   ELSE
      SET MENUSTYLE STANDARD
   ENDIF

   DEFINE WINDOW Win_1 ;
      WIDTH 400 ;
      HEIGHT 400 ;
      TITLE 'Menu Demo 2' ;
      MAIN

      DEFINE MAIN MENU

      POPUP "&Option"

         FOR n := 1 TO 3
            m_char := StrZero( n, 2 )
            MENUITEM Space( 4 ) + 'EXE ' + STUFF( m_char, 2, 0, "&" ) + Space( 10 ) + "Ctrl+" + Str( n, 1 ) + Space( 30 ) ;
               ACTION MenuProc() NAME &m_char
         NEXT

         SEPARATOR

         MENUITEM Space( 4 ) + 'E&xit' ACTION Win_1.Release NAME item_exit

      END POPUP

      POPUP '&Help'

         MENUITEM Space( 8 ) + '&About' + Space( 30 ) ACTION MsgInfo ( MiniGuiVersion(1) ) NAME item_about

      END POPUP

      END MENU

      ON KEY CONTROL + 1 ACTION MsgInfo ( 'Action 01', 'EXE 01' )
      ON KEY CONTROL + 2 ACTION MsgInfo ( 'Action 02', 'EXE 02' )
      ON KEY CONTROL + 3 ACTION MsgInfo ( 'Action 03', 'EXE 03' )

   END WINDOW

   IF IsExtendedMenuStyleActive()
      Set MenuTheme User GetWin7Theme() Of Win_1
   ELSE
      FOR n := 1 TO 3
         m_char := StrZero( n, 2 )
         Win_1.(m_char).Caption := AllTrim( Win_1.(m_char).Caption )
      NEXT
      Win_1.item_exit.Caption  := AllTrim( Win_1.item_exit.Caption )
      Win_1.item_about.Caption := AllTrim( Win_1.item_about.Caption )
   ENDIF

   CENTER WINDOW Win_1
   ACTIVATE WINDOW Win_1

RETURN


PROCEDURE MenuProc()

   LOCAL cTitle := Left( StrTran( LTrim( This.Caption ), "&", "" ), 6 )

   IF This.Name == '01'

      MsgInfo ( 'Action 01', cTitle )

   ELSEIF This.Name == '02'

      MsgInfo ( 'Action 02', cTitle )

   ELSEIF This.Name == '03'

      MsgInfo ( 'Action 03', cTitle )

   ENDIF

RETURN


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
