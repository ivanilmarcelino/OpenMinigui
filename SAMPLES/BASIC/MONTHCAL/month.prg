/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-05 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
*/

#include 'minigui.ch'

#xcommand ON KEY SPACE [ OF <parent> ] ACTION <action> ;
=> ;
_DefineHotKey ( <"parent"> , 0 , VK_SPACE , <{action}> )

FUNCTION Main()

    DEFINE WINDOW Win_1 ; 
        AT 0,0 ; 
        WIDTH 500 ; 
        HEIGHT 500 ; 
        TITLE 'MonthCalendar Control Demo' ; 
	ICON "DEMO.ICO" ;
        MAIN ; 
        NOSIZE ;
	BACKCOLOR { 132, 195, 248 }

	DEFINE MAIN MENU
		DEFINE POPUP 'Test'
			MENUITEM 'Set Row' ACTION Win_1.Control_1.Row := Val(InputBox('Enter Row',''))
			MENUITEM 'Set Col' ACTION Win_1.Control_1.Col := Val(InputBox('Enter Col',''))
			MENUITEM 'Set Width' ACTION Win_1.Control_1.Width := Val(InputBox('Enter Width',''))
			MENUITEM 'Set Height' ACTION Win_1.Control_1.Height := Val(InputBox('Enter Height',''))
			SEPARATOR
			MENUITEM 'Get Row' ACTION MsgInfo ( Win_1.Control_1.Row, 'Row' )
			MENUITEM 'Get Col' ACTION MsgInfo ( Win_1.Control_1.Col, 'Col' )
			MENUITEM 'Get Width' ACTION MsgInfo ( Win_1.Control_1.Width, 'Width' )
			MENUITEM 'Get Height' ACTION MsgInfo ( Win_1.Control_1.Height, 'Height' )
		END POPUP
	END MENU


        DEFINE MONTHCALENDAR CONTROL_1
		ROW	10
		COL	10
		TOOLTIP 'MonthCalendar Control' 
		FONTNAME 'Arial'
		FONTSIZE 8
		BACKCOLOR { 204, 204, 224 }
		FONTCOLOR BLUE
		TITLEBACKCOLOR { 036, 067, 009 }
		TITLEFONTCOLOR { 235, 241, 053 }
		TRAILINGFONTCOLOR GREEN
		BKGNDCOLOR { 204, 204, 224 }
	END MONTHCALENDAR


	@ 10,300 BUTTON Button_1 ;
		CAPTION "IS BOLD DATE?" ;
		ACTION MsgInfo( IsMonthCalBoldDay( 'Control_1', 'Win_1', Win_1.Control_1.Value ), 'Is Bold?' )

	@ 50,300 BUTTON Button_2 ;
		CAPTION "SET BOLD DATE" ;
		ACTION AddMonthCalBoldDay( 'Control_1', 'Win_1', Win_1.Control_1.Value )

	@ 90,300 BUTTON Button_3 ;
		CAPTION "DELETE BOLD DATE" ;
		WIDTH 160 ;
		ACTION DelMonthCalBoldDay( 'Control_1', 'Win_1', Win_1.Control_1.Value )

	ON KEY LEFT		ACTION Win_1.Control_1.Value := Win_1.Control_1.Value - 1
	ON KEY RIGHT		ACTION Win_1.Control_1.Value := Win_1.Control_1.Value + 1
	ON KEY UP		ACTION Win_1.Control_1.Value := Win_1.Control_1.Value - 7
	ON KEY DOWN		ACTION Win_1.Control_1.Value := Win_1.Control_1.Value + 7

	ON KEY SPACE		ACTION Win_1.Control_1.Value := Date()

	ON KEY CONTROL+LEFT	ACTION Win_1.Control_1.Value := Win_1.Control_1.Value - 30
	ON KEY CONTROL+RIGHT	ACTION Win_1.Control_1.Value := Win_1.Control_1.Value + 30

    END WINDOW

    Win_1.Control_1.FONTNAME := _GetSysFont()
    Win_1.Control_1.FONTSIZE := 12

    SETBOLDSUNDAY( Win_1.Control_1.Value )

    CENTER WINDOW Win_1
    ACTIVATE WINDOW Win_1

RETURN NIL


STATIC PROCEDURE SETBOLDSUNDAY( dFecha )

   LOCAL dBoM, dStart
   LOCAL nWeek, nDay

   dBoM := dFecha - Day( dFecha ) + 1
   dStart := If( DoW( dBoM ) != 1, dBoM - DoW( dBoM ) + 2, dBoM - 6 )

   FOR nWeek := 1 TO 6
      FOR nDay := 1 TO 7
         IF nDay == 7 .AND. Month( dStart ) == Month( dFecha ) .AND. dStart != dFecha
            AddMonthCalBoldDay( 'Control_1', 'Win_1', dStart )
         ENDIF
         dStart++
      NEXT
   NEXT

RETURN
