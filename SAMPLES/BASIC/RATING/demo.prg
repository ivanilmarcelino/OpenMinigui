/*
 * HMG - Harbour Win32 GUI library Demo
 *
 * Copyright 2014-2021 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

Function Main

        DEFINE WINDOW Win_1 ;
               AT 0,0 ;
               WIDTH 400 HEIGHT 340 ;
               TITLE 'Rating Test' ;
               ICON 'star.ico' ;
               MAIN ;
               FONT "Arial" SIZE 11 ;
               BACKCOLOR WHITE
 
               DEFINE MAINMENU 
                        DEFINE POPUP "File"
				MENUITEM "Exit" ONCLICK ThisWindow.Release
                        END POPUP
                END MENU

		@ 20, 20 LABEL LABEL_0 VALUE 'Please rate these super-heroes' WIDTH 300 FONT "Arial" SIZE 12 BOLD TRANSPARENT

		@ 70, 20 LABEL LABEL_1 VALUE 'Batman' TRANSPARENT

		@ 70, 200 RATING Rate_1 ;
			WIDTH 18 ;
			HEIGHT 18 ;
			STARS 5 ;
			FROM RESOURCE ;
			SPACING 15 ;
			RATE 3

		@ 120, 20 LABEL LABEL_2 VALUE 'Superman' TRANSPARENT

		@ 120, 200 RATING Rate_2 ;
			WIDTH 18 ;
			HEIGHT 18 ;
			STARS 5 ;
			FROM RESOURCE ;
			SPACING 15 ;
			RATE 2

		@ 170, 20 LABEL LABEL_3 VALUE 'Spiderman' TRANSPARENT

		@ 170, 200 RATING Rate_3 ;
			WIDTH 18 ;
			HEIGHT 18 ;
			STARS 5 ;
			FROM RESOURCE ;
			SPACING 15 ;
			RATE 4

		@ 220, 20 LABEL LABEL_4 VALUE 'Captain Marvel' WIDTH 130 TRANSPARENT

		@ 220, 200 RATING Rate_4 ;
			WIDTH 18 ;
			HEIGHT 18 ;
			STARS 5 ;
			FROM RESOURCE ;
			SPACING 15 ;
			RATE 4

        END WINDOW

        Win_1.Center
        ACTIVATE WINDOW Win_1

Return Nil

